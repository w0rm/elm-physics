module Collision.ConvexConvex exposing
    ( addContacts
    , bestFace
    , faceExtent
    , findSeparatingAxis
    , project
    , projectConvex
    )

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Manifold as Manifold
import Internal.Transform3d as Transform3d exposing (Orientation3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.VertexBuffer as VertexBuffer exposing (VertexBuffer)
import Shapes.Convex as Convex exposing (Convex, Edge, EdgeGroup, Face, FaceGroup(..), Obb(..))


{-| Which body contributed the winning face axis to SAT.
-}
type Side
    = Convex1
    | Convex2


{-| `findFaceSAT` winner. Carries the group so the dispatcher skips one
`bestFace` walk; `groupIdx` is 1-based flat order, matching `bestFace`,
so contact ids stay stable.
-}
type alias FaceWinner =
    { axis : Vec3
    , dmin : Float
    , fromSide : Side
    , groupIdx : Int
    , group : FaceGroup
    }


addContacts : Int -> Convex -> Convex -> List Contact -> List Contact
addContacts shapeKey convex1 convex2 contacts =
    case findFaceSAT convex1 convex2 of
        Nothing ->
            contacts

        Just winner ->
            case findEdgeSAT convex1 convex2 winner.dmin of
                EdgeSeparates _ _ _ _ _ _ _ ->
                    contacts

                EdgeBeats _ axis featureKey edge1 edge2 _ _ ->
                    addEdgeContact shapeKey
                        axis
                        featureKey
                        edge1
                        convex1.vertexBuffer
                        edge2
                        convex2.vertexBuffer
                        contacts

                NoEdgeBeats _ _ _ _ _ _ _ ->
                    dispatchBestFaces shapeKey convex1 convex2 winner contacts


{-| Pick the contact face on the SAT-winning body directly; only run
`bestFace` against the other body.
-}
dispatchBestFaces : Int -> Convex -> Convex -> FaceWinner -> List Contact -> List Contact
dispatchBestFaces shapeKey convex1 convex2 winner contacts =
    let
        separatingAxis =
            orientAxis convex1 convex2 winner.axis

        reversedSeparatingAxis =
            Vec3.negate separatingAxis

        picked =
            case winner.fromSide of
                Convex1 ->
                    let
                        ( wid, wface ) =
                            pickWinningFace winner.groupIdx winner.group separatingAxis

                        ( oid, oface ) =
                            bestFace convex2.faces reversedSeparatingAxis
                    in
                    { id1 = wid, face1 = wface, id2 = oid, face2 = oface }

                Convex2 ->
                    let
                        ( oid, oface ) =
                            bestFace convex1.faces separatingAxis

                        ( wid, wface ) =
                            pickWinningFace winner.groupIdx winner.group reversedSeparatingAxis
                    in
                    { id1 = oid, face1 = oface, id2 = wid, face2 = wface }
    in
    if picked.id1 == -1 || picked.id2 == -1 then
        contacts

    else
        -- face1/face2 stay with convex1/convex2, whichever side won
        clipTwoFaces shapeKey
            picked.id1
            picked.id2
            picked.face1
            convex1.vertexBuffer
            picked.face2
            convex2.vertexBuffer
            reversedSeparatingAxis
            contacts


{-| The face in the group most anti-aligned with `axisToward`; one dot
decides, the partner's is its negation.
-}
pickWinningFace : Int -> FaceGroup -> Vec3 -> ( Int, Face )
pickWinningFace groupIdx group axisToward =
    case group of
        TwoSidedFace n1 i1 _ n2 i2 _ ->
            if Vec3.dot n1 axisToward <= 0 then
                ( groupIdx, { normal = n1, vertices = i1 } )

            else
                ( groupIdx + 1, { normal = n2, vertices = i2 } )

        OneSidedFace n1 i1 _ _ _ _ ->
            ( groupIdx, { normal = n1, vertices = i1 } )


orientAxis : Convex -> Convex -> Vec3 -> Vec3
orientAxis convex1 convex2 axis =
    if Vec3.dot (Vec3.sub convex2.position convex1.position) axis > 0 then
        Vec3.negate axis

    else
        axis


{-| Emit a single edge-edge contact; `axis` is already the contact normal.
The id packs `(dir1Idx, edge1Idx, dir2Idx, edge2Idx)`, stable across
`placeIn`, so warm-start keys survive multi-edge contacts in a body pair.
-}
addEdgeContact : Int -> Vec3 -> Int -> Edge -> VertexBuffer -> Edge -> VertexBuffer -> List Contact -> List Contact
addEdgeContact shapeKey axis featureKey edge1 buffer1 edge2 buffer2 contacts =
    let
        ( pi, pj ) =
            Vec3.closestPointsBetweenSegments
                (VertexBuffer.get edge1.i1 buffer1)
                (VertexBuffer.get edge1.i2 buffer1)
                (VertexBuffer.get edge2.i1 buffer2)
                (VertexBuffer.get edge2.i2 buffer2)
    in
    { shapeKey = shapeKey
    , featureKey = featureKey
    , ni = axis
    , pi = pi
    , pj = pj
    }
        :: contacts


clipTwoFaces : Int -> Int -> Int -> Face -> VertexBuffer -> Face -> VertexBuffer -> Vec3 -> List Contact -> List Contact
clipTwoFaces shapeKey faceId1 faceId2 face faceBuffer incidentFace incidentBuffer separatingAxis contacts =
    let
        -- only the two contacting faces are materialised
        referenceVertices =
            Convex.faceVertices faceBuffer face

        -- vertices carry buffer indices as warm-start keys through the clip
        incidentPolygon =
            Convex.indexedFaceVertices incidentBuffer incidentFace

        point =
            case referenceVertices of
                first :: _ ->
                    first

                [] ->
                    Vec3.zero

        facePlaneConstant =
            -(Vec3.dot face.normal point)
    in
    emitManifold shapeKey
        faceId1
        faceId2
        separatingAxis
        face.normal
        facePlaneConstant
        (Manifold.reduce face.normal
            facePlaneConstant
            (clipAgainstAdjacentFaces face.normal referenceVertices incidentPolygon)
        )
        contacts


emitManifold : Int -> Int -> Int -> Vec3 -> Vec3 -> Float -> List ( Int, Vec3 ) -> List Contact -> List Contact
emitManifold shapeKey faceId1 faceId2 separatingAxis normal planeConstant points result =
    case points of
        ( vertexId, vertex ) :: rest ->
            let
                depth =
                    Vec3.dot normal vertex + planeConstant
            in
            emitManifold shapeKey
                faceId1
                faceId2
                separatingAxis
                normal
                planeConstant
                rest
                ({ shapeKey = shapeKey
                 , featureKey = ContactId.convexConvexFace faceId1 faceId2 vertexId
                 , ni = separatingAxis
                 , pi =
                    { x = vertex.x - depth * normal.x
                    , y = vertex.y - depth * normal.y
                    , z = vertex.z - depth * normal.z
                    }
                 , pj = vertex
                 }
                    :: result
                )

        [] ->
            result


{-| The face most aligned with `-separatingAxis`; one dot per group covers
the antiparallel partner. `( -1, emptyFace )` when there are no groups.
-}
bestFace : List FaceGroup -> Vec3 -> ( Int, Face )
bestFace groups separatingAxis =
    bestFaceWalk separatingAxis groups 1 -1 emptyFace Const.maxNumber


emptyFace : Face
emptyFace =
    { vertices = [], normal = Vec3.zero }


bestFaceWalk : Vec3 -> List FaceGroup -> Int -> Int -> Face -> Float -> ( Int, Face )
bestFaceWalk separatingAxis groups faceId currentBestFaceId currentBestFace currentBestDistance =
    case groups of
        [] ->
            ( currentBestFaceId, currentBestFace )

        (TwoSidedFace n1 i1 _ n2 i2 _) :: restGroups ->
            let
                primaryDot =
                    Vec3.dot n1 separatingAxis

                partnerDot =
                    -primaryDot

                -- Compete primary against running best.
                ( id1, f1, d1 ) =
                    if currentBestDistance - primaryDot > 0 then
                        ( faceId, { normal = n1, vertices = i1 }, primaryDot )

                    else
                        ( currentBestFaceId, currentBestFace, currentBestDistance )
            in
            -- Compete partner against the result.
            if d1 - partnerDot > 0 then
                bestFaceWalk separatingAxis restGroups (faceId + 2) (faceId + 1) { normal = n2, vertices = i2 } partnerDot

            else
                bestFaceWalk separatingAxis restGroups (faceId + 2) id1 f1 d1

        (OneSidedFace n1 i1 _ _ _ _) :: restGroups ->
            let
                d =
                    Vec3.dot n1 separatingAxis
            in
            if currentBestDistance - d > 0 then
                bestFaceWalk separatingAxis restGroups (faceId + 1) faceId { normal = n1, vertices = i1 } d

            else
                bestFaceWalk separatingAxis restGroups (faceId + 1) currentBestFaceId currentBestFace currentBestDistance


clipAgainstAdjacentFaces : Vec3 -> List Vec3 -> List ( Int, Vec3 ) -> List ( Int, Vec3 )
clipAgainstAdjacentFaces normal referenceVertices incidentPolygon =
    Convex.foldFaceEdges
        (\v1 v2 ->
            let
                edge =
                    Vec3.normalize (Vec3.sub v1 v2)

                planeNormal =
                    Vec3.cross normal edge

                planeConstant =
                    -(Vec3.dot v1 planeNormal)
            in
            Convex.foldFaceEdges
                (clipFaceAgainstPlaneAdd planeNormal planeConstant)
                []
        )
        incidentPolygon
        referenceVertices


clipFaceAgainstPlaneAdd : Vec3 -> Float -> ( Int, Vec3 ) -> ( Int, Vec3 ) -> List ( Int, Vec3 ) -> List ( Int, Vec3 )
clipFaceAgainstPlaneAdd planeNormal planeConstant prev next result =
    let
        ( _, prevP ) =
            prev

        ( _, nextP ) =
            next

        nDotPrev =
            Vec3.dot planeNormal prevP + planeConstant

        nDotNext =
            Vec3.dot planeNormal nextP + planeConstant
    in
    if nDotPrev < 0 then
        if nDotNext < 0 then
            next :: result

        else
            crossing nDotPrev nDotNext prev next :: result

    else if nDotNext < 0 then
        next
            :: crossing nDotPrev nDotNext prev next
            :: result

    else
        result


{-| Where incident edge `prev→next` crosses the clip plane, keyed to its nearer
endpoint (by the lerp parameter) for a stable warm-start vertex.
-}
crossing : Float -> Float -> ( Int, Vec3 ) -> ( Int, Vec3 ) -> ( Int, Vec3 )
crossing nDotPrev nDotNext ( prevId, prevP ) ( nextId, nextP ) =
    let
        t =
            nDotPrev / (nDotPrev - nDotNext)
    in
    ( if t < 0.5 then
        prevId

      else
        nextId
    , Vec3.lerp t prevP nextP
    )


findSeparatingAxis : Convex -> Convex -> Maybe Vec3
findSeparatingAxis convex1 convex2 =
    case findFaceSAT convex1 convex2 of
        Nothing ->
            Nothing

        Just winner ->
            case findEdgeSAT convex1 convex2 winner.dmin of
                EdgeSeparates _ _ _ _ _ _ _ ->
                    Nothing

                EdgeBeats _ axis _ _ _ _ _ ->
                    -- axis points 1 → 2; match the face path's convention
                    Just (Vec3.negate axis)

                NoEdgeBeats _ _ _ _ _ _ _ ->
                    Just (orientAxis convex1 convex2 winner.axis)


{-| Test every face group's direction as a SAT axis; return the winning
side + group so the dispatcher skips one `bestFace` walk.
-}
findFaceSAT : Convex -> Convex -> Maybe FaceWinner
findFaceSAT convex1 convex2 =
    findFaceSATHelp convex1
        convex2
        Convex1
        convex1.faces
        convex2.faces
        1
        -1
        Convex1
        emptyGroup
        Const.maxNumber


emptyGroup : FaceGroup
emptyGroup =
    OneSidedFace Vec3.zero [] 0 () () ()


findFaceSATHelp : Convex -> Convex -> Side -> List FaceGroup -> List FaceGroup -> Int -> Int -> Side -> FaceGroup -> Float -> Maybe FaceWinner
findFaceSATHelp convex1 convex2 currentSide normals nextNormals nextGroupIdx winnerIdx winnerSide winnerGroup dmin =
    case normals of
        [] ->
            case nextNormals of
                [] ->
                    if winnerIdx == -1 then
                        Nothing

                    else
                        Just
                            { axis = Convex.faceGroupNormal winnerGroup
                            , dmin = dmin
                            , fromSide = winnerSide
                            , groupIdx = winnerIdx
                            , group = winnerGroup
                            }

                _ ->
                    findFaceSATHelp convex1 convex2 Convex2 nextNormals [] 1 winnerIdx winnerSide winnerGroup dmin

        group :: restNormals ->
            case testFaceSeparatingAxis convex1 convex2 currentSide group of
                Nothing ->
                    Nothing

                Just dist ->
                    let
                        groupSize =
                            case group of
                                TwoSidedFace _ _ _ _ _ _ ->
                                    2

                                OneSidedFace _ _ _ _ _ _ ->
                                    1
                    in
                    if dist - dmin < 0 then
                        findFaceSATHelp convex1 convex2 currentSide restNormals nextNormals (nextGroupIdx + groupSize) nextGroupIdx currentSide group dist

                    else
                        findFaceSATHelp convex1 convex2 currentSide restNormals nextNormals (nextGroupIdx + groupSize) winnerIdx winnerSide winnerGroup dmin


{-| Seven fields on every variant for one monomorphic object shape; the
padding slots hold the running `dmin` so the loop threads no tuple.
`EdgeBeats dist axis featureKey edge1 edge2`: convex1's outward support
axis, the two support edges, and the packed warm-start contact id.
-}
type EdgeResult
    = EdgeSeparates Float () () () () () ()
    | EdgeBeats Float Vec3 Int Edge Edge () ()
    | NoEdgeBeats Float () () () () () ()


edgeSeparates : EdgeResult
edgeSeparates =
    EdgeSeparates 0 () () () () () ()


edgeDmin : EdgeResult -> Float
edgeDmin best =
    case best of
        EdgeBeats d _ _ _ _ _ _ ->
            d

        NoEdgeBeats d _ _ _ _ _ _ ->
            d

        EdgeSeparates d _ _ _ _ _ _ ->
            d


{-| Edge SAT must beat face SAT by 5% to take the edge-edge path; relative so it
scales with size.
-}
edgeBiasFactor : Float
edgeBiasFactor =
    1.05


{-| Iterate `(dir1, dir2)` pairs of unique edge directions. A pair matters
only when arcs of each group cross the other's great circle in consistent
hemispheres — the crossing point is the axis. A miss (incl. parallel
directions: coincident circles) prunes with no cross product, normalize or
projection. Direction indices are 1-based, stable under `placeIn`.
-}
findEdgeSAT : Convex -> Convex -> Float -> EdgeResult
findEdgeSAT convex1 convex2 faceDmin =
    -- threshold pre-biased so the loop compares plain `dist < dmin`
    findEdgeSATOuter convex1
        convex2
        (Transform3d.relativeOrientation convex1.orientation convex2.orientation)
        convex1.uniqueEdges
        1
        (NoEdgeBeats (faceDmin / edgeBiasFactor) () () () () () ())


findEdgeSATOuter : Convex -> Convex -> Orientation3d -> List EdgeGroup -> Int -> EdgeResult -> EdgeResult
findEdgeSATOuter convex1 convex2 relative groups1 dir1Idx best =
    case groups1 of
        group1 :: rest1 ->
            -- group1's direction in convex2's frame, fixed across the inner loop
            case findEdgeSATInner convex1 convex2 relative group1 (Transform3d.derotate relative group1.dir) dir1Idx convex2.uniqueEdges 1 best of
                (EdgeSeparates _ _ _ _ _ _ _) as separated ->
                    separated

                newBest ->
                    findEdgeSATOuter convex1 convex2 relative rest1 (dir1Idx + 1) newBest

        [] ->
            best


findEdgeSATInner : Convex -> Convex -> Orientation3d -> EdgeGroup -> Vec3 -> Int -> List EdgeGroup -> Int -> EdgeResult -> EdgeResult
findEdgeSATInner convex1 convex2 relative group1 x2 dir1Idx groups2 dir2Idx best =
    case groups2 of
        group2 :: rest2 ->
            let
                -- group2's direction in convex1's frame
                x1 =
                    Transform3d.rotate relative group2.dir

                cosd =
                    Vec3.dot x1 group1.dir
            in
            -- near-parallel: the crossing is ill-conditioned and the face
            -- phase covers these
            if 1 - cosd * cosd - Const.parallelTolerance < 0 then
                findEdgeSATInner convex1 convex2 relative group1 x2 dir1Idx rest2 (dir2Idx + 1) best

            else
                let
                    hits1 =
                        scanCrossings x1 group1.edges
                in
                if hits1.posIdx == 0 && hits1.negIdx == 0 then
                    findEdgeSATInner convex1 convex2 relative group1 x2 dir1Idx rest2 (dir2Idx + 1) best

                else
                    let
                        hits2 =
                            scanCrossings x2 group2.edges
                    in
                    -- candidates pair by hemisphere class (sign of the nA dot)
                    if (hits1.posIdx == 0 || hits2.posIdx == 0) && (hits1.negIdx == 0 || hits2.negIdx == 0) then
                        findEdgeSATInner convex1 convex2 relative group1 x2 dir1Idx rest2 (dir2Idx + 1) best

                    else
                        let
                            afterPos =
                                if hits1.posIdx > 0 && hits2.posIdx > 0 then
                                    addCandidate convex1 convex2 x2 (ContactId.convexConvexEdge dir1Idx hits1.posIdx dir2Idx hits2.posIdx) hits1.pos hits2.pos best

                                else
                                    best
                        in
                        case afterPos of
                            (EdgeSeparates _ _ _ _ _ _ _) as separated ->
                                separated

                            best1 ->
                                if hits1.negIdx > 0 && hits2.negIdx > 0 then
                                    case addCandidate convex1 convex2 x2 (ContactId.convexConvexEdge dir1Idx hits1.negIdx dir2Idx hits2.negIdx) hits1.neg hits2.neg best1 of
                                        (EdgeSeparates _ _ _ _ _ _ _) as separated ->
                                            separated

                                        best2 ->
                                            findEdgeSATInner convex1 convex2 relative group1 x2 dir1Idx rest2 (dir2Idx + 1) best2

                                else
                                    findEdgeSATInner convex1 convex2 relative group1 x2 dir1Idx rest2 (dir2Idx + 1) best1

        [] ->
            best


{-| Fold one edge-pair candidate into the running best. The axis is the arc
crossing point — a blend of convex2's outward normals, so no centre check;
a support edge's endpoints project equally, so one endpoint each gives the
exact depth. `EdgeSeparates` is a separating-axis certificate.
-}
addCandidate : Convex -> Convex -> Vec3 -> Int -> Edge -> Edge -> EdgeResult -> EdgeResult
addCandidate convex1 convex2 x2 featureKey edge1 edge2 best =
    let
        a =
            x2.x * edge2.nA.x + x2.y * edge2.nA.y + x2.z * edge2.nA.z

        b =
            x2.x * edge2.nB.x + x2.y * edge2.nB.y + x2.z * edge2.nB.z

        axisOut =
            -- stored pointing convex1 → convex2, the face path's convention
            Vec3.negate
                (Vec3.normalize
                    (Transform3d.rotate convex2.orientation
                        (Vec3.lerp (a / (a - b)) edge2.nA edge2.nB)
                    )
                )

        w1 =
            VertexBuffer.get edge1.i1 convex1.vertexBuffer

        w2 =
            VertexBuffer.get edge2.i1 convex2.vertexBuffer

        dist =
            axisOut.x * (w1.x - w2.x) + axisOut.y * (w1.y - w2.y) + axisOut.z * (w1.z - w2.z)
    in
    if dist + Const.contactBreakingThreshold < 0 then
        edgeSeparates

    else if dist - edgeDmin best < 0 then
        EdgeBeats dist axisOut featureKey edge1 edge2 () ()

    else
        best


{-| Which edges of the group have arcs straddling the great circle ⊥ `x`
(the other group's direction in this body's frame). At most one hit per
hemisphere class (sign of the `nA` dot); the dots ride along for the axis
lerp. Indices are 1-based; 0 means no hit.
-}
scanCrossings : Vec3 -> List Edge -> Crossings
scanCrossings x edges =
    scanCrossingsHelp x edges 1 0 emptyEdge 0 emptyEdge


type alias Crossings =
    { posIdx : Int, pos : Edge, negIdx : Int, neg : Edge }


emptyEdge : Edge
emptyEdge =
    { i1 = -1, i2 = -1, nA = Vec3.zero, nB = Vec3.zero }


scanCrossingsHelp : Vec3 -> List Edge -> Int -> Int -> Edge -> Int -> Edge -> Crossings
scanCrossingsHelp x edges idx posIdx pos negIdx neg =
    case edges of
        edge :: rest ->
            let
                a =
                    x.x * edge.nA.x + x.y * edge.nA.y + x.z * edge.nA.z

                b =
                    x.x * edge.nB.x + x.y * edge.nB.y + x.z * edge.nB.z
            in
            -- straddle, with a noise floor: both dots ≈ 0 makes the sign
            -- test garbage — treat as no crossing
            if a * b < 0 && (a * a - parallelSquaredTolerance > 0 || b * b - parallelSquaredTolerance > 0) then
                if a > 0 then
                    scanCrossingsHelp x rest (idx + 1) idx edge negIdx neg

                else
                    scanCrossingsHelp x rest (idx + 1) posIdx pos idx edge

            else
                scanCrossingsHelp x rest (idx + 1) posIdx pos negIdx neg

        [] ->
            { posIdx = posIdx, pos = pos, negIdx = negIdx, neg = neg }


parallelSquaredTolerance : Float
parallelSquaredTolerance =
    Const.precision * Const.precision


{-| A convex's [min,max] projection onto `axis`. A box projects in O(1) from its
axes + half-extents (`dot(axis, centre) ± Σ|axis·axisᵢ|·heᵢ`, no vertex scan); a
general hull scans its placed vertex list.
-}
projectConvex : Vec3 -> Convex -> { min : Float, max : Float }
projectConvex axis convex =
    case convex.obb of
        Box ax ay az he ->
            let
                c =
                    Vec3.dot axis convex.position

                e =
                    abs (Vec3.dot axis ax) * he.x + abs (Vec3.dot axis ay) * he.y + abs (Vec3.dot axis az) * he.z
            in
            { min = c - e, max = c + e }

        NotBox vs _ _ _ ->
            project axis Const.maxNumber -Const.maxNumber vs


{-| SAT for a face-normal axis: the owning convex's extent is the cached
`faceDist`/`partnerDist` plus one `dot(axis, position)`; the other side
projects via `projectConvex`. Differs from a vertex scan in the last FP
bit — enough to flip a borderline face/edge tie.
-}
testFaceSeparatingAxis : Convex -> Convex -> Side -> FaceGroup -> Maybe Float
testFaceSeparatingAxis convex1 convex2 owningSide group =
    let
        axis =
            Convex.faceGroupNormal group
    in
    case owningSide of
        Convex1 ->
            overlap
                (faceExtent axis group convex1)
                (projectConvex axis convex2)

        Convex2 ->
            overlap
                (projectConvex axis convex1)
                (faceExtent axis group convex2)


{-| Penetration depth of two projection ranges, or `Nothing` if they separate past
the margin.
-}
overlap : { min : Float, max : Float } -> { min : Float, max : Float } -> Maybe Float
overlap p1 p2 =
    let
        d1 =
            p1.max - p2.min

        d2 =
            p2.max - p1.min
    in
    if d1 + Const.contactBreakingThreshold < 0 || d2 + Const.contactBreakingThreshold < 0 then
        Nothing

    else if d1 - d2 > 0 then
        Just d2

    else
        Just d1


{-| The owning convex's [min,max] along its own face normal, from the cached
plane distances + one `dot(axis, placedCentroid)`. `OneSidedFace` has no
antipodal partner, so fall back to a full scan.
-}
faceExtent : Vec3 -> FaceGroup -> Convex -> { min : Float, max : Float }
faceExtent axis group convex =
    case group of
        TwoSidedFace _ _ faceDist _ _ partnerDist ->
            let
                posDot =
                    Vec3.dot axis convex.position
            in
            { max = faceDist + posDot, min = partnerDist + posDot }

        OneSidedFace _ _ _ _ _ _ ->
            project axis Const.maxNumber -Const.maxNumber (Convex.convexVertices convex)


{-| Get max and min dot product of a convex hull at ShapeWorldTransform3d projected onto an axis.
-}
project : Vec3 -> Float -> Float -> List Vec3 -> { min : Float, max : Float }
project localAxis minVal maxVal currentVertices =
    case currentVertices of
        [] ->
            { min = minVal, max = maxVal }

        vec :: remainingVertices ->
            let
                val =
                    vec.x * localAxis.x + vec.y * localAxis.y + vec.z * localAxis.z
            in
            project
                localAxis
                (if minVal - val > 0 then
                    val

                 else
                    minVal
                )
                (if maxVal - val > 0 then
                    maxVal

                 else
                    val
                )
                remainingVertices
