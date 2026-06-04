module Collision.ConvexConvex exposing
    ( addContacts
    , bestFace
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.VertexBuffer as VertexBuffer exposing (VertexBuffer)
import Shapes.Convex as Convex exposing (Convex, Face, FaceGroup(..))


{-| Which body contributed the winning face axis to SAT.
-}
type Side
    = Convex1
    | Convex2


{-| Result of `findFaceSAT`. Carries the winning face's group so the
dispatcher can skip one of the two `bestFace` walks. `groupIdx` is 1-based
in flat traversal order, matching `bestFace` — keeps contact IDs stable
for warm-start cache keys.
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
                EdgeSeparates _ _ _ _ _ ->
                    contacts

                EdgeBeats edgeAxis dir1Idx edges1 dir2Idx edges2 ->
                    addEdgeContact shapeKey
                        (orientAxis convex1 convex2 edgeAxis)
                        dir1Idx
                        edges1
                        convex1.vertexBuffer
                        dir2Idx
                        edges2
                        convex2.vertexBuffer
                        contacts

                NoEdgeBeats _ _ _ _ _ ->
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
        -- face1 is always convex1's, face2 always convex2's (see the case
        -- above), so each materialises against its own convex's buffer.
        clipTwoFaces shapeKey
            picked.id1
            picked.id2
            picked.face1
            convex1.vertexBuffer
            picked.face2
            convex2.vertexBuffer
            reversedSeparatingAxis
            contacts


{-| Pick the face in the group whose normal is most anti-aligned with
`axisToward`. The partner's dot is the negation of the primary's, so a
single dot product decides by sign.
-}
pickWinningFace : Int -> FaceGroup -> Vec3 -> ( Int, Face )
pickWinningFace groupIdx group axisToward =
    case group of
        TwoSidedFace n1 i1 n2 i2 ->
            if Vec3.dot n1 axisToward <= 0 then
                ( groupIdx, { normal = n1, vertices = i1 } )

            else
                ( groupIdx + 1, { normal = n2, vertices = i2 } )

        OneSidedFace n1 i1 _ _ ->
            ( groupIdx, { normal = n1, vertices = i1 } )


orientAxis : Convex -> Convex -> Vec3 -> Vec3
orientAxis convex1 convex2 axis =
    if Vec3.dot (Vec3.sub convex2.position convex1.position) axis > 0 then
        Vec3.negate axis

    else
        axis


{-| Emit a single edge-edge contact. The id encodes
`(dir1Idx, edge1Idx, dir2Idx, edge2Idx)` — stable across `placeIn`, so
warm-start cache keys survive multi-edge contacts in the same body pair.
-}
addEdgeContact : Int -> Vec3 -> Int -> List Int -> VertexBuffer -> Int -> List Int -> VertexBuffer -> List Contact -> List Contact
addEdgeContact shapeKey separatingAxis dir1Idx edges1 buffer1 dir2Idx edges2 buffer2 contacts =
    let
        reversedSeparatingAxis =
            Vec3.negate separatingAxis

        ( edge1Idx, ( e1p, e1q ) ) =
            pickSupportEdge reversedSeparatingAxis edges1 buffer1

        ( edge2Idx, ( e2p, e2q ) ) =
            pickSupportEdge separatingAxis edges2 buffer2

        ( pi, pj ) =
            Vec3.closestPointsBetweenSegments e1p e1q e2p e2q
    in
    { shapeKey = shapeKey
    , featureKey = ContactId.convexConvexEdge dir1Idx edge1Idx dir2Idx edge2Idx
    , ni = reversedSeparatingAxis
    , pi = pi
    , pj = pj
    }
        :: contacts


{-| Pick the edge in a direction group whose midpoint is furthest along
`supportDir`. The 1-based index is part of the warm-start cache key.
-}
pickSupportEdge : Vec3 -> List Int -> VertexBuffer -> ( Int, ( Vec3, Vec3 ) )
pickSupportEdge supportDir edges buffer =
    pickSupportEdgeHelp supportDir edges buffer 1 0 ( Vec3.zero, Vec3.zero ) -Const.maxNumber


pickSupportEdgeHelp : Vec3 -> List Int -> VertexBuffer -> Int -> Int -> ( Vec3, Vec3 ) -> Float -> ( Int, ( Vec3, Vec3 ) )
pickSupportEdgeHelp supportDir edges buffer idx bestIdx bestEdge bestDot =
    case edges of
        i1 :: i2 :: rest ->
            let
                v1 =
                    VertexBuffer.get i1 buffer

                v2 =
                    VertexBuffer.get i2 buffer

                midDot =
                    supportDir.x * (v1.x + v2.x) + supportDir.y * (v1.y + v2.y) + supportDir.z * (v1.z + v2.z)
            in
            if midDot - bestDot > 0 then
                pickSupportEdgeHelp supportDir rest buffer (idx + 1) idx ( v1, v2 ) midDot

            else
                pickSupportEdgeHelp supportDir rest buffer (idx + 1) bestIdx bestEdge bestDot

        _ ->
            ( bestIdx, bestEdge )


clipTwoFaces : Int -> Int -> Int -> Face -> VertexBuffer -> Face -> VertexBuffer -> Vec3 -> List Contact -> List Contact
clipTwoFaces shapeKey faceId1 faceId2 face faceBuffer incidentFace incidentBuffer separatingAxis contacts =
    let
        -- Only the two faces in contact are materialised, on demand — the other
        -- faces' vertices are never turned into a `List Vec3`.
        referenceVertices =
            Convex.faceVertices faceBuffer face

        incidentVertices =
            Convex.faceVertices incidentBuffer incidentFace

        point =
            case referenceVertices of
                first :: _ ->
                    first

                [] ->
                    Vec3.zero

        facePlaneConstant =
            -(Vec3.dot face.normal point)
    in
    clipTwoFacesHelp shapeKey
        faceId1
        faceId2
        separatingAxis
        face
        facePlaneConstant
        0
        (clipAgainstAdjacentFaces face.normal referenceVertices incidentVertices)
        contacts


clipTwoFacesHelp : Int -> Int -> Int -> Vec3 -> Face -> Float -> Int -> List Vec3 -> List Contact -> List Contact
clipTwoFacesHelp shapeKey faceId1 faceId2 separatingAxis face facePlaneConstant n vertices result =
    case vertices of
        vertex :: remainingVertices ->
            let
                depth =
                    Vec3.dot face.normal vertex + facePlaneConstant
            in
            if depth - Const.contactBreakingThreshold < 0 then
                clipTwoFacesHelp shapeKey
                    faceId1
                    faceId2
                    separatingAxis
                    face
                    facePlaneConstant
                    (n + 1)
                    remainingVertices
                    ({ shapeKey = shapeKey
                     , featureKey = ContactId.convexConvexFace faceId1 faceId2 (n + 1)
                     , ni = separatingAxis
                     , pi =
                        { x = vertex.x - depth * face.normal.x
                        , y = vertex.y - depth * face.normal.y
                        , z = vertex.z - depth * face.normal.z
                        }
                     , pj = vertex
                     }
                        :: result
                    )

            else
                clipTwoFacesHelp shapeKey
                    faceId1
                    faceId2
                    separatingAxis
                    face
                    facePlaneConstant
                    (n + 1)
                    remainingVertices
                    result

        [] ->
            result


{-| Finds the face whose normal is most aligned with `-separatingAxis`.
The partner is the antiparallel of the primary, so one dot per group
covers both. Returns `( -1, emptyFace )` for empty groups.
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

        (TwoSidedFace n1 i1 n2 i2) :: restGroups ->
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

        (OneSidedFace n1 i1 _ _) :: restGroups ->
            let
                d =
                    Vec3.dot n1 separatingAxis
            in
            if currentBestDistance - d > 0 then
                bestFaceWalk separatingAxis restGroups (faceId + 1) faceId { normal = n1, vertices = i1 } d

            else
                bestFaceWalk separatingAxis restGroups (faceId + 1) currentBestFaceId currentBestFace currentBestDistance


clipAgainstAdjacentFaces : Vec3 -> List Vec3 -> List Vec3 -> List Vec3
clipAgainstAdjacentFaces normal referenceVertices incidentVertices =
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
        incidentVertices
        referenceVertices


clipFaceAgainstPlaneAdd : Vec3 -> Float -> Vec3 -> Vec3 -> List Vec3 -> List Vec3
clipFaceAgainstPlaneAdd planeNormal planeConstant prev next result =
    let
        nDotPrev =
            Vec3.dot planeNormal prev + planeConstant

        nDotNext =
            Vec3.dot planeNormal next + planeConstant
    in
    if nDotPrev < 0 then
        if nDotNext < 0 then
            next :: result

        else
            Vec3.lerp (nDotPrev / (nDotPrev - nDotNext)) prev next
                :: result

    else if nDotNext < 0 then
        next
            :: Vec3.lerp (nDotPrev / (nDotPrev - nDotNext)) prev next
            :: result

    else
        result


findSeparatingAxis : Convex -> Convex -> Maybe Vec3
findSeparatingAxis convex1 convex2 =
    case findFaceSAT convex1 convex2 of
        Nothing ->
            Nothing

        Just winner ->
            case findEdgeSAT convex1 convex2 winner.dmin of
                EdgeSeparates _ _ _ _ _ ->
                    Nothing

                EdgeBeats edgeAxis _ _ _ _ ->
                    Just (orientAxis convex1 convex2 edgeAxis)

                NoEdgeBeats _ _ _ _ _ ->
                    Just (orientAxis convex1 convex2 winner.axis)


{-| Test every face group's direction as a SAT axis. Returns the winning
body + group so `dispatchBestFaces` can skip one of the two `bestFace`
walks while keeping contact IDs stable.
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
    OneSidedFace Vec3.zero [] () ()


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
            case testSeparatingAxis convex1 convex2 (Convex.faceGroupNormal group) of
                Nothing ->
                    Nothing

                Just dist ->
                    let
                        groupSize =
                            case group of
                                TwoSidedFace _ _ _ _ ->
                                    2

                                OneSidedFace _ _ _ _ ->
                                    1
                    in
                    if dist - dmin < 0 then
                        findFaceSATHelp convex1 convex2 currentSide restNormals nextNormals (nextGroupIdx + groupSize) nextGroupIdx currentSide group dist

                    else
                        findFaceSATHelp convex1 convex2 currentSide restNormals nextNormals (nextGroupIdx + groupSize) winnerIdx winnerSide winnerGroup dmin


{-| The two nullary cases carry five `()` fields so all three variants share
`EdgeBeats`'s object shape — a monomorphic `.$` for the consuming `case` and for
the `best` accumulator threaded through `findEdgeSATHelp`. They're built once as
`edgeSeparates`/`noEdgeBeats` constants and reused, so the padding costs no
per-call allocation (unlike re-applying the constructor each time).
-}
type EdgeResult
    = EdgeSeparates () () () () ()
    | EdgeBeats Vec3 Int (List Int) Int (List Int)
    | NoEdgeBeats () () () () ()


edgeSeparates : EdgeResult
edgeSeparates =
    EdgeSeparates () () () () ()


noEdgeBeats : EdgeResult
noEdgeBeats =
    NoEdgeBeats () () () () ()


{-| Multiplier on edge SAT depth when ranked against face SAT depth —
edge must be at least 5% better to take the edge-edge path. Relative so
it scales with shape size; an absolute window mis-classifies
shallow-rotation edge-vs-face configurations as edge-edge and emits one
contact where clipping would emit two.
-}
edgeBiasFactor : Float
edgeBiasFactor =
    1.05


{-| Iterate `(dir1, dir2)` pairs of unique edge directions. The winner
stores its full edge list so the support-edge picker walks only parallel
edges (4 for a cube) instead of all face-edges. Direction indices are
1-based, stable under `placeIn` — safe to encode in contact ids.
-}
findEdgeSAT : Convex -> Convex -> Float -> EdgeResult
findEdgeSAT convex1 convex2 faceDmin =
    -- Pre-bias the threshold so the loop runs plain `dist < dmin`.
    -- Once an edge wins, subsequent edges compete on raw depth — the
    -- 5% bias is a face/edge boundary effect, not edge/edge.
    findEdgeSATHelp convex1
        convex2
        convex2.uniqueEdges
        convex1.uniqueEdges
        convex2.uniqueEdges
        1
        1
        noEdgeBeats
        (faceDmin / edgeBiasFactor)


findEdgeSATHelp : Convex -> Convex -> List (List Int) -> List (List Int) -> List (List Int) -> Int -> Int -> EdgeResult -> Float -> EdgeResult
findEdgeSATHelp convex1 convex2 initGroups2 groups1 groups2 dir1Idx dir2Idx best dmin =
    case groups1 of
        [] ->
            best

        ((v1a :: v1b :: _) as group1) :: remainingGroups1 ->
            case groups2 of
                [] ->
                    -- requeue groups2 and advance outer
                    findEdgeSATHelp convex1 convex2 initGroups2 remainingGroups1 initGroups2 (dir1Idx + 1) 1 best dmin

                ((v2a :: v2b :: _) as group2) :: remainingGroups2 ->
                    let
                        dir1 =
                            Vec3.direction (VertexBuffer.get v1a convex1.vertexBuffer) (VertexBuffer.get v1b convex1.vertexBuffer)

                        dir2 =
                            Vec3.direction (VertexBuffer.get v2a convex2.vertexBuffer) (VertexBuffer.get v2b convex2.vertexBuffer)

                        cross =
                            Vec3.cross dir1 dir2
                    in
                    if Vec3.almostZero cross then
                        -- skip parallel directions
                        findEdgeSATHelp convex1 convex2 initGroups2 groups1 remainingGroups2 dir1Idx (dir2Idx + 1) best dmin

                    else
                        let
                            normalizedCross =
                                Vec3.normalize cross
                        in
                        case testSeparatingAxis convex1 convex2 normalizedCross of
                            Nothing ->
                                edgeSeparates

                            Just dist ->
                                if dist - dmin < 0 then
                                    findEdgeSATHelp convex1 convex2 initGroups2 groups1 remainingGroups2 dir1Idx (dir2Idx + 1) (EdgeBeats normalizedCross dir1Idx group1 dir2Idx group2) dist

                                else
                                    findEdgeSATHelp convex1 convex2 initGroups2 groups1 remainingGroups2 dir1Idx (dir2Idx + 1) best dmin

                _ :: remainingGroups2 ->
                    -- malformed group2 (< 2 points): skip
                    findEdgeSATHelp convex1 convex2 initGroups2 groups1 remainingGroups2 dir1Idx (dir2Idx + 1) best dmin

        _ :: remainingGroups1 ->
            -- malformed group (< 2 points): skip
            findEdgeSATHelp convex1 convex2 initGroups2 remainingGroups1 groups2 (dir1Idx + 1) dir2Idx best dmin


{-| If projections of two convexes don’t overlap, then they don’t collide.
-}
testSeparatingAxis : Convex -> Convex -> Vec3 -> Maybe Float
testSeparatingAxis convex1 convex2 separatingAxis =
    let
        p1 =
            project separatingAxis Const.maxNumber -Const.maxNumber convex1.vertices

        p2 =
            project separatingAxis Const.maxNumber -Const.maxNumber convex2.vertices

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
