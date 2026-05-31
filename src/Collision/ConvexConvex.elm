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
import Shapes.Convex as Convex exposing (Convex, Face)


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
    , primary : Face
    , partner : Maybe Face
    }


addContacts : Int -> Convex -> Convex -> List Contact -> List Contact
addContacts shapeKey convex1 convex2 contacts =
    case findFaceSAT convex1 convex2 of
        Nothing ->
            contacts

        Just winner ->
            case findEdgeSAT convex1 convex2 winner.dmin of
                EdgeSeparates ->
                    contacts

                EdgeBeats edgeAxis dir1Idx edges1 dir2Idx edges2 ->
                    addEdgeContact shapeKey
                        (orientAxis convex1 convex2 edgeAxis)
                        dir1Idx
                        edges1
                        dir2Idx
                        edges2
                        contacts

                NoEdgeBeats ->
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
                            pickWinningFace winner.groupIdx winner.primary winner.partner separatingAxis

                        ( oid, oface ) =
                            bestFace convex2.faces reversedSeparatingAxis
                    in
                    { id1 = wid, face1 = wface, id2 = oid, face2 = oface }

                Convex2 ->
                    let
                        ( oid, oface ) =
                            bestFace convex1.faces separatingAxis

                        ( wid, wface ) =
                            pickWinningFace winner.groupIdx winner.primary winner.partner reversedSeparatingAxis
                    in
                    { id1 = oid, face1 = oface, id2 = wid, face2 = wface }
    in
    if picked.id1 == -1 || picked.id2 == -1 then
        contacts

    else
        clipTwoFaces shapeKey
            picked.id1
            picked.id2
            picked.face1
            picked.face2
            reversedSeparatingAxis
            contacts


{-| Pick the face in the group whose normal is most anti-aligned with
`axisToward`. The partner's dot is the negation of the primary's, so a
single dot product decides by sign.
-}
pickWinningFace : Int -> Face -> Maybe Face -> Vec3 -> ( Int, Face )
pickWinningFace groupIdx primary partner axisToward =
    case partner of
        Just p ->
            if Vec3.dot primary.normal axisToward <= 0 then
                ( groupIdx, primary )

            else
                ( groupIdx + 1, p )

        Nothing ->
            ( groupIdx, primary )


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
addEdgeContact : Int -> Vec3 -> Int -> List ( Vec3, Vec3 ) -> Int -> List ( Vec3, Vec3 ) -> List Contact -> List Contact
addEdgeContact shapeKey separatingAxis dir1Idx edges1 dir2Idx edges2 contacts =
    let
        reversedSeparatingAxis =
            Vec3.negate separatingAxis

        ( edge1Idx, ( e1p, e1q ) ) =
            pickSupportEdge reversedSeparatingAxis edges1

        ( edge2Idx, ( e2p, e2q ) ) =
            pickSupportEdge separatingAxis edges2

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
pickSupportEdge : Vec3 -> List ( Vec3, Vec3 ) -> ( Int, ( Vec3, Vec3 ) )
pickSupportEdge supportDir edges =
    pickSupportEdgeHelp supportDir edges 1 0 ( Vec3.zero, Vec3.zero ) -Const.maxNumber


pickSupportEdgeHelp : Vec3 -> List ( Vec3, Vec3 ) -> Int -> Int -> ( Vec3, Vec3 ) -> Float -> ( Int, ( Vec3, Vec3 ) )
pickSupportEdgeHelp supportDir edges idx bestIdx bestEdge bestDot =
    case edges of
        (( v1, v2 ) as edge) :: rest ->
            let
                midDot =
                    supportDir.x * (v1.x + v2.x) + supportDir.y * (v1.y + v2.y) + supportDir.z * (v1.z + v2.z)
            in
            if midDot - bestDot > 0 then
                pickSupportEdgeHelp supportDir rest (idx + 1) idx edge midDot

            else
                pickSupportEdgeHelp supportDir rest (idx + 1) bestIdx bestEdge bestDot

        [] ->
            ( bestIdx, bestEdge )


clipTwoFaces : Int -> Int -> Int -> Face -> Face -> Vec3 -> List Contact -> List Contact
clipTwoFaces shapeKey faceId1 faceId2 face { vertices } separatingAxis contacts =
    let
        point =
            case face.vertices of
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
        (clipAgainstAdjacentFaces face vertices)
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
bestFace : List ( Face, Maybe Face ) -> Vec3 -> ( Int, Face )
bestFace groups separatingAxis =
    bestFaceWalk separatingAxis groups 1 -1 emptyFace Const.maxNumber


emptyFace : Face
emptyFace =
    { vertices = [], normal = Vec3.zero }


bestFaceWalk : Vec3 -> List ( Face, Maybe Face ) -> Int -> Int -> Face -> Float -> ( Int, Face )
bestFaceWalk separatingAxis groups faceId currentBestFaceId currentBestFace currentBestDistance =
    case groups of
        [] ->
            ( currentBestFaceId, currentBestFace )

        ( primary, Just partner ) :: restGroups ->
            let
                primaryDot =
                    Vec3.dot primary.normal separatingAxis

                partnerDot =
                    -primaryDot

                -- Compete primary against running best.
                ( id1, f1, d1 ) =
                    if currentBestDistance - primaryDot > 0 then
                        ( faceId, primary, primaryDot )

                    else
                        ( currentBestFaceId, currentBestFace, currentBestDistance )
            in
            -- Compete partner against the result.
            if d1 - partnerDot > 0 then
                bestFaceWalk separatingAxis restGroups (faceId + 2) (faceId + 1) partner partnerDot

            else
                bestFaceWalk separatingAxis restGroups (faceId + 2) id1 f1 d1

        ( primary, Nothing ) :: restGroups ->
            let
                d =
                    Vec3.dot primary.normal separatingAxis
            in
            if currentBestDistance - d > 0 then
                bestFaceWalk separatingAxis restGroups (faceId + 1) faceId primary d

            else
                bestFaceWalk separatingAxis restGroups (faceId + 1) currentBestFaceId currentBestFace currentBestDistance


clipAgainstAdjacentFaces : Face -> List Vec3 -> List Vec3
clipAgainstAdjacentFaces { vertices, normal } faceVertices =
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
        faceVertices
        vertices


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
                EdgeSeparates ->
                    Nothing

                EdgeBeats edgeAxis _ _ _ _ ->
                    Just (orientAxis convex1 convex2 edgeAxis)

                NoEdgeBeats ->
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
        emptyFace
        Nothing
        Const.maxNumber


findFaceSATHelp : Convex -> Convex -> Side -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face ) -> Int -> Int -> Side -> Face -> Maybe Face -> Float -> Maybe FaceWinner
findFaceSATHelp convex1 convex2 currentSide normals nextNormals nextGroupIdx winnerIdx winnerSide winnerPrimary winnerPartner dmin =
    case normals of
        [] ->
            case nextNormals of
                [] ->
                    if winnerIdx == -1 then
                        Nothing

                    else
                        Just
                            { axis = winnerPrimary.normal
                            , dmin = dmin
                            , fromSide = winnerSide
                            , groupIdx = winnerIdx
                            , primary = winnerPrimary
                            , partner = winnerPartner
                            }

                _ ->
                    findFaceSATHelp convex1 convex2 Convex2 nextNormals [] 1 winnerIdx winnerSide winnerPrimary winnerPartner dmin

        ( primary, partner ) :: restNormals ->
            case testSeparatingAxis convex1 convex2 primary.normal of
                Nothing ->
                    Nothing

                Just dist ->
                    let
                        groupSize =
                            case partner of
                                Just _ ->
                                    2

                                Nothing ->
                                    1
                    in
                    if dist - dmin < 0 then
                        findFaceSATHelp convex1 convex2 currentSide restNormals nextNormals (nextGroupIdx + groupSize) nextGroupIdx currentSide primary partner dist

                    else
                        findFaceSATHelp convex1 convex2 currentSide restNormals nextNormals (nextGroupIdx + groupSize) winnerIdx winnerSide winnerPrimary winnerPartner dmin


type EdgeResult
    = EdgeSeparates
    | EdgeBeats Vec3 Int (List ( Vec3, Vec3 )) Int (List ( Vec3, Vec3 ))
    | NoEdgeBeats


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
        NoEdgeBeats
        (faceDmin / edgeBiasFactor)


findEdgeSATHelp : Convex -> Convex -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> Int -> Int -> EdgeResult -> Float -> EdgeResult
findEdgeSATHelp convex1 convex2 initGroups2 groups1 groups2 dir1Idx dir2Idx best dmin =
    case groups1 of
        [] ->
            best

        ( ( v1a, v1b ) as firstEdge1, otherEdges1 ) :: remainingGroups1 ->
            case groups2 of
                [] ->
                    -- requeue groups2 and advance outer
                    findEdgeSATHelp convex1 convex2 initGroups2 remainingGroups1 initGroups2 (dir1Idx + 1) 1 best dmin

                ( ( v2a, v2b ) as firstEdge2, otherEdges2 ) :: remainingGroups2 ->
                    let
                        dir1 =
                            Vec3.direction v1a v1b

                        dir2 =
                            Vec3.direction v2a v2b

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
                                EdgeSeparates

                            Just dist ->
                                if dist - dmin < 0 then
                                    findEdgeSATHelp convex1 convex2 initGroups2 groups1 remainingGroups2 dir1Idx (dir2Idx + 1) (EdgeBeats normalizedCross dir1Idx (firstEdge1 :: otherEdges1) dir2Idx (firstEdge2 :: otherEdges2)) dist

                                else
                                    findEdgeSATHelp convex1 convex2 initGroups2 groups1 remainingGroups2 dir1Idx (dir2Idx + 1) best dmin


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
