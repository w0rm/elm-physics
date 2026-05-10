module Collision.ConvexConvex exposing
    ( addContacts
    , bestFace
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex as Convex exposing (Convex, Face)


{-| Which body contributed the winning face axis to SAT.
-}
type Side
    = Convex1
    | Convex2


{-| The result of `findFaceSAT`. We carry not just the axis and depth that
SAT chose, but also which body contributed the winning face and that face's
group key + members. The dispatcher then uses this to skip one of the two
`bestFace` walks: the SAT-winning body's contact face is already nailed down
(it's the face whose normal points toward the other body within that group),
so we only need `bestFace` for the OTHER body.

`groupIdx` is the 1-based index of the winning group's first face in the flat
traversal order — the same numbering `bestFace` uses, so contact IDs remain
stable across the two paths (warm-start cache keys depend on this).

-}
type alias FaceWinner =
    { axis : Vec3
    , dmin : Float
    , fromSide : Side
    , groupIdx : Int
    , primary : Face
    , partner : Maybe Face
    }


addContacts : String -> Convex -> Convex -> List Contact -> List Contact
addContacts idPrefix convex1 convex2 contacts =
    case findFaceSAT convex1 convex2 of
        Nothing ->
            contacts

        Just winner ->
            case findEdgeSAT convex1 convex2 winner.dmin of
                EdgeSeparates ->
                    contacts

                EdgeBeats edgeAxis dir1Idx edges1 dir2Idx edges2 ->
                    addEdgeContact idPrefix
                        (orientAxis convex1 convex2 edgeAxis)
                        dir1Idx
                        edges1
                        dir2Idx
                        edges2
                        contacts

                NoEdgeBeats ->
                    dispatchBestFaces idPrefix convex1 convex2 winner contacts


{-| With the SAT winner's group in hand, pick the contact face on the
SAT-winning body directly (no `bestFace` walk on that side) and only run
`bestFace` against the OTHER body. The contact normal is the oriented
separating axis pointing from convex1 toward convex2.
-}
dispatchBestFaces : String -> Convex -> Convex -> FaceWinner -> List Contact -> List Contact
dispatchBestFaces idPrefix convex1 convex2 winner contacts =
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
        clipTwoFaces (idPrefix ++ "-" ++ String.fromInt picked.id1 ++ "-" ++ String.fromInt picked.id2)
            picked.face1
            picked.face2
            reversedSeparatingAxis
            contacts


{-| Pick the contact face from a SAT-winning group: the face on this body
whose normal is most anti-aligned with `axisToward`.

For a 2-face antiparallel pair (the only valid case for a convex polytope),
the partner's dot is the negation of the first face's dot, so a single
`firstFace.normal · axisToward` dot product determines the answer by sign.
We don't rely on any layout convention — only on the antiparallel pairing
that `groupFacesByNormal` guarantees.

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


{-| Emit a single edge-edge contact from the SAT-winning direction
groups. The contact id encodes `(dir1Idx, edge1Idx, dir2Idx,
edge2Idx)` — the indices are stable across `placeIn` for a given
source convex, so the solver's warm-start cache survives multi-edge
contacts in the same body pair instead of collapsing every edge-edge
contact onto a single `"-edge"` key.
-}
addEdgeContact : String -> Vec3 -> Int -> List ( Vec3, Vec3 ) -> Int -> List ( Vec3, Vec3 ) -> List Contact -> List Contact
addEdgeContact idPrefix separatingAxis dir1Idx edges1 dir2Idx edges2 contacts =
    let
        reversedSeparatingAxis =
            Vec3.negate separatingAxis

        ( edge1Idx, edge1 ) =
            pickSupportEdge reversedSeparatingAxis edges1

        ( edge2Idx, edge2 ) =
            pickSupportEdge separatingAxis edges2

        ( pi, pj ) =
            closestPointsOnSegments edge1 edge2
    in
    { id =
        idPrefix
            ++ "-e-"
            ++ String.fromInt dir1Idx
            ++ "."
            ++ String.fromInt edge1Idx
            ++ "-"
            ++ String.fromInt dir2Idx
            ++ "."
            ++ String.fromInt edge2Idx
    , ni = reversedSeparatingAxis
    , pi = pi
    , pj = pj
    }
        :: contacts


{-| Among the parallel edges of a single direction group, return the one
whose midpoint is furthest along `supportDir`, alongside its 1-based
index in the group. The index is the warm-start cache key component
(stable under `placeIn`).
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


{-| Compute the closest points between two line segments (p1, q1) and (p2, q2).
Returns (point on segment 1, point on segment 2).
-}
closestPointsOnSegments : ( Vec3, Vec3 ) -> ( Vec3, Vec3 ) -> ( Vec3, Vec3 )
closestPointsOnSegments ( p1, q1 ) ( p2, q2 ) =
    let
        d1 =
            Vec3.sub q1 p1

        d2 =
            Vec3.sub q2 p2

        r =
            Vec3.sub p1 p2

        a =
            Vec3.dot d1 d1

        e =
            Vec3.dot d2 d2

        f =
            Vec3.dot d2 r

        c =
            Vec3.dot d1 r

        b =
            Vec3.dot d1 d2

        denom =
            a * e - b * b
    in
    if denom < Const.precision then
        -- Parallel segments: pick midpoints.
        ( { x = p1.x + 0.5 * d1.x, y = p1.y + 0.5 * d1.y, z = p1.z + 0.5 * d1.z }
        , { x = p2.x + 0.5 * d2.x, y = p2.y + 0.5 * d2.y, z = p2.z + 0.5 * d2.z }
        )

    else
        let
            sUnclamped =
                (b * f - c * e) / denom

            tUnclamped =
                (a * f - b * c) / denom

            ( s, t ) =
                if sUnclamped < 0 then
                    ( 0, clamp 0 1 (f / e) )

                else if sUnclamped > 1 then
                    ( 1, clamp 0 1 ((b + f) / e) )

                else if tUnclamped < 0 then
                    ( clamp 0 1 (-c / a), 0 )

                else if tUnclamped > 1 then
                    ( clamp 0 1 ((b - c) / a), 1 )

                else
                    ( sUnclamped, tUnclamped )
        in
        ( { x = p1.x + s * d1.x, y = p1.y + s * d1.y, z = p1.z + s * d1.z }
        , { x = p2.x + t * d2.x, y = p2.y + t * d2.y, z = p2.z + t * d2.z }
        )


clipTwoFaces : String -> Face -> Face -> Vec3 -> List Contact -> List Contact
clipTwoFaces idPrefix face { vertices } separatingAxis contacts =
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
    clipTwoFacesHelp idPrefix
        separatingAxis
        face
        facePlaneConstant
        0
        (clipAgainstAdjacentFaces face vertices)
        contacts


clipTwoFacesHelp : String -> Vec3 -> Face -> Float -> Int -> List Vec3 -> List Contact -> List Contact
clipTwoFacesHelp idPrefix separatingAxis face facePlaneConstant n vertices result =
    case vertices of
        vertex :: remainingVertices ->
            let
                -- used to be (max minDist depth), where minDist = -100
                depth =
                    Vec3.dot face.normal vertex + facePlaneConstant
            in
            if depth <= 0 then
                clipTwoFacesHelp idPrefix
                    separatingAxis
                    face
                    facePlaneConstant
                    (n + 1)
                    remainingVertices
                    ({ id = idPrefix ++ String.fromInt (n + 1)
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
                clipTwoFacesHelp idPrefix
                    separatingAxis
                    face
                    facePlaneConstant
                    (n + 1)
                    remainingVertices
                    result

        [] ->
            result


{-| Finds the face whose normal is most aligned with `-separatingAxis`
(equivalently: minimum `dot face.normal separatingAxis`).

For each 2-face group in a convex polytope, the second face is the
antiparallel partner of the first, so a single `firstFace.normal · axis`
dot product gives both faces' dots (the second is the negation). For a
cube (3 antiparallel pairs) that's 3 dots + 3 sign-tracked comparisons
instead of 6 dots. For a 32-subdivision cylinder (16 antiparallel side
pairs + 1 cap pair) it's 17 dots instead of 34. The shortcut needs no
layout convention — only the antiparallel pairing that
`groupFacesByNormal` already enforces.

Returns `( -1, emptyFace )` for empty `groups` (defensive — valid convexes
always have faces). Callers detect this via `id == -1`.

IDs are 1-based and assigned in flat traversal order across groups; for a
cube `[(+x, [+xf, -xf]), (+y, [+yf, -yf]), (+z, [+zf, -zf])]` the IDs are
1..6 in that flat order.

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


{-| Walk every face group on both convexes, testing each group's direction as
a SAT axis. Returns the winner with enough info (which body, which group) for
`dispatchBestFaces` to skip one of the two `bestFace` walks. `groupIdx` is
the 1-based index of the winning group's first face in flat traversal order
(matching the IDs `bestFace` assigns), so contact IDs stay stable across
both code paths.
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


{-| Edge SAT must beat face SAT by more than this to take the edge-edge
contact path (single contact via `closestPointsOnSegments`). Below this
margin we stay on face-clip (4-corner manifold).

`Const.precision` (1e-6) is too tight: cold simulation of an
axis-aligned 5-box stack drifts pair-wise into the 1e-6..2e-5 window
within a few hundred frames; flipping a face-aligned pair onto a
single edge-edge contact removes the torque arm and the stack tips.
Bisection on `coldStableFrames` for the 5-box stack: 1e-5 → 14762
frames, 2e-5 → 73401, 2.5e-5 → 100000 (stable). Genuine edge-edge
contact (two cubes rotated by pi/6 against each other, ConvexConvexTest)
clears this threshold by tens of percent, so the cheap edge path
still fires when it's actually correct.

-}
edgePreferenceMargin : Float
edgePreferenceMargin =
    2.5e-5


{-| Iterate `(dir1, dir2)` pairs of unique edge directions. Each
direction group is `( firstEdge, otherEdges )` — direction is derived
from the first edge via `Vec3.direction firstEdge.v1 firstEdge.v2`.
The winner stores its full edge list (`firstEdge :: otherEdges`) so
the support-edge picker walks only the parallel edges of the chosen
direction (4 for a cube) instead of every face-edge (24 face-edge-
visits today). Direction indices are 1-based and assigned in
iteration order — stable under `placeIn`, which is what makes them
safe to encode in contact ids.
-}
findEdgeSAT : Convex -> Convex -> Float -> EdgeResult
findEdgeSAT convex1 convex2 faceDmin =
    findEdgeSATHelp convex1
        convex2
        convex2.uniqueEdges
        convex1.uniqueEdges
        convex2.uniqueEdges
        1
        1
        NoEdgeBeats
        faceDmin


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
                                if dist - dmin + edgePreferenceMargin < 0 then
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
    if d1 < 0 || d2 < 0 then
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
