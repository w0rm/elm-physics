module Collision.CapsuleConvex exposing (addContacts, supportFeature)

import Collision.ConvexConvex as ConvexConvex
import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.VertexBuffer as VertexBuffer exposing (VertexBuffer)
import Shapes.Capsule exposing (Capsule)
import Shapes.Convex as Convex exposing (Convex, Face)


{-| `|dot(capsule.axis, separatingAxis)|` below this snaps to the cylinder
(face-clip) path. Wider than `Const.precision` so near-flat capsules don't
chatter between cap-only and two-contact face-clip.
-}
perpendicularThreshold : Float
perpendicularThreshold =
    1.0e-3


{-| `dot(face.normal, separatingAxis)` above this means SAT picked a face
normal (face-clip path); otherwise the axis came from an edge/vertex.
-}
faceAlignedThreshold : Float
faceAlignedThreshold =
    0.999


{-| Convex vertices within this projection gap of the support maximum count as
tied, so a supporting edge is collected whole rather than split to one vertex.
-}
supportTieTolerance : Float
supportTieTolerance =
    1.0e-4


{-| Generate contacts between a capsule (body 1) and a convex (body 2).

Contact id suffix — encodes which feature each side touches so warm-start
keys clear cleanly across face/edge/vertex transitions:

    | Path                                          | Id                |
    | --------------------------------------------- | ----------------- |
    | Cap on face F                                 | `e1-fF` / `e2-fF` |
    | Cap on convex edge                            | `e1-e` / `e2-e`   |
    | Cap on convex vertex                          | `e1-v` / `e2-v`   |
    | Cylinder on face F (segment clip)             | `c1-fF` / `c2-fF` |
    | Cylinder on face F (closest-edge fallback)    | `c-fF`            |
    | Cylinder on convex edge, parallel             | `c1-e` / `c2-e`   |
    | Cylinder on convex edge, skew                 | `c-e`             |
    | Cylinder on convex vertex                     | `c-v`             |

`e*` = endpoint (cap), `c*` = cylinder body. `1`/`2` distinguishes the
ep1/ep2 sides when both contribute; bare suffixes are single contacts.

-}
addContacts : Int -> (Contact -> Contact) -> Capsule -> Convex -> List Contact -> List Contact
addContacts shapeKey orderContact capsule convex contacts =
    let
        ep1 =
            { x = capsule.position.x - capsule.halfLength * capsule.axis.x
            , y = capsule.position.y - capsule.halfLength * capsule.axis.y
            , z = capsule.position.z - capsule.halfLength * capsule.axis.z
            }

        ep2 =
            { x = capsule.position.x + capsule.halfLength * capsule.axis.x
            , y = capsule.position.y + capsule.halfLength * capsule.axis.y
            , z = capsule.position.z + capsule.halfLength * capsule.axis.z
            }
    in
    case findSeparatingAxis capsule convex ep1 ep2 of
        Nothing ->
            contacts

        Just separatingAxis ->
            case testCapsuleConvexAxis capsule convex separatingAxis of
                Nothing ->
                    contacts

                Just penetration ->
                    let
                        t =
                            Vec3.dot capsule.axis separatingAxis

                        -- `Just` only when the SAT axis is a face normal;
                        -- non-face axes go through the edge/vertex path.
                        faceContext =
                            let
                                ( fid, f ) =
                                    ConvexConvex.bestFace convex.faces (Vec3.negate separatingAxis)
                            in
                            if fid == -1 then
                                Nothing

                            else if Vec3.dot f.normal separatingAxis - faceAlignedThreshold > 0 then
                                Just ( fid, f )

                            else
                                Nothing

                        -- Convex's support feature along the SAT axis:
                        -- an edge (2 verts) or a single vertex.
                        supportVerts =
                            case faceContext of
                                Just _ ->
                                    []

                                Nothing ->
                                    supportFeature separatingAxis convex

                        convexFeature =
                            case faceContext of
                                Just ( fid, _ ) ->
                                    ContactId.onConvexFace fid

                                Nothing ->
                                    case supportVerts of
                                        _ :: _ :: _ ->
                                            ContactId.onConvexEdge

                                        _ :: [] ->
                                            ContactId.onConvexVertex

                                        [] ->
                                            ContactId.onConvexNone
                    in
                    if t - perpendicularThreshold > 0 then
                        -- Cap-on-feature: ep1 is the deepest cap. Also pick
                        -- up any cylinder-body contacts on the SAT-aligned
                        -- face — no multi-frame manifold accumulation.
                        contacts
                            |> addDirectContact shapeKey (ContactId.capsuleCapEnd1 convexFeature) orderContact ep1 separatingAxis penetration capsule
                            |> addBodyContacts shapeKey convexFeature orderContact convex.vertexBuffer faceContext capsule ep1 ep2 separatingAxis ep1

                    else if t + perpendicularThreshold < 0 then
                        -- Cap-on-feature: ep2 is the deepest cap.
                        contacts
                            |> addDirectContact shapeKey (ContactId.capsuleCapEnd2 convexFeature) orderContact ep2 separatingAxis penetration capsule
                            |> addBodyContacts shapeKey convexFeature orderContact convex.vertexBuffer faceContext capsule ep1 ep2 separatingAxis ep2

                    else
                        -- Cylinder body touches the convex.
                        case faceContext of
                            Just ( _, face ) ->
                                -- Face Support: clip the capsule segment
                                -- against the face's adjacent edge planes;
                                -- emit one contact per surviving endpoint.
                                case clipSegmentAgainstFace convex.vertexBuffer face ep1 ep2 of
                                    ClipAlive p1 p2 ->
                                        contacts
                                            |> addDirectContact shapeKey (ContactId.capsuleCylinder1 convexFeature) orderContact p1 separatingAxis penetration capsule
                                            |> addDirectContact shapeKey (ContactId.capsuleCylinder2 convexFeature) orderContact p2 separatingAxis penetration capsule

                                    ClipDead _ _ ->
                                        -- Segment outside the polygon → the
                                        -- contact is on a face edge; re-derive
                                        -- normal and depth from closest-points
                                        -- geometry (SAT's penetration is too
                                        -- generous here).
                                        addClosestEdgeContact shapeKey (ContactId.capsuleCylinder convexFeature) orderContact convex.vertexBuffer face ep1 ep2 capsule contacts

                            Nothing ->
                                -- Edge/Vertex support: emit at the closest
                                -- point between capsule axis and feature.
                                case supportVerts of
                                    v1 :: v2 :: _ ->
                                        if Vec3.almostZero (Vec3.cross capsule.axis (Vec3.sub v2 v1)) then
                                            -- Parallel: contact is a segment,
                                            -- emit one at each end of the overlap.
                                            addParallelEdgeContacts shapeKey convexFeature orderContact ep1 ep2 v1 v2 separatingAxis penetration capsule contacts

                                        else
                                            -- Skew: single closest-pair contact.
                                            let
                                                ( pCapsule, _ ) =
                                                    Vec3.closestPointsBetweenSegments ep1 ep2 v1 v2
                                            in
                                            addDirectContact shapeKey (ContactId.capsuleCylinder convexFeature) orderContact pCapsule separatingAxis penetration capsule contacts

                                    v :: [] ->
                                        let
                                            ( pCapsule, _ ) =
                                                Vec3.closestPointsBetweenSegments ep1 ep2 v v
                                        in
                                        addDirectContact shapeKey (ContactId.capsuleCylinder convexFeature) orderContact pCapsule separatingAxis penetration capsule contacts

                                    [] ->
                                        contacts


{-| Cylinder body parallel to a convex edge: emit one contact at each end
of the overlap interval. Falls back to a single closest-pair contact if
the overlap collapses to a point.
-}
addParallelEdgeContacts : Int -> Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Capsule -> List Contact -> List Contact
addParallelEdgeContacts shapeKey convexFeature orderContact ep1 ep2 v1 v2 separatingAxis penetration capsule contacts =
    let
        sEp1 =
            Vec3.dot ep1 capsule.axis

        sEp2 =
            Vec3.dot ep2 capsule.axis

        sV1 =
            Vec3.dot v1 capsule.axis

        sV2 =
            Vec3.dot v2 capsule.axis

        sLo =
            max (min sEp1 sEp2) (min sV1 sV2)

        sHi =
            min (max sEp1 sEp2) (max sV1 sV2)

        pointAt s =
            { x = ep1.x + (s - sEp1) * capsule.axis.x
            , y = ep1.y + (s - sEp1) * capsule.axis.y
            , z = ep1.z + (s - sEp1) * capsule.axis.z
            }
    in
    if sHi - sLo - Const.precision > 0 then
        contacts
            |> addDirectContact shapeKey (ContactId.capsuleCylinder1 convexFeature) orderContact (pointAt sLo) separatingAxis penetration capsule
            |> addDirectContact shapeKey (ContactId.capsuleCylinder2 convexFeature) orderContact (pointAt sHi) separatingAxis penetration capsule

    else
        let
            ( pCapsule, _ ) =
                Vec3.closestPointsBetweenSegments ep1 ep2 v1 v2
        in
        addDirectContact shapeKey (ContactId.capsuleCylinder convexFeature) orderContact pCapsule separatingAxis penetration capsule contacts


addDirectContact : Int -> Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> Float -> Capsule -> List Contact -> List Contact
addDirectContact shapeKey featureKey orderContact segmentPoint separatingAxis penetration capsule contacts =
    let
        normal =
            Vec3.negate separatingAxis

        pi =
            { x = segmentPoint.x + capsule.radius * normal.x
            , y = segmentPoint.y + capsule.radius * normal.y
            , z = segmentPoint.z + capsule.radius * normal.z
            }

        pj =
            { x = pi.x - penetration * normal.x
            , y = pi.y - penetration * normal.y
            , z = pi.z - penetration * normal.z
            }
    in
    orderContact
        { shapeKey = shapeKey
        , featureKey = featureKey
        , ni = normal
        , pi = pi
        , pj = pj
        }
        :: contacts


{-| For tilted-cap configurations: emit additional cylinder-body contacts
where the body crosses the SAT-aligned face. The cap endpoint is skipped
to avoid two contacts at the same world point with different depths.
-}
addBodyContacts : Int -> Int -> (Contact -> Contact) -> VertexBuffer -> Maybe ( Int, Face ) -> Capsule -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Contact -> List Contact
addBodyContacts shapeKey convexFeature orderContact buffer faceContext capsule ep1 ep2 separatingAxis capPoint contacts =
    case faceContext of
        Nothing ->
            contacts

        Just ( _, face ) ->
            case clipSegmentAgainstFace buffer face ep1 ep2 of
                ClipDead _ _ ->
                    contacts

                ClipAlive q1 q2 ->
                    let
                        facePlaneConstant =
                            case Convex.faceVertices buffer face of
                                v :: _ ->
                                    -(Vec3.dot face.normal v)

                                [] ->
                                    0
                    in
                    contacts
                        |> tryAddBodyPoint shapeKey (ContactId.capsuleCylinder1 convexFeature) orderContact face facePlaneConstant separatingAxis capsule capPoint q1
                        |> tryAddBodyPoint shapeKey (ContactId.capsuleCylinder2 convexFeature) orderContact face facePlaneConstant separatingAxis capsule capPoint q2


tryAddBodyPoint : Int -> Int -> (Contact -> Contact) -> Face -> Float -> Vec3 -> Capsule -> Vec3 -> Vec3 -> List Contact -> List Contact
tryAddBodyPoint shapeKey featureKey orderContact face facePlaneConstant separatingAxis capsule capPoint point contacts =
    if Vec3.distance point capPoint < Const.precision then
        contacts

    else
        let
            signedDist =
                Vec3.dot face.normal point + facePlaneConstant

            bodyPen =
                capsule.radius - signedDist
        in
        if bodyPen <= 0 then
            contacts

        else
            addDirectContact shapeKey featureKey orderContact point separatingAxis bodyPen capsule contacts


{-| `ClipDead` carries two `()` fields so both variants share `ClipAlive`'s
object shape (monomorphic `.$`); built once as `clipDead` and reused, so the
padding costs no per-call allocation.
-}
type ClipResult
    = ClipAlive Vec3 Vec3
    | ClipDead () ()


clipDead : ClipResult
clipDead =
    ClipDead () ()


{-| Sutherland-Hodgman clip of a segment against a face's adjacent edge
planes. Returns `ClipAlive p1 p2` for the surviving segment, or `ClipDead`
if the segment lies entirely outside the polygon.
-}
clipSegmentAgainstFace : VertexBuffer -> Face -> Vec3 -> Vec3 -> ClipResult
clipSegmentAgainstFace buffer face ep1 ep2 =
    let
        vertices =
            Convex.faceVertices buffer face
    in
    case vertices of
        first :: _ :: _ ->
            walkClipEdge face.normal first vertices ep1 ep2

        _ ->
            ClipAlive ep1 ep2


walkClipEdge : Vec3 -> Vec3 -> List Vec3 -> Vec3 -> Vec3 -> ClipResult
walkClipEdge faceNormal firstVertex vertices p1 p2 =
    case vertices of
        v1 :: rest1 ->
            let
                v2 =
                    case rest1 of
                        [] ->
                            firstVertex

                        next :: _ ->
                            next

                edge =
                    Vec3.normalize (Vec3.sub v1 v2)

                planeNormal =
                    Vec3.cross faceNormal edge

                planeConstant =
                    -(Vec3.dot v1 planeNormal)

                d1 =
                    Vec3.dot planeNormal p1 + planeConstant

                d2 =
                    Vec3.dot planeNormal p2 + planeConstant
            in
            if d1 < 0 && d2 < 0 then
                walkClipEdge faceNormal firstVertex rest1 p1 p2

            else if d1 >= 0 && d2 >= 0 then
                clipDead

            else if d1 < 0 then
                walkClipEdge faceNormal firstVertex rest1 p1 (Vec3.lerp (d1 / (d1 - d2)) p1 p2)

            else
                walkClipEdge faceNormal firstVertex rest1 (Vec3.lerp (d1 / (d1 - d2)) p1 p2) p2

        [] ->
            ClipAlive p1 p2


{-| Fallback when face-support clipping discards the segment: emit a single
contact at the closest face edge. Skipped silently if no edge is within
`capsule.radius` of the capsule axis.
-}
addClosestEdgeContact : Int -> Int -> (Contact -> Contact) -> VertexBuffer -> Face -> Vec3 -> Vec3 -> Capsule -> List Contact -> List Contact
addClosestEdgeContact shapeKey featureKey orderContact buffer face ep1 ep2 capsule contacts =
    let
        radiusSq =
            capsule.radius * capsule.radius

        result =
            closestEdgeToSegment buffer face ep1 ep2 radiusSq
    in
    -- Sentinel: distSq == radiusSq means no edge was within radius.
    if result.distSq - radiusSq >= 0 then
        contacts

    else
        let
            distance =
                sqrt result.distSq
        in
        if distance - Const.precision <= 0 then
            -- Axis intersects the edge — direction is undefined.
            contacts

        else
            let
                -- separatingAxis: from convex toward capsule.
                inv =
                    1 / distance

                edgeSeparatingAxis =
                    { x = (result.pCapsule.x - result.pEdge.x) * inv
                    , y = (result.pCapsule.y - result.pEdge.y) * inv
                    , z = (result.pCapsule.z - result.pEdge.z) * inv
                    }
            in
            addDirectContact shapeKey
                featureKey
                orderContact
                result.pCapsule
                edgeSeparatingAxis
                (capsule.radius - distance)
                capsule
                contacts


{-| Closest face edge to the capsule axis segment. `maxDistSq` is the
starting upper bound and the "no edge found" sentinel.
-}
closestEdgeToSegment : VertexBuffer -> Face -> Vec3 -> Vec3 -> Float -> { pCapsule : Vec3, pEdge : Vec3, distSq : Float }
closestEdgeToSegment buffer face ep1 ep2 maxDistSq =
    let
        vertices =
            Convex.faceVertices buffer face
    in
    case vertices of
        first :: _ :: _ ->
            walkClosestEdge ep1 ep2 first vertices Vec3.zero Vec3.zero maxDistSq

        _ ->
            { pCapsule = Vec3.zero, pEdge = Vec3.zero, distSq = maxDistSq }


walkClosestEdge : Vec3 -> Vec3 -> Vec3 -> List Vec3 -> Vec3 -> Vec3 -> Float -> { pCapsule : Vec3, pEdge : Vec3, distSq : Float }
walkClosestEdge ep1 ep2 firstVertex vertices bestPCap bestPEdge bestDistSq =
    case vertices of
        v1 :: rest1 ->
            let
                v2 =
                    case rest1 of
                        [] ->
                            firstVertex

                        next :: _ ->
                            next

                ( pCap, pEdge ) =
                    Vec3.closestPointsBetweenSegments ep1 ep2 v1 v2

                dx =
                    pCap.x - pEdge.x

                dy =
                    pCap.y - pEdge.y

                dz =
                    pCap.z - pEdge.z

                distSq =
                    dx * dx + dy * dy + dz * dz
            in
            if distSq - bestDistSq < 0 then
                walkClosestEdge ep1 ep2 firstVertex rest1 pCap pEdge distSq

            else
                walkClosestEdge ep1 ep2 firstVertex rest1 bestPCap bestPEdge bestDistSq

        [] ->
            { pCapsule = bestPCap, pEdge = bestPEdge, distSq = bestDistSq }


findSeparatingAxis : Capsule -> Convex -> Vec3 -> Vec3 -> Maybe Vec3
findSeparatingAxis capsule convex ep1 ep2 =
    testConvexNormals capsule convex ep1 ep2 convex.faces Vec3.zero Const.maxNumber


testConvexNormals : Capsule -> Convex -> Vec3 -> Vec3 -> List Convex.FaceGroup -> Vec3 -> Float -> Maybe Vec3
testConvexNormals capsule convex ep1 ep2 groups target dmin =
    case groups of
        [] ->
            testUniqueEdges capsule convex ep1 ep2 convex.uniqueEdges target dmin

        group :: restGroups ->
            let
                -- Only the primary normal is needed; the partner's is its
                -- negation and `testCapsuleConvexAxis` is sign-independent.
                normal =
                    Convex.faceGroupNormal group
            in
            case testCapsuleConvexAxis capsule convex normal of
                Nothing ->
                    Nothing

                Just dist ->
                    if dist - dmin < 0 then
                        testConvexNormals capsule convex ep1 ep2 restGroups normal dist

                    else
                        testConvexNormals capsule convex ep1 ep2 restGroups target dmin


{-| Closest-points axis between capsule axis and each unique edge — covers
the vertex-vs-cap and edge-vs-cap axes that standard convex SAT misses,
preventing false-overlap reports near convex corners. Falls back to the
cross product when closest points coincide.
-}
testUniqueEdges : Capsule -> Convex -> Vec3 -> Vec3 -> List (List Int) -> Vec3 -> Float -> Maybe Vec3
testUniqueEdges capsule convex ep1 ep2 groups target dmin =
    walkUniqueEdges capsule convex ep1 ep2 [] groups target dmin


walkUniqueEdges : Capsule -> Convex -> Vec3 -> Vec3 -> List Int -> List (List Int) -> Vec3 -> Float -> Maybe Vec3
walkUniqueEdges capsule convex ep1 ep2 edges queuedGroups target dmin =
    case edges of
        i1 :: i2 :: rest ->
            let
                v1 =
                    VertexBuffer.get i1 convex.vertexBuffer

                v2 =
                    VertexBuffer.get i2 convex.vertexBuffer
            in
            case edgeAxis capsule ep1 ep2 v1 v2 of
                Nothing ->
                    walkUniqueEdges capsule convex ep1 ep2 rest queuedGroups target dmin

                Just axis ->
                    case testCapsuleConvexAxis capsule convex axis of
                        Nothing ->
                            Nothing

                        Just dist ->
                            if dist - dmin < 0 then
                                walkUniqueEdges capsule convex ep1 ep2 rest queuedGroups axis dist

                            else
                                walkUniqueEdges capsule convex ep1 ep2 rest queuedGroups target dmin

        _ ->
            case queuedGroups of
                group :: restGroups ->
                    walkUniqueEdges capsule convex ep1 ep2 group restGroups target dmin

                [] ->
                    -- Orient the axis from convex toward capsule.
                    if Vec3.dot (Vec3.sub convex.position capsule.position) target > 0 then
                        Just (Vec3.negate target)

                    else
                        Just target


edgeAxis : Capsule -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
edgeAxis capsule ep1 ep2 v1 v2 =
    let
        ( pCap, pEdge ) =
            Vec3.closestPointsBetweenSegments ep1 ep2 v1 v2

        diff =
            Vec3.sub pCap pEdge

        distSq =
            Vec3.lengthSquared diff
    in
    if distSq - Const.precision > 0 then
        Just (Vec3.scale (1 / sqrt distSq) diff)

    else
        let
            cross =
                Vec3.cross capsule.axis (Vec3.sub v2 v1)
        in
        if Vec3.almostZero cross then
            Nothing

        else
            Just (Vec3.normalize cross)


{-| Support feature of `convex` along `axis`: at most two vertices at the
same max projection within `supportTieTolerance`, and (if two) guaranteed to
form a real convex edge — picking a face diagonal would drift the contact
off the surface.
-}
supportFeature : Vec3 -> Convex -> List Vec3
supportFeature axis convex =
    let
        vertices =
            Convex.convexVertices convex

        maxProj =
            maxProjection axis vertices -Const.maxNumber
    in
    case collectFirstTwoTied axis maxProj vertices 0 Vec3.zero of
        [] ->
            []

        [ v ] ->
            [ v ]

        firstTied :: _ ->
            case findTiedUniqueEdge axis maxProj convex.vertexBuffer convex.uniqueEdges of
                Just ( e1, e2 ) ->
                    [ e1, e2 ]

                Nothing ->
                    [ firstTied ]


maxProjection : Vec3 -> List Vec3 -> Float -> Float
maxProjection axis verts soFar =
    case verts of
        [] ->
            soFar

        v :: rest ->
            let
                p =
                    Vec3.dot v axis
            in
            maxProjection axis
                rest
                (if p - soFar > 0 then
                    p

                 else
                    soFar
                )


collectFirstTwoTied : Vec3 -> Float -> List Vec3 -> Int -> Vec3 -> List Vec3
collectFirstTwoTied axis maxProj verts count v1 =
    case verts of
        [] ->
            if count == 0 then
                []

            else
                [ v1 ]

        v :: rest ->
            if maxProj - Vec3.dot v axis - supportTieTolerance < 0 then
                if count == 0 then
                    collectFirstTwoTied axis maxProj rest 1 v

                else
                    [ v1, v ]

            else
                collectFirstTwoTied axis maxProj rest count v1


{-| Walk every unique edge across all direction groups and return the
first whose both endpoints are within tolerance of `maxProj`. Short-
circuits on first match.
-}
findTiedUniqueEdge : Vec3 -> Float -> VertexBuffer -> List (List Int) -> Maybe ( Vec3, Vec3 )
findTiedUniqueEdge axis maxProj buffer groups =
    case groups of
        group :: restGroups ->
            case findTiedEdgeInGroup axis maxProj buffer group of
                (Just _) as found ->
                    found

                Nothing ->
                    findTiedUniqueEdge axis maxProj buffer restGroups

        [] ->
            Nothing


findTiedEdgeInGroup : Vec3 -> Float -> VertexBuffer -> List Int -> Maybe ( Vec3, Vec3 )
findTiedEdgeInGroup axis maxProj buffer edges =
    case edges of
        i1 :: i2 :: rest ->
            let
                v1 =
                    VertexBuffer.get i1 buffer

                v2 =
                    VertexBuffer.get i2 buffer
            in
            if (maxProj - Vec3.dot v1 axis - supportTieTolerance < 0) && (maxProj - Vec3.dot v2 axis - supportTieTolerance < 0) then
                Just ( v1, v2 )

            else
                findTiedEdgeInGroup axis maxProj buffer rest

        _ ->
            Nothing


{-| Returns the overlap depth along the given axis, or Nothing if the axis
separates the capsule from the convex.
-}
testCapsuleConvexAxis : Capsule -> Convex -> Vec3 -> Maybe Float
testCapsuleConvexAxis { radius, halfLength, axis, position } convex n =
    let
        centerProj =
            Vec3.dot position n

        axisContrib =
            abs (Vec3.dot axis n) * halfLength

        capsuleMin =
            centerProj - axisContrib - radius

        capsuleMax =
            centerProj + axisContrib + radius

        p2 =
            ConvexConvex.projectConvex n convex

        d1 =
            capsuleMax - p2.min

        d2 =
            p2.max - capsuleMin
    in
    if d1 + Const.contactBreakingThreshold < 0 || d2 + Const.contactBreakingThreshold < 0 then
        Nothing

    else if d1 - d2 > 0 then
        Just d2

    else
        Just d1
