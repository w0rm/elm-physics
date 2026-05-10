module Collision.CapsuleConvex exposing (addContacts, supportFeature)

import Collision.ConvexConvex as ConvexConvex
import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Capsule exposing (Capsule)
import Shapes.Convex exposing (Convex, Face)


{-| `|dot(capsule.axis, separatingAxis)|` below this is treated as
perpendicular: the cylinder body is the contact feature, not a cap.
Wider than `Const.precision` so a slight tilt still snaps to the
two-contact face-clip path — keeps a near-flat capsule resting on a
face stable instead of chattering on a single end-cap contact.
-}
perpendicularThreshold : Float
perpendicularThreshold =
    1.0e-3


{-| `dot(face.normal, separatingAxis)` above this means the SAT axis
IS the face normal: do the Sutherland-Hodgman face clip. Otherwise
the SAT axis came from an edge or vertex, so use edge/vertex support
instead.
-}
faceAlignedThreshold : Float
faceAlignedThreshold =
    0.999


{-| Generate contacts between a capsule (body 1) and a convex (body 2).

Outline of the algorithm:

1.  SAT to find the minimum-overlap separating axis. The axis is oriented
    so it points FROM the convex TOWARD the capsule.

2.  Pick the contact-generation strategy from how the capsule axis
    relates to the separating axis:
      - `t > 0` : capsule axis aligned with the separating axis,
        the `ep1` end is deepest. Single contact at `ep1`.

      - `t < 0` : opposite alignment, `ep2` is deepest.
        Single contact at `ep2`.

      - `t ≈ 0` : capsule axis is roughly perpendicular to the
        separating axis. The capsule cylinder is what touches the
        convex. Pick the convex face most facing the capsule:
          - face normal aligns with the separating axis (Face Support):
            Sutherland-Hodgman clip the capsule segment against the
            face's adjacent edge planes; emit one contact per surviving
            endpoint. If the clip returns `Nothing` (capsule projects
            entirely outside the face polygon — happens when SAT picked
            a face whose extent doesn't actually contain the contact,
            e.g. capsule axis parallel to a face edge), fall back to
            finding the closest face EDGE to the capsule segment and
            emit a contact derived from the closest-points geometry.
          - face normal does NOT align (Edge/Vertex Support): the SAT
            minimum came from an axis derived from a convex edge or
            vertex. Find the convex's support feature in the
            +separatingAxis direction (1+ vertices at maximum
            projection) and emit one contact at the closest point
            between the capsule axis and that feature.

Contact convention (matches `SphereConvex.addContacts`):

  - `ni` points OUT OF body 1 (capsule) at the contact, i.e.
    `Vec3.negate separatingAxis`.
  - `pi` is on the capsule surface (the deepest cap-sphere point along
    `ni`); `pj` is the corresponding point on the convex surface, with
    `pj - pi = -penetration * ni`.

Contact id scheme (suffix appended to `idPrefix`):

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

  - `e*` = endpoint (cap) contact, `c*` = cylinder body contact.
  - Numeric suffix `1`/`2` identifies which side of the capsule (1 = ep1
    side, 2 = ep2 side) and only appears when a sibling contact exists.
  - Bare `c` / `e1` / `e2` = single contact in that path.
  - Face-anchored contacts include `-fF` (1-based face index from
    `ConvexConvex.bestFace`); non-face-anchored contacts use `-e` / `-v`
    to identify the supporting feature on the convex.

The id is consumed by the solver as a warm-starting key, so the suffix
encodes which feature each side of the capsule is touching — slides
across face/edge/vertex transitions invalidate the cache cleanly.

-}
addContacts : String -> (Contact -> Contact) -> Capsule -> Convex -> List Contact -> List Contact
addContacts idPrefix orderContact capsule convex contacts =
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

                        -- Pick the convex face whose outward normal aligns
                        -- with separatingAxis (the face "most facing" the
                        -- capsule). Returns Just only if the SAT axis IS a
                        -- face normal — for non-face axes we treat the
                        -- contact as edge/vertex-anchored and emit a
                        -- different id namespace.
                        faceContext =
                            let
                                ( fid, f ) =
                                    ConvexConvex.bestFace convex.faces (Vec3.negate separatingAxis)
                            in
                            if fid == -1 then
                                Nothing

                            else if Vec3.dot f.normal separatingAxis > faceAlignedThreshold then
                                Just ( fid, f )

                            else
                                Nothing

                        -- For non-face SAT axes, the convex's support
                        -- feature along the axis is either an edge (2
                        -- supporting vertices) or a single vertex. Used
                        -- both as the contact point in the perpendicular
                        -- branch AND as a feature tag on cap contacts.
                        supportVerts =
                            case faceContext of
                                Just _ ->
                                    []

                                Nothing ->
                                    supportFeature separatingAxis convex

                        -- Suffix encoding which feature of the convex was
                        -- contacted: face index for face-aligned SAT axes,
                        -- "-edge"/"-vert" for cross-product axes.
                        featureTag =
                            case faceContext of
                                Just ( fid, _ ) ->
                                    "-f" ++ String.fromInt fid

                                Nothing ->
                                    case supportVerts of
                                        _ :: _ :: _ ->
                                            "-e"

                                        _ :: [] ->
                                            "-v"

                                        [] ->
                                            ""
                    in
                    if t > perpendicularThreshold then
                        -- Cap-on-feature: ep1 is the deepest cap.
                        -- Also pick up cylinder-body contacts where the body
                        -- crosses the SAT-aligned face: a tilted capsule on a
                        -- box can have both the cap and a length of cylinder
                        -- penetrating, and elm-physics has no multi-frame
                        -- manifold accumulation, so we need to emit them now.
                        contacts
                            |> addDirectContact (idPrefix ++ "-e1" ++ featureTag) orderContact ep1 separatingAxis penetration capsule
                            |> addBodyContacts idPrefix featureTag orderContact faceContext capsule ep1 ep2 separatingAxis ep1

                    else if t < -perpendicularThreshold then
                        -- Cap-on-feature: ep2 is the deepest cap.
                        contacts
                            |> addDirectContact (idPrefix ++ "-e2" ++ featureTag) orderContact ep2 separatingAxis penetration capsule
                            |> addBodyContacts idPrefix featureTag orderContact faceContext capsule ep1 ep2 separatingAxis ep2

                    else
                        -- Cylinder body touches the convex.
                        case faceContext of
                            Just ( _, face ) ->
                                -- Face Support: capsule cylinder is parallel
                                -- to a convex face. Clip the capsule segment
                                -- against the face's adjacent edge planes
                                -- (Sutherland-Hodgman); emit one contact per
                                -- surviving endpoint.
                                case clipSegmentAgainstFace face ep1 ep2 of
                                    ClipAlive p1 p2 ->
                                        contacts
                                            |> addDirectContact (idPrefix ++ "-c1" ++ featureTag) orderContact p1 separatingAxis penetration capsule
                                            |> addDirectContact (idPrefix ++ "-c2" ++ featureTag) orderContact p2 separatingAxis penetration capsule

                                    ClipDead ->
                                        -- Capsule segment lies entirely
                                        -- outside the face polygon — the
                                        -- contact is on a face edge.
                                        -- Re-derive the contact normal and
                                        -- depth from the actual closest-
                                        -- points geometry (SAT's penetration
                                        -- overestimates here).
                                        addClosestEdgeContact (idPrefix ++ "-c" ++ featureTag) orderContact face ep1 ep2 capsule contacts

                            Nothing ->
                                -- Edge/Vertex support: SAT picked a non-face axis.
                                -- Emit one contact at the closest point on
                                -- the capsule axis to the support feature.
                                -- featureTag here is "-edge" (2 verts) or
                                -- "-vert" (1 vert) so the id reads e.g.
                                -- "test-c-edge" / "test-c-vert".
                                case supportVerts of
                                    v1 :: v2 :: _ ->
                                        if Vec3.almostZero (Vec3.cross capsule.axis (Vec3.sub v2 v1)) then
                                            -- Cylinder body and supporting edge are
                                            -- parallel: the contact is a line
                                            -- segment, not a point. Emit one
                                            -- contact at each end of the overlap
                                            -- (mirrors perpendicular face support's
                                            -- two clip-endpoint contacts).
                                            addParallelEdgeContacts (idPrefix ++ "-c") featureTag orderContact ep1 ep2 v1 v2 separatingAxis penetration capsule contacts

                                        else
                                            -- Skew segments: closest-pair geometry
                                            -- gives a single contact.
                                            let
                                                ( pCapsule, _ ) =
                                                    closestPointsBetweenSegments ep1 ep2 v1 v2
                                            in
                                            addDirectContact (idPrefix ++ "-c" ++ featureTag) orderContact pCapsule separatingAxis penetration capsule contacts

                                    v :: [] ->
                                        let
                                            ( pCapsule, _ ) =
                                                closestPointsBetweenSegments ep1 ep2 v v
                                        in
                                        addDirectContact (idPrefix ++ "-c" ++ featureTag) orderContact pCapsule separatingAxis penetration capsule contacts

                                    [] ->
                                        contacts


{-| Emit two contacts for a cylinder-body / convex-edge configuration where
both segments lie on parallel lines. Project both onto `capsule.axis` and
take the overlap interval `[sLo, sHi]`; each end of the interval becomes a
contact at the corresponding capsule-axis point. If the overlap collapses
to a (numerically) single point, fall back to a one-contact closest-pair
emit so we still produce something.
-}
addParallelEdgeContacts : String -> String -> (Contact -> Contact) -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Capsule -> List Contact -> List Contact
addParallelEdgeContacts idPrefix featureTag orderContact ep1 ep2 v1 v2 separatingAxis penetration capsule contacts =
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
    if sHi - sLo > Const.precision then
        contacts
            |> addDirectContact (idPrefix ++ "1" ++ featureTag) orderContact (pointAt sLo) separatingAxis penetration capsule
            |> addDirectContact (idPrefix ++ "2" ++ featureTag) orderContact (pointAt sHi) separatingAxis penetration capsule

    else
        let
            ( pCapsule, _ ) =
                closestPointsBetweenSegments ep1 ep2 v1 v2
        in
        addDirectContact (idPrefix ++ featureTag) orderContact pCapsule separatingAxis penetration capsule contacts


addDirectContact : String -> (Contact -> Contact) -> Vec3 -> Vec3 -> Float -> Capsule -> List Contact -> List Contact
addDirectContact idPrefix orderContact segmentPoint separatingAxis penetration capsule contacts =
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
        { id = idPrefix
        , ni = normal
        , pi = pi
        , pj = pj
        }
        :: contacts


{-| When the cap-on-feature branch fires, the cylinder body may also be
penetrating the SAT-aligned face elsewhere along its length. Run the
same Sutherland-Hodgman clip used by the perpendicular branch on the
capsule axis, recompute per-point penetration against the face plane,
and emit body contacts at clipped points whose cylinder surface lies
below the face plane. The cap endpoint is skipped to avoid emitting
two contacts with different depths at the same world point.

Skipped entirely when SAT picked a non-face axis (`faceContext = Nothing`):
no face polygon to clip against in that case.

-}
addBodyContacts : String -> String -> (Contact -> Contact) -> Maybe ( Int, Face ) -> Capsule -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Contact -> List Contact
addBodyContacts idPrefix faceTag orderContact faceContext capsule ep1 ep2 separatingAxis capPoint contacts =
    case faceContext of
        Nothing ->
            contacts

        Just ( _, face ) ->
            case clipSegmentAgainstFace face ep1 ep2 of
                ClipDead ->
                    contacts

                ClipAlive q1 q2 ->
                    let
                        facePlaneConstant =
                            case face.vertices of
                                v :: _ ->
                                    -(Vec3.dot face.normal v)

                                [] ->
                                    0
                    in
                    contacts
                        |> tryAddBodyPoint (idPrefix ++ "-c1" ++ faceTag) orderContact face facePlaneConstant separatingAxis capsule capPoint q1
                        |> tryAddBodyPoint (idPrefix ++ "-c2" ++ faceTag) orderContact face facePlaneConstant separatingAxis capsule capPoint q2


tryAddBodyPoint : String -> (Contact -> Contact) -> Face -> Float -> Vec3 -> Capsule -> Vec3 -> Vec3 -> List Contact -> List Contact
tryAddBodyPoint idPrefix orderContact face facePlaneConstant separatingAxis capsule capPoint point contacts =
    if Vec3.distance point capPoint < Const.precision then
        contacts

    else
        let
            -- Signed distance from the face plane, positive on the +face.normal
            -- (outward) side. Cylinder surface in the contact direction sits at
            -- signedDist - radius, so penetration depth is `radius - signedDist`.
            signedDist =
                Vec3.dot face.normal point + facePlaneConstant

            bodyPen =
                capsule.radius - signedDist
        in
        if bodyPen <= 0 then
            contacts

        else
            addDirectContact idPrefix orderContact point separatingAxis bodyPen capsule contacts


type ClipResult
    = ClipAlive Vec3 Vec3
    | ClipDead


{-| Sutherland-Hodgman clip of a segment against a face's adjacent
edge planes. Each edge plane is built from `cross(face.normal, v1 - v2)`,
which (for the CCW face winding produced by `Convex.fromBlock` and
preserved by `Convex.placeIn`) points OUTWARD from the polygon. So
`d < 0` means a point projects INSIDE the polygon along that edge.
Returns `ClipAlive p1 p2` for the surviving segment, or `ClipDead`
if the segment lies entirely outside the face polygon.

Implemented as a single tail-recursive walker over face vertices to
avoid the per-edge `Maybe ( Vec3, Vec3 )` accumulator that threading
through `Convex.foldFaceEdges` would require — the dead state short-
circuits via direct return rather than as a propagated `Nothing`.

-}
clipSegmentAgainstFace : Face -> Vec3 -> Vec3 -> ClipResult
clipSegmentAgainstFace face ep1 ep2 =
    case face.vertices of
        first :: _ :: _ ->
            walkClipEdge face.normal first face.vertices ep1 ep2

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
                ClipDead

            else if d1 < 0 then
                walkClipEdge faceNormal firstVertex rest1 p1 (Vec3.lerp (d1 / (d1 - d2)) p1 p2)

            else
                walkClipEdge faceNormal firstVertex rest1 (Vec3.lerp (d1 / (d1 - d2)) p1 p2) p2

        [] ->
            ClipAlive p1 p2


{-| Iterate the face's edges, find the one closest to the capsule
segment, and emit a single contact derived from the actual closest-
points geometry. Used as a fallback when face-support clipping
discards the segment (which means the projected segment fell outside
the face polygon, so the contact is really on a face edge).

Skipped silently if no edge is within `capsule.radius` of the
capsule axis — that can happen when SAT picked an axis whose
overlap claim was geometrically too generous.

-}
addClosestEdgeContact : String -> (Contact -> Contact) -> Face -> Vec3 -> Vec3 -> Capsule -> List Contact -> List Contact
addClosestEdgeContact idPrefix orderContact face ep1 ep2 capsule contacts =
    let
        radiusSq =
            capsule.radius * capsule.radius

        result =
            closestEdgeToSegment face ep1 ep2 radiusSq
    in
    -- Sentinel: result.distSq == radiusSq means no edge improved the
    -- initial bound, so no edge is within the capsule radius.
    if result.distSq - radiusSq >= 0 then
        contacts

    else
        let
            distance =
                sqrt result.distSq
        in
        if distance - Const.precision <= 0 then
            -- Capsule axis intersects the edge — direction is undefined.
            -- Skip rather than emit a contact with a garbage normal.
            contacts

        else
            let
                -- separatingAxis convention: from convex toward capsule.
                inv =
                    1 / distance

                edgeSeparatingAxis =
                    { x = (result.pCapsule.x - result.pEdge.x) * inv
                    , y = (result.pCapsule.y - result.pEdge.y) * inv
                    , z = (result.pCapsule.z - result.pEdge.z) * inv
                    }
            in
            addDirectContact idPrefix
                orderContact
                result.pCapsule
                edgeSeparatingAxis
                (capsule.radius - distance)
                capsule
                contacts


{-| Walks `face.vertices` as consecutive edges and returns the closest
edge to the capsule axis segment as `(pCapsule, pEdge, distSq)`. Uses
`maxDistSq` as both the starting upper bound and the "no edge found"
sentinel — if no edge improves it, the caller treats the result as
"no contact within `sqrt maxDistSq`". Squared distances throughout to
defer the per-edge `sqrt`.
-}
closestEdgeToSegment : Face -> Vec3 -> Vec3 -> Float -> { pCapsule : Vec3, pEdge : Vec3, distSq : Float }
closestEdgeToSegment face ep1 ep2 maxDistSq =
    case face.vertices of
        first :: _ :: _ ->
            walkClosestEdge ep1 ep2 first face.vertices Vec3.zero Vec3.zero maxDistSq

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
                    closestPointsBetweenSegments ep1 ep2 v1 v2

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


{-| Closest points between segments p1-q1 and p2-q2, returned as
`(pointOnFirst, pointOnSecond)`. Adapted from Christer Ericson's
"Real-Time Collision Detection".
-}
closestPointsBetweenSegments : Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( Vec3, Vec3 )
closestPointsBetweenSegments p1 q1 p2 q2 =
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

        ( s, t ) =
            if a <= Const.precision && e <= Const.precision then
                ( 0, 0 )

            else if a <= Const.precision then
                ( 0, clamp 0 1 (f / e) )

            else
                let
                    c =
                        Vec3.dot d1 r
                in
                if e <= Const.precision then
                    ( clamp 0 1 (-c / a), 0 )

                else
                    let
                        b =
                            Vec3.dot d1 d2

                        denom =
                            a * e - b * b

                        sInit =
                            if denom /= 0.0 then
                                clamp 0 1 ((b * f - c * e) / denom)

                            else
                                0.0

                        tNom =
                            b * sInit + f
                    in
                    if tNom < 0.0 then
                        ( clamp 0 1 (-c / a), 0 )

                    else if tNom > e then
                        ( clamp 0 1 ((b - c) / a), 1 )

                    else
                        ( sInit, tNom / e )
    in
    ( Vec3.add p1 (Vec3.scale s d1)
    , Vec3.add p2 (Vec3.scale t d2)
    )


findSeparatingAxis : Capsule -> Convex -> Vec3 -> Vec3 -> Maybe Vec3
findSeparatingAxis capsule convex ep1 ep2 =
    testConvexNormals capsule convex ep1 ep2 convex.faces Vec3.zero Const.maxNumber


testConvexNormals : Capsule -> Convex -> Vec3 -> Vec3 -> List ( Face, Maybe Face ) -> Vec3 -> Float -> Maybe Vec3
testConvexNormals capsule convex ep1 ep2 groups target dmin =
    case groups of
        [] ->
            testUniqueEdges capsule convex ep1 ep2 convex.uniqueEdges target dmin

        ( { normal }, _ ) :: restGroups ->
            case testCapsuleConvexAxis capsule convex normal of
                Nothing ->
                    Nothing

                Just dist ->
                    if dist - dmin < 0 then
                        testConvexNormals capsule convex ep1 ep2 restGroups normal dist

                    else
                        testConvexNormals capsule convex ep1 ep2 restGroups target dmin


{-| For each unique convex edge, derive a SAT axis from the closest
points between the capsule axis segment and that edge, then test it.
This subsumes the cross-product test used in standard convex SAT and
additionally covers the configuration-dependent axes capsule-vs-convex
needs:

  - convex vertex vs cap sphere (axis = vertex → cap centre)
  - convex vertex vs cylinder (axis = vertex → closest pt on capsule axis)
  - convex edge vs cap sphere (axis = edge → cap centre, perpendicular
    to edge)

Without these axes SAT can falsely report overlap when the cap sphere
is just outside an edge or corner of the convex.

When the segments meet at a point (closest-pair distance is zero —
penetration through the edge) the closest-pair direction is undefined,
so fall back to the cross-product axis. If the cross product is also
degenerate (segments parallel) skip this edge.

Walks `convex.uniqueEdges` (each physical edge appears exactly once
across direction groups), avoiding the face-shared double-visit a walk
over `convex.faces` would incur.

-}
testUniqueEdges : Capsule -> Convex -> Vec3 -> Vec3 -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> Vec3 -> Float -> Maybe Vec3
testUniqueEdges capsule convex ep1 ep2 groups target dmin =
    walkUniqueEdges capsule convex ep1 ep2 [] groups target dmin


walkUniqueEdges : Capsule -> Convex -> Vec3 -> Vec3 -> List ( Vec3, Vec3 ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> Vec3 -> Float -> Maybe Vec3
walkUniqueEdges capsule convex ep1 ep2 edges queuedGroups target dmin =
    case edges of
        ( v1, v2 ) :: rest ->
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

        [] ->
            case queuedGroups of
                ( firstEdge, otherEdges ) :: restGroups ->
                    walkUniqueEdges capsule convex ep1 ep2 (firstEdge :: otherEdges) restGroups target dmin

                [] ->
                    -- Orient the axis to point FROM convex TOWARDS capsule, so the
                    -- contact normal `Vec3.negate target` points OUT of the capsule
                    -- at the contact (matches `SphereConvex` convention).
                    if Vec3.dot (Vec3.sub convex.position capsule.position) target > 0 then
                        Just (Vec3.negate target)

                    else
                        Just target


edgeAxis : Capsule -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
edgeAxis capsule ep1 ep2 v1 v2 =
    let
        ( pCap, pEdge ) =
            closestPointsBetweenSegments ep1 ep2 v1 v2

        diff =
            Vec3.sub pCap pEdge

        distSq =
            Vec3.lengthSquared diff
    in
    if distSq > Const.precision then
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


{-| Support feature of `convex` along `axis`: at most two vertices, both at
maximum projection within a 1.0e-4 tolerance, and (when two are returned)
guaranteed to form a real edge of the convex.

Algorithm:

  - Pass 1: max projection over vertices.
  - Pass 2: collect the first two vertices within tolerance of max.
  - If only one is found, return it (vertex support).
  - If two or more are tied, walk `convex.uniqueEdges` to find an edge
    whose both endpoints are within tolerance. Each physical edge is
    visited once (no face-shared double-visit).
  - If no such edge exists (truly degenerate: two tied vertices that aren't
    connected by an edge), fall back to the first tied vertex.

This avoids the earlier "first 2 vertices in `convex.vertices` order" bug
where the picked pair could be a face diagonal rather than an edge — the
contact location would then drift off the convex surface and corrupt the
contact's torque arm.

-}
supportFeature : Vec3 -> Convex -> List Vec3
supportFeature axis convex =
    let
        maxProj =
            maxProjection axis convex.vertices -Const.maxNumber
    in
    case collectFirstTwoTied axis maxProj convex.vertices 0 Vec3.zero of
        [] ->
            []

        [ v ] ->
            [ v ]

        firstTied :: _ ->
            case findTiedUniqueEdge axis maxProj convex.uniqueEdges of
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
            if maxProj - Vec3.dot v axis < 1.0e-4 then
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
findTiedUniqueEdge : Vec3 -> Float -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> Maybe ( Vec3, Vec3 )
findTiedUniqueEdge axis maxProj groups =
    case groups of
        ( firstEdge, otherEdges ) :: restGroups ->
            case findTiedEdgeInGroup axis maxProj (firstEdge :: otherEdges) of
                (Just _) as found ->
                    found

                Nothing ->
                    findTiedUniqueEdge axis maxProj restGroups

        [] ->
            Nothing


findTiedEdgeInGroup : Vec3 -> Float -> List ( Vec3, Vec3 ) -> Maybe ( Vec3, Vec3 )
findTiedEdgeInGroup axis maxProj edges =
    case edges of
        ( v1, v2 ) :: rest ->
            if (maxProj - Vec3.dot v1 axis < 1.0e-4) && (maxProj - Vec3.dot v2 axis < 1.0e-4) then
                Just ( v1, v2 )

            else
                findTiedEdgeInGroup axis maxProj rest

        [] ->
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
            ConvexConvex.project n Const.maxNumber -Const.maxNumber convex.vertices

        d1 =
            capsuleMax - p2.min

        d2 =
            p2.max - capsuleMin
    in
    if d1 < 0 || d2 < 0 then
        Nothing

    else if d1 - d2 > 0 then
        Just d2

    else
        Just d1
