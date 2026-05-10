module Collision.SphereConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex exposing (Convex, Face)
import Shapes.Sphere exposing (Sphere)


addContacts : String -> (Contact -> Contact) -> Sphere -> Convex -> List Contact -> List Contact
addContacts idPrefix orderContact { radius, position } hull2 contacts =
    let
        ( maybeContact, penetration ) =
            sphereContact position radius hull2
    in
    case maybeContact of
        Just contact2 ->
            let
                normal =
                    Vec3.direction contact2 position
            in
            orderContact
                { id = idPrefix
                , ni = normal
                , pi =
                    { x = contact2.x + penetration * normal.x
                    , y = contact2.y + penetration * normal.y
                    , z = contact2.z + penetration * normal.z
                    }
                , pj = contact2
                }
                :: contacts

        Nothing ->
            contacts


{-| Contact between a sphere (centred at `center`, with `radius`) and a
convex hull. Returns the contact point on the convex (in world frame)
plus the sphere's penetration depth past that point, or `Nothing` if
they don't intersect.

Algorithm — two passes over the convex's faces:

  - **Face pass.** For each face whose plane intersects the sphere
    (centre is on the outward side and within `radius` of the plane),
    walk the face's edges in world frame: an edge whose plane excludes
    the sphere centre from the polygon is a "separating" candidate
    edge for the boundary pass. If a face has _no_ separating edges,
    the centre projects inside the polygon — that's a face contact and
    we return immediately.

  - **Boundary pass.** Walk the accumulated separating edges, find the
    closest feature (vertex or edge interior) to the sphere centre.
    Short-circuits on the first edge-interior contact closer than the
    current best, matching the original's `listRecurseUntil` behaviour.

The previous implementation reframed every face's vertices into the
sphere's local frame (per-face `List Vec3` allocation) and threaded
state through `TestFaceResult` / `TestBoundaryResult` wrappers driven
by `listRecurseUntil`. This version works in world frame with inlined
component math — no per-vertex `Vec3` allocation — and uses direct
tail recursion with explicit accumulator args.

-}
sphereContact : Vec3 -> Float -> Convex -> ( Maybe Vec3, Float )
sphereContact center radius { faces } =
    case faces of
        ( primary, partner ) :: rest ->
            walkFaces center radius primary partner rest []

        [] ->
            walkBoundaries center radius [] Vec3.zero (radius * radius)


walkFaces : Vec3 -> Float -> Face -> Maybe Face -> List ( Face, Maybe Face ) -> List ( Vec3, Vec3 ) -> ( Maybe Vec3, Float )
walkFaces center radius currentFace nextFace queuedGroups candidateEdges =
    let
        faceDistance =
            case currentFace.vertices of
                first :: _ ->
                    currentFace.normal.x * (center.x - first.x) + currentFace.normal.y * (center.y - first.y) + currentFace.normal.z * (center.z - first.z)

                [] ->
                    -1
    in
    if faceDistance > 0 && faceDistance - radius < 0 then
        let
            ( anyOutside, newCandidateEdges ) =
                classifyAndCollectEdges center currentFace.normal currentFace.vertices candidateEdges
        in
        if anyOutside then
            -- Advance: face partially excludes sphere; move on to the
            -- antipodal partner / next group / boundary pass. Inlined
            -- (no `let advance = ...`) so the recursive `walkFaces`
            -- call sits in tail position for Elm's TCO — Elm doesn't
            -- TCO recursive calls made through let-bound helpers or
            -- mutually recursive pairs.
            case nextFace of
                Just face ->
                    walkFaces center radius face Nothing queuedGroups newCandidateEdges

                Nothing ->
                    case queuedGroups of
                        ( primary, partner ) :: restGroups ->
                            walkFaces center radius primary partner restGroups newCandidateEdges

                        [] ->
                            walkBoundaries center radius newCandidateEdges Vec3.zero (radius * radius)

        else
            -- Sphere centre projects inside the face polygon → face contact.
            ( Just
                { x = center.x - faceDistance * currentFace.normal.x
                , y = center.y - faceDistance * currentFace.normal.y
                , z = center.z - faceDistance * currentFace.normal.z
                }
            , radius - faceDistance
            )

    else
        case nextFace of
            Just face ->
                walkFaces center radius face Nothing queuedGroups candidateEdges

            Nothing ->
                case queuedGroups of
                    ( primary, partner ) :: restGroups ->
                        walkFaces center radius primary partner restGroups candidateEdges

                    [] ->
                        walkBoundaries center radius candidateEdges Vec3.zero (radius * radius)


{-| Walks face vertices as consecutive edges. For each edge, an edge whose
plane excludes the sphere centre is "separating" — the centre is on the
_outside_ of that edge within the face plane. Such edges are prepended
onto the candidate list for the boundary pass. The returned `Bool`
indicates whether any separating edge was found (`False` means centre
projects inside the polygon → face contact).

Inlines the cross-product `normal × edge` and the
`(prevVertex - center) · (normal × edge)` test as component math, so
no `Vec3` is allocated per edge.

-}
classifyAndCollectEdges : Vec3 -> Vec3 -> List Vec3 -> List ( Vec3, Vec3 ) -> ( Bool, List ( Vec3, Vec3 ) )
classifyAndCollectEdges center normal vertices candidateEdges =
    case vertices of
        first :: _ :: _ ->
            classifyAndCollectEdgesHelp center normal first vertices candidateEdges False

        _ ->
            -- Degenerate face (< 2 vertices): no edges to test, no separating edges.
            -- Matches original behaviour where empty `originProjection` triggers a face contact.
            ( False, candidateEdges )


classifyAndCollectEdgesHelp : Vec3 -> Vec3 -> Vec3 -> List Vec3 -> List ( Vec3, Vec3 ) -> Bool -> ( Bool, List ( Vec3, Vec3 ) )
classifyAndCollectEdgesHelp center normal firstVertex vertices candidateEdges anyOutside =
    case vertices of
        v1 :: rest1 ->
            let
                v2 =
                    case rest1 of
                        [] ->
                            firstVertex

                        next :: _ ->
                            next

                edgeX =
                    v2.x - v1.x

                edgeY =
                    v2.y - v1.y

                edgeZ =
                    v2.z - v1.z

                -- normal × edge
                cnX =
                    normal.y * edgeZ - normal.z * edgeY

                cnY =
                    normal.z * edgeX - normal.x * edgeZ

                cnZ =
                    normal.x * edgeY - normal.y * edgeX

                -- (v1 - center) · (normal × edge). For CCW face winding with
                -- outward `normal`, `normal × edge` points toward the polygon
                -- interior, so a positive dot means `v1 - center` has a
                -- component pointing inward → the centre is on the *outside*
                -- of this edge plane → separating edge.
                d =
                    cnX * (v1.x - center.x) + cnY * (v1.y - center.y) + cnZ * (v1.z - center.z)
            in
            if d > 0 then
                classifyAndCollectEdgesHelp center normal firstVertex rest1 (( v1, v2 ) :: candidateEdges) True

            else
                classifyAndCollectEdgesHelp center normal firstVertex rest1 candidateEdges anyOutside

        [] ->
            ( anyOutside, candidateEdges )


{-| Walks the candidate edges, tracking the closest feature (vertex or
edge interior) to the sphere centre. Returns directly on the first
edge-interior contact closer than the current best (matches the
`listRecurseUntil isAnEdgeContact` short-circuit in the original).
At the end, if `bestDistSq < radius²` a vertex contact is returned;
otherwise no contact.
-}
walkBoundaries : Vec3 -> Float -> List ( Vec3, Vec3 ) -> Vec3 -> Float -> ( Maybe Vec3, Float )
walkBoundaries center radius edges bestPoint bestDistSq =
    case edges of
        [] ->
            if bestDistSq - radius * radius < 0 then
                ( Just bestPoint, radius - sqrt bestDistSq )

            else
                ( Nothing, 0 )

        ( prevVertex, vertex ) :: rest ->
            let
                edgeX =
                    vertex.x - prevVertex.x

                edgeY =
                    vertex.y - prevVertex.y

                edgeZ =
                    vertex.z - prevVertex.z

                edgeLenSq =
                    edgeX * edgeX + edgeY * edgeY + edgeZ * edgeZ

                -- (center - prevVertex) · edge
                offsetTimesLen =
                    (center.x - prevVertex.x) * edgeX + (center.y - prevVertex.y) * edgeY + (center.z - prevVertex.z) * edgeZ
            in
            if offsetTimesLen < 0 then
                let
                    dx =
                        prevVertex.x - center.x

                    dy =
                        prevVertex.y - center.y

                    dz =
                        prevVertex.z - center.z

                    distSq =
                        dx * dx + dy * dy + dz * dz
                in
                if distSq - bestDistSq < 0 then
                    walkBoundaries center radius rest prevVertex distSq

                else
                    walkBoundaries center radius rest bestPoint bestDistSq

            else if offsetTimesLen - edgeLenSq > 0 then
                let
                    dx =
                        vertex.x - center.x

                    dy =
                        vertex.y - center.y

                    dz =
                        vertex.z - center.z

                    distSq =
                        dx * dx + dy * dy + dz * dz
                in
                if distSq - bestDistSq < 0 then
                    walkBoundaries center radius rest vertex distSq

                else
                    walkBoundaries center radius rest bestPoint bestDistSq

            else
                let
                    fraction =
                        offsetTimesLen / edgeLenSq

                    contactX =
                        prevVertex.x + fraction * edgeX

                    contactY =
                        prevVertex.y + fraction * edgeY

                    contactZ =
                        prevVertex.z + fraction * edgeZ

                    dx =
                        contactX - center.x

                    dy =
                        contactY - center.y

                    dz =
                        contactZ - center.z

                    distSq =
                        dx * dx + dy * dy + dz * dz
                in
                if distSq - bestDistSq < 0 then
                    -- Edge-interior contact closer than current best — short-circuit.
                    ( Just { x = contactX, y = contactY, z = contactZ }
                    , radius - sqrt distSq
                    )

                else
                    walkBoundaries center radius rest bestPoint bestDistSq
