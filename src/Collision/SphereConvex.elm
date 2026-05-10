module Collision.SphereConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex exposing (Convex, Face)
import Shapes.Sphere exposing (Sphere)


{-| Generate contacts between a sphere (body 1) and a convex (body 2).
Contact id suffix: `-fF` (face F, matching `ConvexConvex.bestFace`),
`-e` (edge interior), `-v` (vertex).
-}
addContacts : String -> (Contact -> Contact) -> Sphere -> Convex -> List Contact -> List Contact
addContacts idPrefix orderContact { radius, position } { faces } contacts =
    case faces of
        ( primary, partner ) :: rest ->
            walkFaces idPrefix orderContact position radius primary 1 partner rest [] contacts

        [] ->
            -- Degenerate convex with no faces: nothing for the
            -- boundary pass to walk either, so no contact possible.
            contacts


walkFaces : String -> (Contact -> Contact) -> Vec3 -> Float -> Face -> Int -> Maybe Face -> List ( Face, Maybe Face ) -> List ( Vec3, Vec3 ) -> List Contact -> List Contact
walkFaces idPrefix orderContact center radius currentFace currentFaceId nextFace queuedGroups candidateEdges contacts =
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
            case nextFace of
                Just face ->
                    walkFaces idPrefix orderContact center radius face (currentFaceId + 1) Nothing queuedGroups newCandidateEdges contacts

                Nothing ->
                    case queuedGroups of
                        ( primary, partner ) :: restGroups ->
                            walkFaces idPrefix orderContact center radius primary (currentFaceId + 1) partner restGroups newCandidateEdges contacts

                        [] ->
                            walkBoundaries idPrefix orderContact center radius newCandidateEdges Vec3.zero (radius * radius) contacts

        else
            -- Sphere centre projects inside the face polygon → face contact.
            emitContact idPrefix
                orderContact
                ("-f" ++ String.fromInt currentFaceId)
                center
                { x = center.x - faceDistance * currentFace.normal.x
                , y = center.y - faceDistance * currentFace.normal.y
                , z = center.z - faceDistance * currentFace.normal.z
                }
                (radius - faceDistance)
                contacts

    else
        case nextFace of
            Just face ->
                walkFaces idPrefix orderContact center radius face (currentFaceId + 1) Nothing queuedGroups candidateEdges contacts

            Nothing ->
                case queuedGroups of
                    ( primary, partner ) :: restGroups ->
                        walkFaces idPrefix orderContact center radius primary (currentFaceId + 1) partner restGroups candidateEdges contacts

                    [] ->
                        walkBoundaries idPrefix orderContact center radius candidateEdges Vec3.zero (radius * radius) contacts


{-| Returns the edges that separate the sphere centre from the face polygon
(prepended to `candidateEdges`) and whether any was found. `False` means
the centre projects inside the polygon → face contact.
-}
classifyAndCollectEdges : Vec3 -> Vec3 -> List Vec3 -> List ( Vec3, Vec3 ) -> ( Bool, List ( Vec3, Vec3 ) )
classifyAndCollectEdges center normal vertices candidateEdges =
    case vertices of
        first :: _ :: _ ->
            classifyAndCollectEdgesHelp center normal first vertices candidateEdges False

        _ ->
            -- Degenerate face (< 2 vertices): no edges to test → face contact.
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

                -- For CCW winding + outward normal, `normal × edge` points
                -- inward, so `d > 0` means the centre is outside this edge.
                d =
                    cnX * (v1.x - center.x) + cnY * (v1.y - center.y) + cnZ * (v1.z - center.z)
            in
            if d > 0 then
                classifyAndCollectEdgesHelp center normal firstVertex rest1 (( v1, v2 ) :: candidateEdges) True

            else
                classifyAndCollectEdgesHelp center normal firstVertex rest1 candidateEdges anyOutside

        [] ->
            ( anyOutside, candidateEdges )


{-| Tracks the closest feature (vertex or edge interior) to the sphere
centre. Short-circuits on the first edge-interior contact closer than
the current best; at the end, emits a vertex contact if `bestDistSq < radius²`.
-}
walkBoundaries : String -> (Contact -> Contact) -> Vec3 -> Float -> List ( Vec3, Vec3 ) -> Vec3 -> Float -> List Contact -> List Contact
walkBoundaries idPrefix orderContact center radius edges bestPoint bestDistSq contacts =
    case edges of
        [] ->
            if bestDistSq - radius * radius < 0 then
                emitContact idPrefix orderContact "-v" center bestPoint (radius - sqrt bestDistSq) contacts

            else
                contacts

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
                    walkBoundaries idPrefix orderContact center radius rest prevVertex distSq contacts

                else
                    walkBoundaries idPrefix orderContact center radius rest bestPoint bestDistSq contacts

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
                    walkBoundaries idPrefix orderContact center radius rest vertex distSq contacts

                else
                    walkBoundaries idPrefix orderContact center radius rest bestPoint bestDistSq contacts

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
                    emitContact idPrefix
                        orderContact
                        "-e"
                        center
                        { x = contactX, y = contactY, z = contactZ }
                        (radius - sqrt distSq)
                        contacts

                else
                    walkBoundaries idPrefix orderContact center radius rest bestPoint bestDistSq contacts


emitContact : String -> (Contact -> Contact) -> String -> Vec3 -> Vec3 -> Float -> List Contact -> List Contact
emitContact idPrefix orderContact featureTag center contactPoint penetration contacts =
    let
        normal =
            Vec3.direction contactPoint center
    in
    orderContact
        { id = idPrefix ++ featureTag
        , ni = normal
        , pi =
            { x = contactPoint.x + penetration * normal.x
            , y = contactPoint.y + penetration * normal.y
            , z = contactPoint.z + penetration * normal.z
            }
        , pj = contactPoint
        }
        :: contacts
