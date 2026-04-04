module Collision.PlaneConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 exposing (Vec3)
import Shapes.Convex exposing (Convex)
import Shapes.Plane exposing (Plane)


addContacts : String -> (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts idPrefix orderContact plane { vertices } contacts =
    addContactsHelp idPrefix
        orderContact
        plane.position
        plane.normal
        0
        vertices
        contacts


addContactsHelp : String -> (Contact -> Contact) -> Vec3 -> Vec3 -> Int -> List Vec3 -> List Contact -> List Contact
addContactsHelp idPrefix orderContact planePosition planeNormal vertexId vertices contacts =
    case vertices of
        vertex :: remainingVertices ->
            let
                dot =
                    ((vertex.x - planePosition.x) * planeNormal.x)
                        + ((vertex.y - planePosition.y) * planeNormal.y)
                        + ((vertex.z - planePosition.z) * planeNormal.z)
            in
            if dot <= 0 then
                addContactsHelp idPrefix
                    orderContact
                    planePosition
                    planeNormal
                    (vertexId + 1)
                    remainingVertices
                    (orderContact
                        { id = idPrefix ++ "-" ++ String.fromInt (vertexId + 1)
                        , ni = planeNormal
                        , pi =
                            { x = vertex.x - dot * planeNormal.x
                            , y = vertex.y - dot * planeNormal.y
                            , z = vertex.z - dot * planeNormal.z
                            }
                        , pj = vertex
                        }
                        :: contacts
                    )

            else
                addContactsHelp idPrefix
                    orderContact
                    planePosition
                    planeNormal
                    (vertexId + 1)
                    remainingVertices
                    contacts

        [] ->
            contacts
