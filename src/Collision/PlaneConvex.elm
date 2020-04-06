module Collision.PlaneConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 exposing (Vec3)
import Shapes.Convex exposing (Convex)
import Shapes.Plane exposing (Plane)


addContacts : (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts orderContact plane { vertices } contacts =
    addContactsHelp
        orderContact
        plane.position
        plane.normal
        vertices
        contacts


addContactsHelp : (Contact -> Contact) -> Vec3 -> Vec3 -> List Vec3 -> List Contact -> List Contact
addContactsHelp orderContact planePosition planeNormal vertices contacts =
    case vertices of
        vertex :: remainingVertices ->
            let
                dot =
                    ((vertex.x - planePosition.x) * planeNormal.x)
                        + ((vertex.y - planePosition.y) * planeNormal.y)
                        + ((vertex.z - planePosition.z) * planeNormal.z)
            in
            if dot <= 0 then
                addContactsHelp
                    orderContact
                    planePosition
                    planeNormal
                    remainingVertices
                    (orderContact
                        { ni = planeNormal
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
                addContactsHelp
                    orderContact
                    planePosition
                    planeNormal
                    remainingVertices
                    contacts

        [] ->
            contacts
