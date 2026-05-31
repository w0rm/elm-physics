module Collision.PlaneConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 exposing (Vec3)
import Shapes.Convex exposing (Convex)
import Shapes.Plane exposing (Plane)


addContacts : Int -> (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts shapeKey orderContact plane { vertices } contacts =
    addContactsHelp shapeKey
        orderContact
        plane.position
        plane.normal
        0
        vertices
        contacts


addContactsHelp : Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> Int -> List Vec3 -> List Contact -> List Contact
addContactsHelp shapeKey orderContact planePosition planeNormal vertexId vertices contacts =
    case vertices of
        vertex :: remainingVertices ->
            let
                dot =
                    ((vertex.x - planePosition.x) * planeNormal.x)
                        + ((vertex.y - planePosition.y) * planeNormal.y)
                        + ((vertex.z - planePosition.z) * planeNormal.z)
            in
            if dot - Const.contactBreakingThreshold < 0 then
                addContactsHelp shapeKey
                    orderContact
                    planePosition
                    planeNormal
                    (vertexId + 1)
                    remainingVertices
                    (orderContact
                        { shapeKey = shapeKey
                        , featureKey = ContactId.planeVertex (vertexId + 1)
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
                addContactsHelp shapeKey
                    orderContact
                    planePosition
                    planeNormal
                    (vertexId + 1)
                    remainingVertices
                    contacts

        [] ->
            contacts
