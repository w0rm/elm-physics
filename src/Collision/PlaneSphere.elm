module Collision.PlaneSphere exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Shapes.Plane exposing (Plane)
import Shapes.Sphere exposing (Sphere)


addContacts : Int -> (Contact -> Contact) -> Plane -> Sphere -> List Contact -> List Contact
addContacts shapeKey orderContact { normal, position } sphere contacts =
    let
        { x, y, z } =
            sphere.position

        vertex =
            { x = x - sphere.radius * normal.x
            , y = y - sphere.radius * normal.y
            , z = z - sphere.radius * normal.z
            }

        dot =
            ((vertex.x - position.x) * normal.x)
                + ((vertex.y - position.y) * normal.y)
                + ((vertex.z - position.z) * normal.z)
    in
    if dot - Const.contactBreakingThreshold < 0 then
        orderContact
            { shapeKey = shapeKey
            , featureKey = ContactId.simple
            , ni = normal
            , pi =
                { x = vertex.x - dot * normal.x
                , y = vertex.y - dot * normal.y
                , z = vertex.z - dot * normal.z
                }
            , pj = vertex
            }
            :: contacts

    else
        contacts
