module Collision.PlaneSphere exposing (addContacts)

import Internal.Contact exposing (Contact)
import Shapes.Plane exposing (Plane)
import Shapes.Sphere exposing (Sphere)


addContacts : (Contact -> Contact) -> Plane -> Sphere -> List Contact -> List Contact
addContacts orderContact { normal, position } sphere contacts =
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
    if dot <= 0 then
        orderContact
            { ni = normal
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
