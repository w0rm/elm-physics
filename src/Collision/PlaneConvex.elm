module Collision.PlaneConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex)
import Internal.Plane exposing (Plane)


addContacts : (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts orderContact { position, normal } { vertices } contacts =
    List.foldl
        (\vertex currentContacts ->
            let
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
                    :: currentContacts

            else
                currentContacts
        )
        contacts
        vertices
