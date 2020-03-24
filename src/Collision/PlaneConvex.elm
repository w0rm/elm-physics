module Collision.PlaneConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex)
import Internal.Plane exposing (Plane)
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts orderContact { position, normal } { vertices } contacts =
    List.foldl
        (\vertex currentContacts ->
            let
                dot =
                    position
                        |> Vec3.sub vertex
                        |> Vec3.dot normal
            in
            if dot <= 0 then
                orderContact
                    { ni = normal
                    , pi =
                        normal
                            |> Vec3.scale dot
                            |> Vec3.sub vertex
                    , pj = vertex
                    }
                    :: currentContacts

            else
                currentContacts
        )
        contacts
        vertices
