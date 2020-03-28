module Collision.PlaneParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Plane exposing (Plane)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : (Contact -> Contact) -> Plane -> Vec3 -> List Contact -> List Contact
addContacts orderContact plane particlePosition contacts =
    let
        dot =
            plane.position
                |> Vec3.sub particlePosition
                |> Vec3.dot plane.normal
    in
    if dot <= 0 then
        orderContact
            { ni = plane.normal
            , pi = Vec3.sub particlePosition (Vec3.scale dot plane.normal)
            , pj = particlePosition
            }
            :: contacts

    else
        contacts
