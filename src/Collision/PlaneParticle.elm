module Collision.PlaneParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 exposing (Vec3)
import Shapes.Plane exposing (Plane)


addContacts : (Contact -> Contact) -> Plane -> Vec3 -> List Contact -> List Contact
addContacts orderContact { position, normal } particlePosition contacts =
    let
        dot =
            ((particlePosition.x - position.x) * normal.x)
                + ((particlePosition.y - position.y) * normal.y)
                + ((particlePosition.z - position.z) * normal.z)
    in
    if dot <= 0 then
        orderContact
            { ni = normal
            , pi =
                { x = particlePosition.x - dot * normal.x
                , y = particlePosition.y - dot * normal.y
                , z = particlePosition.z - dot * normal.z
                }
            , pj = particlePosition
            }
            :: contacts

    else
        contacts
