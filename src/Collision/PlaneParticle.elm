module Collision.PlaneParticle exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 exposing (Vec3)
import Shapes.Plane exposing (Plane)


addContacts : Int -> (Contact -> Contact) -> Plane -> Vec3 -> List Contact -> List Contact
addContacts shapeKey orderContact { position, normal } particlePosition contacts =
    let
        dot =
            ((particlePosition.x - position.x) * normal.x)
                + ((particlePosition.y - position.y) * normal.y)
                + ((particlePosition.z - position.z) * normal.z)
    in
    if dot - Const.contactBreakingThreshold < 0 then
        orderContact
            { shapeKey = shapeKey
            , featureKey = ContactId.simple
            , ni = normal
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
