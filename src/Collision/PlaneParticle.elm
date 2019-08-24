module Collision.PlaneParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Quaternion as Quaternion
import Internal.Transform exposing (Transform)
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> Transform -> Transform -> List Contact -> List Contact
addContacts orderContact planeTransform t2 contacts =
    let
        worldPlaneNormal =
            Quaternion.rotate planeTransform.orientation Vec3.k

        worldVertex =
            t2.position

        dot =
            planeTransform.position
                |> Vec3.sub worldVertex
                |> Vec3.dot worldPlaneNormal
    in
    if dot <= 0 then
        orderContact
            { ni = worldPlaneNormal
            , pi = Vec3.sub worldVertex (Vec3.scale dot worldPlaneNormal)
            , pj = worldVertex
            }
            :: contacts

    else
        contacts
