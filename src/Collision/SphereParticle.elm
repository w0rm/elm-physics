module Collision.SphereParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> Transform -> Float -> Transform -> List Contact -> List Contact
addContacts orderContact t1 radius1 t2 contacts =
    let
        radius2 =
            0

        center1 =
            Transform.pointToWorldFrame t1 Vec3.zero

        center2 =
            Transform.pointToWorldFrame t2 Vec3.zero

        distance =
            Vec3.distance center2 center1
                - radius1
                - radius2

        normal =
            Vec3.direction center2 center1
    in
    if distance > 0 then
        contacts

    else
        orderContact
            { ni = normal
            , pi = Vec3.add center1 (Vec3.scale (radius1 - distance) normal)
            , pj = Vec3.add center2 (Vec3.scale -radius2 normal)
            }
            :: contacts
