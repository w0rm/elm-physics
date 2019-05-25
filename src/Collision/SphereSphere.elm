module Collision.SphereSphere exposing (addContacts)

import Internal.Contact as Contact exposing (Contact)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : Transform -> Float -> Transform -> Float -> List Contact -> List Contact
addContacts t1 radius1 t2 radius2 contacts =
    let
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
        { ni = normal
        , pi = Vec3.add center1 (Vec3.scale (radius1 - distance) normal)
        , pj = Vec3.add center2 (Vec3.scale -radius2 normal)
        }
            :: contacts
