module Collision.SphereParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Sphere exposing (Sphere)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : (Contact -> Contact) -> Sphere -> Vec3 -> List Contact -> List Contact
addContacts orderContact { radius, position } center2 contacts =
    let
        center1 =
            position

        distance =
            Vec3.distance center2 center1 - radius

        normal =
            Vec3.direction center2 center1
    in
    if distance > 0 then
        contacts

    else
        orderContact
            { ni = normal
            , pi = Vec3.add center1 (Vec3.scale (radius - distance) normal)
            , pj = center2
            }
            :: contacts
