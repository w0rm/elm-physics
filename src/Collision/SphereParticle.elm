module Collision.SphereParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Sphere exposing (Sphere)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : (Contact -> Contact) -> Sphere -> Vec3 -> List Contact -> List Contact
addContacts orderContact { radius, position } particlePosition contacts =
    let
        distance =
            Vec3.distance particlePosition position - radius

        normal =
            Vec3.direction particlePosition position
    in
    if distance > 0 then
        contacts

    else
        orderContact
            { ni = normal
            , pi = Vec3.add position (Vec3.scale (radius - distance) normal)
            , pj = particlePosition
            }
            :: contacts
