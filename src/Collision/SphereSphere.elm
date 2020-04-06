module Collision.SphereSphere exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3
import Shapes.Sphere exposing (Sphere)


addContacts : Sphere -> Sphere -> List Contact -> List Contact
addContacts sphere1 sphere2 contacts =
    let
        radius1 =
            sphere1.radius

        radius2 =
            sphere2.radius

        center1 =
            sphere1.position

        center2 =
            sphere2.position

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
