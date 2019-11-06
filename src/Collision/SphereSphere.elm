module Collision.SphereSphere exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldTransform3d)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


addContacts : ShapeWorldTransform3d -> Float -> ShapeWorldTransform3d -> Float -> List Contact -> List Contact
addContacts transform3d1 radius1 transform3d2 radius2 contacts =
    let
        center1 =
            Transform3d.originPoint transform3d1

        center2 =
            Transform3d.originPoint transform3d2

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
