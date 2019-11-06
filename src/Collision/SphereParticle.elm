module Collision.SphereParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldTransform3d)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> ShapeWorldTransform3d -> Float -> ShapeWorldTransform3d -> List Contact -> List Contact
addContacts orderContact sphereTransform3d radius pointTransform3d contacts =
    let
        center1 =
            Transform3d.originPoint sphereTransform3d

        center2 =
            Transform3d.originPoint pointTransform3d

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
