module Collision.SphereSphere exposing (addContacts)

import Frame3d
import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldFrame3d)
import Internal.Vector3 as Vec3
import Point3d


addContacts : ShapeWorldFrame3d -> Float -> ShapeWorldFrame3d -> Float -> List Contact -> List Contact
addContacts frame3d1 radius1 frame3d2 radius2 contacts =
    let
        center1 =
            Point3d.toMeters (Frame3d.originPoint frame3d1)

        center2 =
            Point3d.toMeters (Frame3d.originPoint frame3d2)

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
