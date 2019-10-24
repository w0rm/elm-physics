module Collision.SphereParticle exposing (addContacts)

import Frame3d
import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldFrame3d)
import Internal.Vector3 as Vec3
import Point3d


addContacts : (Contact -> Contact) -> ShapeWorldFrame3d -> Float -> ShapeWorldFrame3d -> List Contact -> List Contact
addContacts orderContact sphereFrame3d radius pointFrame3d contacts =
    let
        center1 =
            Point3d.toMeters (Frame3d.originPoint sphereFrame3d)

        center2 =
            Point3d.toMeters (Frame3d.originPoint pointFrame3d)

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
