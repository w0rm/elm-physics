module Collision.PlaneParticle exposing (addContacts)

import Frame3d
import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldFrame3d)
import Internal.Vector3 as Vec3
import Point3d
import Vector3d


addContacts : (Contact -> Contact) -> ShapeWorldFrame3d -> ShapeWorldFrame3d -> List Contact -> List Contact
addContacts orderContact planeFrame3d particleFrame3d contacts =
    let
        worldPlaneNormal =
            Vector3d.toMeters (Vector3d.placeIn planeFrame3d (Vector3d.fromMeters Vec3.k))

        worldVertex =
            Point3d.toMeters (Frame3d.originPoint particleFrame3d)

        dot =
            Point3d.toMeters (Frame3d.originPoint planeFrame3d)
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
