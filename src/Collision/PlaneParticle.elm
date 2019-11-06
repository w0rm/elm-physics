module Collision.PlaneParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldTransform3d)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> ShapeWorldTransform3d -> ShapeWorldTransform3d -> List Contact -> List Contact
addContacts orderContact planeTransform3d particleFrame3d contacts =
    let
        worldPlaneNormal =
            Transform3d.directionPlaceIn planeTransform3d Vec3.k

        worldVertex =
            Transform3d.originPoint particleFrame3d

        dot =
            Transform3d.originPoint planeTransform3d
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
