module Collision.PlaneSphere exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldTransform3d)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> ShapeWorldTransform3d -> ShapeWorldTransform3d -> Float -> List Contact -> List Contact
addContacts orderContact planeTransform3d sphereTransform3d radius contacts =
    let
        worldPlaneNormal =
            Transform3d.directionPlaceIn planeTransform3d Vec3.k

        worldVertex =
            worldPlaneNormal
                |> Vec3.scale radius
                |> Vec3.sub (Transform3d.originPoint sphereTransform3d)

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
