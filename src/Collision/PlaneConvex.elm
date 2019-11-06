module Collision.PlaneConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex)
import Internal.Coordinates exposing (ShapeWorldTransform3d)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> ShapeWorldTransform3d -> ShapeWorldTransform3d -> Convex -> List Contact -> List Contact
addContacts orderContact planeTransform3d convexFrame3d { vertices } contacts =
    let
        planeWorldNormal =
            Transform3d.directionPlaceIn planeTransform3d Vec3.k

        planeOriginPosition =
            Transform3d.originPoint planeTransform3d
    in
    List.foldl
        (\vertex currentContacts ->
            let
                worldVertex =
                    Transform3d.pointPlaceIn convexFrame3d vertex

                dot =
                    planeOriginPosition
                        |> Vec3.sub worldVertex
                        |> Vec3.dot planeWorldNormal
            in
            if dot <= 0 then
                orderContact
                    { ni = planeWorldNormal
                    , pi =
                        planeWorldNormal
                            |> Vec3.scale dot
                            |> Vec3.sub worldVertex
                    , pj = worldVertex
                    }
                    :: currentContacts

            else
                currentContacts
        )
        contacts
        vertices
