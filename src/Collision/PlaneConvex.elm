module Collision.PlaneConvex exposing (addContacts)

import Frame3d
import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex)
import Internal.Coordinates exposing (ShapeWorldFrame3d)
import Internal.Vector3 as Vec3
import Point3d
import Vector3d


addContacts : (Contact -> Contact) -> ShapeWorldFrame3d -> ShapeWorldFrame3d -> Convex -> List Contact -> List Contact
addContacts orderContact planeTransform convexFrame3d { vertices } contacts =
    let
        worldNormal =
            Vector3d.toMeters (Vector3d.placeIn convexFrame3d (Vector3d.fromMeters Vec3.k))

        planeOriginPosition =
            Point3d.toMeters (Frame3d.originPoint planeTransform)
    in
    List.foldl
        (\vertex currentContacts ->
            let
                worldVertex =
                    Point3d.toMeters (Point3d.placeIn convexFrame3d (Point3d.fromMeters vertex))

                dot =
                    planeOriginPosition
                        |> Vec3.sub worldVertex
                        |> Vec3.dot worldNormal
            in
            if dot <= 0 then
                orderContact
                    { ni = worldNormal
                    , pi =
                        worldNormal
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
