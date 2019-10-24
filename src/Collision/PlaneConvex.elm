module Collision.PlaneConvex exposing (addContacts)

import Direction3d
import Frame3d
import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex)
import Internal.Coordinates exposing (ShapeWorldFrame3d)
import Internal.Vector3 as Vec3
import Point3d


addContacts : (Contact -> Contact) -> ShapeWorldFrame3d -> ShapeWorldFrame3d -> Convex -> List Contact -> List Contact
addContacts orderContact planeFrame3d convexFrame3d { vertices } contacts =
    let
        planeWorldNormal =
            Direction3d.unwrap (Direction3d.placeIn planeFrame3d Direction3d.z)

        planeOriginPosition =
            Point3d.toMeters (Frame3d.originPoint planeFrame3d)
    in
    List.foldl
        (\vertex currentContacts ->
            let
                worldVertex =
                    Point3d.toMeters (Point3d.placeIn convexFrame3d (Point3d.fromMeters vertex))

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
