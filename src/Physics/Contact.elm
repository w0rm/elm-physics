module Physics.Contact exposing
    ( Contact
    , bodies, either, both
    , points, centerPoint
    )

{-|

@docs Contact

@docs bodies, either, both

@docs points, centerPoint

-}

import Direction3d exposing (Direction3d)
import Internal.Body as InternalBody
import Internal.Contact exposing (Protected(..))
import Length exposing (Meters)
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates)
import Point3d exposing (Point3d)


{-| Contact between two colliding bodies.
-}
type alias Contact data =
    Protected data


{-| Get contacting bodies in unspecified order.
-}
bodies : Contact data -> ( Body data, Body data )
bodies (Protected { body1, body2 }) =
    ( InternalBody.Protected body1
    , InternalBody.Protected body2
    )


{-| Return true if at least one body satisfies the test.
-}
either : (Body data -> Bool) -> Contact data -> Bool
either fn (Protected { body1, body2 }) =
    fn (InternalBody.Protected body1)
        || fn (InternalBody.Protected body2)


{-| Return true if both bodies satisfy the test.
-}
both : (Body data -> Bool) -> Contact data -> Bool
both fn (Protected { body1, body2 }) =
    fn (InternalBody.Protected body1)
        && fn (InternalBody.Protected body2)


{-| Get coordinates and normals of contact points.

Normals are defined on the surface of the first body,
that is returned from [bodies](#bodies).

-}
points : Contact data -> List { point : Point3d Meters WorldCoordinates, normal : Direction3d WorldCoordinates }
points (Protected contact) =
    contact.points


{-| Get the centroid of all the contact points.
-}
centerPoint : Contact data -> Point3d Meters WorldCoordinates
centerPoint (Protected contact) =
    case contact.points of
        { point } :: remainingPoints ->
            Point3d.centroid point
                (List.map .point remainingPoints)

        _ ->
            -- This should never happen,
            -- because contacts are never empty
            Point3d.origin
