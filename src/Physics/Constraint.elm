module Physics.Constraint exposing
    ( Constraint
    , pointToPoint, hinge, distance
    )

{-|

@docs Constraint

@docs pointToPoint, hinge, distance

-}

import Axis3d exposing (Axis3d)
import Direction3d
import Internal.Constraint as Internal exposing (Protected(..))
import Length exposing (Length, Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)


{-| Constraint allows to limit the freedom of movement
of two bodies with relation to each other.
-}
type alias Constraint =
    Protected


{-| Connect a point on the first body with a point on the second body.
Points are defined within the bodies’ local coordinate systems.
-}
pointToPoint :
    Point3d Meters BodyCoordinates
    -> Point3d Meters BodyCoordinates
    -> Constraint
pointToPoint pivot1 pivot2 =
    Protected (Internal.PointToPoint (Point3d.toMeters pivot1) (Point3d.toMeters pivot2))


{-| Keep two bodies connected with each other and limit the freedom of rotation.
Useful for e.g. connecting a window to a window frame, or to connect a wheel to a car.
-}
hinge :
    Axis3d Meters BodyCoordinates
    -> Axis3d Meters BodyCoordinates
    -> Constraint
hinge axis3d1 axis3d2 =
    let
        pivot1 =
            Point3d.toMeters (Axis3d.originPoint axis3d1)

        axis1 =
            Direction3d.unwrap (Axis3d.direction axis3d1)

        pivot2 =
            Point3d.toMeters (Axis3d.originPoint axis3d2)

        axis2 =
            Direction3d.unwrap (Axis3d.direction axis3d2)
    in
    Protected (Internal.Hinge pivot1 axis1 pivot2 axis2)


{-| Keep the centers of two bodies at the constant distance
from each other.
-}
distance : Length -> Constraint
distance length =
    Protected (Internal.Distance (Length.inMeters length))
