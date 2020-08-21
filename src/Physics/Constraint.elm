module Physics.Constraint exposing
    ( Constraint
    , pointToPoint, hinge, distance, lock
    )

{-|

@docs Constraint

@docs pointToPoint, hinge, distance, lock

-}

import Axis3d exposing (Axis3d)
import Direction3d
import Frame3d exposing (Frame3d)
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
This doesn’t limit the freedom of rotation of two bodies.
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


{-| Keep two bodies connected with each other and remove all degrees of freedom between bodies.
-}
lock :
    Frame3d Meters BodyCoordinates {}
    -> Frame3d Meters BodyCoordinates {}
    -> Constraint
lock frame3d1 frame3d2 =
    let
        pivot1 =
            Point3d.toMeters (Frame3d.originPoint frame3d1)

        x1 =
            Direction3d.unwrap (Frame3d.xDirection frame3d1)

        y1 =
            Direction3d.unwrap (Frame3d.yDirection frame3d1)

        z1 =
            if Frame3d.isRightHanded frame3d1 then
                Direction3d.unwrap (Frame3d.zDirection frame3d1)

            else
                Direction3d.unwrap (Direction3d.reverse (Frame3d.zDirection frame3d1))

        pivot2 =
            Point3d.toMeters (Frame3d.originPoint frame3d2)

        x2 =
            Direction3d.unwrap (Frame3d.xDirection frame3d2)

        y2 =
            Direction3d.unwrap (Frame3d.yDirection frame3d2)

        z2 =
            if Frame3d.isRightHanded frame3d2 then
                Direction3d.unwrap (Frame3d.zDirection frame3d2)

            else
                Direction3d.unwrap (Direction3d.reverse (Frame3d.zDirection frame3d2))
    in
    Protected (Internal.Lock pivot1 x1 y1 z1 pivot2 x2 y2 z2)


{-| Keep the centers of two bodies at the constant distance
from each other.
-}
distance : Length -> Constraint
distance length =
    Protected (Internal.Distance (Length.inMeters length))
