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
import Internal.Constraint as Internal
import Internal.Coordinates exposing (BodyLocalCoordinates)
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)


{-| Constraint allows to limit the freedom of movement
of two bodies with relation to each other.
-}
type alias Constraint =
    Internal.Constraint


{-| Connect a point on the first body with a point on the second body.
Points are defined within the bodies’ local coordinate systems.
-}
pointToPoint :
    Point3d Meters BodyLocalCoordinates
    -> Point3d Meters BodyLocalCoordinates
    -> Constraint
pointToPoint pivot1 pivot2 =
    Internal.PointToPoint
        { pivot1 = Point3d.toMeters pivot1
        , pivot2 = Point3d.toMeters pivot2
        }


{-| Keep two bodies connected with each other and limit the freedom of rotation.
Useful for e.g. connecting a window to a window frame.
-}
hinge :
    Axis3d Meters BodyLocalCoordinates
    -> Axis3d Meters BodyLocalCoordinates
    -> Constraint
hinge axis1 axis2 =
    Internal.Hinge
        { pivot1 = Point3d.toMeters (Axis3d.originPoint axis1)
        , axis1 = Direction3d.unwrap (Axis3d.direction axis1)
        , pivot2 = Point3d.toMeters (Axis3d.originPoint axis2)
        , axis2 = Direction3d.unwrap (Axis3d.direction axis2)
        }


{-| Keep the centers of two bodies at the constant distance
from each other.
-}
distance : Length -> Constraint
distance length =
    Internal.Distance (Length.inMeters length)
