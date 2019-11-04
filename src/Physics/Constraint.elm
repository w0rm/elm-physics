module Physics.Constraint exposing
    ( Constraint
    , pointToPoint, hinge, distance
    )

{-|

@docs Constraint

@docs pointToPoint, hinge, distance

-}

import Axis3d exposing (Axis3d)
import Internal.Constraint as Internal
import Length exposing (Length, Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)


{-| Constraint allows to limit the freedom of movement
of two bodies with relation to each other.
-}
type alias Constraint =
    Internal.Protected


{-| Connect a point on the first body with a point on the second body.
Points are defined within the bodies’ local coordinate systems.
-}
pointToPoint :
    Point3d Meters BodyCoordinates
    -> Point3d Meters BodyCoordinates
    -> Constraint
pointToPoint pivot1 pivot2 =
    Internal.Protected (Internal.PointToPoint pivot1 pivot2)


{-| Keep two bodies connected with each other and limit the freedom of rotation.
Useful for e.g. connecting a window to a window frame, or to connect a wheel to a car.
-}
hinge :
    Axis3d Meters BodyCoordinates
    -> Axis3d Meters BodyCoordinates
    -> Constraint
hinge axis1 axis2 =
    Internal.Protected (Internal.Hinge axis1 axis2)


{-| Keep the centers of two bodies at the constant distance
from each other.
-}
distance : Length -> Constraint
distance length =
    Internal.Protected (Internal.Distance length)
