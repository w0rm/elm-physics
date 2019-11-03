module Internal.Constraint exposing (Constraint(..), ConstraintGroup, relativeToCenterOfMass)

import Axis3d exposing (Axis3d)
import Frame3d exposing (Frame3d)
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Length exposing (Length, Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)


type Constraint coordinates
    = PointToPoint (Point3d Meters coordinates) (Point3d Meters coordinates)
    | Hinge (Axis3d Meters coordinates) (Axis3d Meters coordinates)
    | Distance Length


type alias ConstraintGroup =
    { bodyId1 : Int
    , bodyId2 : Int
    , constraints : List (Constraint CenterOfMassCoordinates)
    }


relativeToCenterOfMass :
    Frame3d Meters BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Frame3d Meters BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Constraint BodyCoordinates
    -> Constraint CenterOfMassCoordinates
relativeToCenterOfMass centerOfMassFrame3d1 centerOfMassFrame3d2 constraint =
    case constraint of
        PointToPoint pivot1 pivot2 ->
            PointToPoint
                (Point3d.relativeTo centerOfMassFrame3d1 pivot1)
                (Point3d.relativeTo centerOfMassFrame3d2 pivot2)

        Hinge axis1 axis2 ->
            Hinge
                (Axis3d.relativeTo centerOfMassFrame3d1 axis1)
                (Axis3d.relativeTo centerOfMassFrame3d2 axis2)

        Distance length ->
            Distance length
