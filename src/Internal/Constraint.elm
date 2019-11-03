module Internal.Constraint exposing (Constraint(..), ConstraintGroup, relativeToCenterOfMass)

import Direction3d
import Frame3d exposing (Frame3d)
import Internal.Vector3 exposing (Vec3)
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates, CenterOfMassCoordinates)
import Point3d


type Constraint coordinates
    = PointToPoint
        { pivot1 : Vec3
        , pivot2 : Vec3
        }
    | Hinge
        { pivot1 : Vec3
        , axis1 : Vec3
        , pivot2 : Vec3
        , axis2 : Vec3
        }
    | Distance Float


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
        PointToPoint { pivot1, pivot2 } ->
            PointToPoint
                { pivot1 = Point3d.toMeters (Point3d.relativeTo centerOfMassFrame3d1 (Point3d.fromMeters pivot1))
                , pivot2 = Point3d.toMeters (Point3d.relativeTo centerOfMassFrame3d2 (Point3d.fromMeters pivot2))
                }

        Hinge { pivot1, axis1, pivot2, axis2 } ->
            Hinge
                { pivot1 = Point3d.toMeters (Point3d.relativeTo centerOfMassFrame3d1 (Point3d.fromMeters pivot1))
                , axis1 = Direction3d.unwrap (Direction3d.relativeTo centerOfMassFrame3d1 (Direction3d.unsafe axis1))
                , pivot2 = Point3d.toMeters (Point3d.relativeTo centerOfMassFrame3d2 (Point3d.fromMeters pivot2))
                , axis2 = Direction3d.unwrap (Direction3d.relativeTo centerOfMassFrame3d1 (Direction3d.unsafe axis2))
                }

        Distance length ->
            Distance length
