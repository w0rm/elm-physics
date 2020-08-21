module Internal.Constraint exposing (Constraint(..), ConstraintGroup, Protected(..), relativeToCenterOfMass)

import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates)


type Protected
    = Protected (Constraint BodyCoordinates)


type Constraint coordinates
    = PointToPoint Vec3 Vec3
    | Hinge Vec3 Vec3 Vec3 Vec3
    | Lock Vec3 Vec3 Vec3 Vec3 Vec3 Vec3 Vec3 Vec3
    | Distance Float


type alias ConstraintGroup =
    { bodyId1 : Int
    , bodyId2 : Int
    , constraints : List (Constraint CenterOfMassCoordinates)
    }


relativeToCenterOfMass :
    Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Protected
    -> Constraint CenterOfMassCoordinates
relativeToCenterOfMass centerOfMassFrame3d1 centerOfMassFrame3d2 (Protected constraint) =
    case constraint of
        PointToPoint pivot1 pivot2 ->
            PointToPoint
                (Transform3d.pointRelativeTo centerOfMassFrame3d1 pivot1)
                (Transform3d.pointRelativeTo centerOfMassFrame3d2 pivot2)

        Hinge pivot1 axis1 pivot2 axis2 ->
            Hinge
                (Transform3d.pointRelativeTo centerOfMassFrame3d1 pivot1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 axis1)
                (Transform3d.pointRelativeTo centerOfMassFrame3d2 pivot2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 axis2)

        Lock pivot1 x1 y1 z1 pivot2 x2 y2 z2 ->
            Lock
                (Transform3d.pointRelativeTo centerOfMassFrame3d1 pivot1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 x1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 y1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 z1)
                (Transform3d.pointRelativeTo centerOfMassFrame3d2 pivot2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 x2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 y2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 z2)

        Distance length ->
            Distance length
