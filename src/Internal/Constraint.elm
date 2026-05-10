module Internal.Constraint exposing (Constraint(..), flip, relativeToCenterOfMass)

import Internal.Coordinates exposing (BodyCoordinates)
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 exposing (Vec3)


type Constraint coordinates
    = PointToPoint Vec3 Vec3
    | Hinge Vec3 Vec3 Vec3 Vec3
    | Lock Vec3 Vec3 Vec3 Vec3 Vec3 Vec3 Vec3 Vec3
    | Distance Float


relativeToCenterOfMass :
    Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Constraint BodyCoordinates
    -> Constraint CenterOfMassCoordinates
relativeToCenterOfMass centerOfMassFrame3d1 centerOfMassFrame3d2 constraint =
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


{-| Swap body1 and body2 roles. Used when the user registered the constrain
callback on the body that ends up at the larger gravity-sort position, so
the constraint must be re-keyed to put the smaller-position body first.
-}
flip : Constraint coordinates -> Constraint coordinates
flip constraint =
    case constraint of
        PointToPoint p1 p2 ->
            PointToPoint p2 p1

        Hinge p1 a1 p2 a2 ->
            Hinge p2 a2 p1 a1

        Lock p1 x1 y1 z1 p2 x2 y2 z2 ->
            Lock p2 x2 y2 z2 p1 x1 y1 z1

        Distance length ->
            Distance length
