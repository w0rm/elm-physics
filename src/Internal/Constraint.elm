module Internal.Constraint exposing (Constraint(..), relativeToCenterOfMass, relativeToCenterOfMassFlipped)

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


{-| Like `relativeToCenterOfMass`, but also swaps body1 and body2 roles in
the output. Used when the user registered the constrain callback on the
body that ends up at the larger gravity-sort position: the input pivots are
labeled relative to (callback-body, partner) but the output must be labeled
relative to (smaller-position-body, larger-position-body).

Caller passes the frames in the _natural_ order (smaller-position body first,
larger-position body second). The function transforms input.pivot1 (the
caller-body's pivot) with the larger-position frame and emits it as output
pivot2; transforms input.pivot2 with the smaller-position frame and emits it
as output pivot1. Single allocation, vs. `relativeToCenterOfMass >> flip`
which would allocate twice.

-}
relativeToCenterOfMassFlipped :
    Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> Constraint BodyCoordinates
    -> Constraint CenterOfMassCoordinates
relativeToCenterOfMassFlipped centerOfMassFrame3d1 centerOfMassFrame3d2 constraint =
    case constraint of
        PointToPoint pivot1 pivot2 ->
            PointToPoint
                (Transform3d.pointRelativeTo centerOfMassFrame3d1 pivot2)
                (Transform3d.pointRelativeTo centerOfMassFrame3d2 pivot1)

        Hinge pivot1 axis1 pivot2 axis2 ->
            Hinge
                (Transform3d.pointRelativeTo centerOfMassFrame3d1 pivot2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 axis2)
                (Transform3d.pointRelativeTo centerOfMassFrame3d2 pivot1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 axis1)

        Lock pivot1 x1 y1 z1 pivot2 x2 y2 z2 ->
            Lock
                (Transform3d.pointRelativeTo centerOfMassFrame3d1 pivot2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 x2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 y2)
                (Transform3d.directionRelativeTo centerOfMassFrame3d1 z2)
                (Transform3d.pointRelativeTo centerOfMassFrame3d2 pivot1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 x1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 y1)
                (Transform3d.directionRelativeTo centerOfMassFrame3d2 z1)

        Distance length ->
            Distance length
