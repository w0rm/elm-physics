module Internal.Constraint exposing (Constraint(..), ConstraintGroup, getConstraints)

import Internal.Body as Body
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates)


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


getConstraints :
    (id -> Maybe (id -> List (Constraint BodyCoordinates)))
    -> List ( id, Body.Body )
    -> List ConstraintGroup
getConstraints constrain bodiesWithIds =
    buildConstraintsOuter constrain bodiesWithIds bodiesWithIds []


buildConstraintsOuter :
    (id -> Maybe (id -> List (Constraint BodyCoordinates)))
    -> List ( id, Body.Body )
    -> List ( id, Body.Body )
    -> List ConstraintGroup
    -> List ConstraintGroup
buildConstraintsOuter constrain bodies allBodies acc =
    case bodies of
        [] ->
            acc

        ( id1, body1 ) :: rest ->
            case constrain id1 of
                Nothing ->
                    buildConstraintsOuter constrain rest allBodies acc

                Just fn ->
                    buildConstraintsOuter constrain
                        rest
                        allBodies
                        (buildConstraintsInner fn body1 allBodies acc)


buildConstraintsInner :
    (id -> List (Constraint BodyCoordinates))
    -> Body.Body
    -> List ( id, Body.Body )
    -> List ConstraintGroup
    -> List ConstraintGroup
buildConstraintsInner fn body1 bodies acc =
    case bodies of
        [] ->
            acc

        ( id2, body2 ) :: rest ->
            if body1.id - body2.id == 0 then
                buildConstraintsInner fn body1 rest acc

            else
                case fn id2 of
                    [] ->
                        buildConstraintsInner fn body1 rest acc

                    cs ->
                        buildConstraintsInner fn
                            body1
                            rest
                            ({ bodyId1 = body1.id
                             , bodyId2 = body2.id
                             , constraints =
                                List.map
                                    (relativeToCenterOfMass
                                        body1.centerOfMassTransform3d
                                        body2.centerOfMassTransform3d
                                    )
                                    cs
                             }
                                :: acc
                            )
