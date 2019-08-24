module Internal.Constraint exposing (Constraint(..), ConstraintGroup)

import Internal.Vector3 exposing (Vec3)


type Constraint
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
    , constraints : List Constraint
    }
