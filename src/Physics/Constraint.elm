module Physics.Constraint exposing
    ( Constraint
    , pointToPoint, hinge, distance
    )

{-|

@docs Constraint

@docs pointToPoint, hinge, distance

-}

import Internal.Constraint as Internal


{-| Constraint allows to limit the freedom of movement
of two bodies with relation to each other.
-}
type alias Constraint =
    Internal.Constraint


{-| Connect a point on the first body with a point on the second body.
Points are defined within the bodies’ local coordinate systems.
-}
pointToPoint :
    { pivot1 : { x : Float, y : Float, z : Float }
    , pivot2 : { x : Float, y : Float, z : Float }
    }
    -> Constraint
pointToPoint =
    Internal.PointToPoint


{-| Keep two bodies connected with each other like pointToPoint,
but also limit the freedom of rotation. Useful for e.g. connecting
a window to a window frame.
-}
hinge :
    { pivot1 : { x : Float, y : Float, z : Float }
    , axis1 : { x : Float, y : Float, z : Float }
    , pivot2 : { x : Float, y : Float, z : Float }
    , axis2 : { x : Float, y : Float, z : Float }
    }
    -> Constraint
hinge =
    Internal.Hinge

{-| Keep the centers of two bodies at the constant distance
from each other.
-}
distance :
    Float
    -> Constraint
distance =
    Internal.Distance
