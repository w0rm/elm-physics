module Physics.Constraint exposing (Constraint, pointToPoint)

import Internal.Constraint as Internal


{-| -}
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
