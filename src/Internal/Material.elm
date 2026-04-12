module Internal.Material exposing
    ( Material
    , combineBounciness
    , combineFriction
    , ice
    , rubber
    , steel
    , wood
    )


type alias Material =
    { bounciness : Float
    , friction : Float
    , density : Float
    }


{-| Geometric mean of two frictions: sqrt(f1 \* f2).
If one surface is slippery, the result stays low.
-}
combineFriction : Float -> Float -> Float
combineFriction f1 f2 =
    sqrt (f1 * f2)


{-| Branchless max of two bounciness values.
The bouncier surface wins.
-}
combineBounciness : Float -> Float -> Float
combineBounciness b1 b2 =
    (b1 + b2 + abs (b1 - b2)) * 0.5


wood : Material
wood =
    { friction = 0.4, bounciness = 0.3, density = 700 }


rubber : Material
rubber =
    { friction = 0.8, bounciness = 0.7, density = 1100 }


steel : Material
steel =
    { friction = 0.3, bounciness = 0.2, density = 7800 }


ice : Material
ice =
    { friction = 0.03, bounciness = 0.1, density = 900 }
