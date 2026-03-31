module Internal.Material exposing
    ( Material
    , Protected(..)
    , combine
    , ice
    , rubber
    , steel
    , wood
    )


type Protected kind
    = Protected Material


type alias Material =
    { bounciness : Float
    , friction : Float
    , density : Float -- kg/m³; 0 for static materials / no density; negative for void shapes (subtracts from mass/inertia, excluded from collision)
    }


{-| Average of two floats, clamped between 0 and 1
-}
combine : Float -> Float -> Float
combine v1 v2 =
    let
        avg =
            (v1 + v2) * 0.5

        temp =
            1 + avg - abs (1 - avg)
    in
    (temp + abs temp) * 0.25


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
