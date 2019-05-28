module Internal.Material exposing
    ( Material
    , Protected(..)
    , contactBounciness
    , contactFriction
    , default
    )


type Protected
    = Protected Material


type alias Material =
    { bounciness : Float
    , friction : Float
    }


contactFriction : Material -> Material -> Float
contactFriction m1 m2 =
    combine
        m1.friction
        m2.friction


contactBounciness : Material -> Material -> Float
contactBounciness m1 m2 =
    combine
        m1.bounciness
        m2.bounciness


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


default : Material
default =
    { friction = 0.3
    , bounciness = 0
    }
