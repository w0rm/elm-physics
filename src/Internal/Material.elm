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


combine : Float -> Float -> Float
combine v1 v2 =
    clamp 0 1 ((v1 + v2) * 0.5)


default : Material
default =
    { friction = 0.3
    , bounciness = 0
    }
