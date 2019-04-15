module Internal.Material exposing
    ( Material
    , Protected(..)
    , Strategy
    , average
    , contactBounciness
    , contactFriction
    , default
    , maximum
    , minimum
    , multiply
    )


type Protected
    = Protected Material


type alias Material =
    { bounciness : Float
    , friction : Float
    , bouncinessStrategy : Strategy
    , frictionStrategy : Strategy
    }


type Strategy
    = Strategy Int


average : Strategy
average =
    Strategy 1


minimum : Strategy
minimum =
    Strategy 2


multiply : Strategy
multiply =
    Strategy 3


maximum : Strategy
maximum =
    Strategy 4


contactFriction : Material -> Material -> Float
contactFriction m1 m2 =
    combine
        m1.frictionStrategy
        m1.friction
        m2.frictionStrategy
        m2.friction


contactBounciness : Material -> Material -> Float
contactBounciness m1 m2 =
    combine
        m1.bouncinessStrategy
        m1.bounciness
        m2.bouncinessStrategy
        m2.bounciness


combine : Strategy -> Float -> Strategy -> Float -> Float
combine (Strategy s1) v1 (Strategy s2) v2 =
    case max s1 s2 of
        -- average
        1 ->
            clamp 0 1 ((v1 + v2) * 0.5)

        -- minimum
        2 ->
            clamp 0 1 (min v1 v2)

        -- multiply
        3 ->
            clamp 0 1 (v1 * v2)

        -- maximum
        _ ->
            clamp 0 1 (max v1 v2)


default : Material
default =
    { friction = 0.3
    , bounciness = 0
    , bouncinessStrategy = average
    , frictionStrategy = average
    }
