module Physics.Material exposing
    ( Material, default, custom
    , average, minimum, multiply, maximum
    )

{-|

@docs Material, default, custom

@docs customWithStrategy, Strategy
@docs average, minimum, multiply, maximum

-}

import Internal.Material as Internal exposing (Protected(..))


{-| -}
type alias Material =
    Protected


{-| -}
default : Material
default =
    Protected Internal.default


{-| -}
custom : { friction : Float, bounciness : Float } -> Material
custom { friction, bounciness } =
    customWithStrategy
        { friction = friction
        , bounciness = bounciness
        , frictionStrategy = average
        , bouncinessStrategy = average
        }


{-| -}
customWithStrategy :
    { friction : Float
    , bounciness : Float
    , frictionStrategy : Strategy
    , bouncinessStrategy : Strategy
    }
    -> Material
customWithStrategy =
    Protected


{-| -}
type alias Strategy =
    Internal.Strategy


{-| -}
average : Strategy
average =
    Internal.average


{-| -}
minimum : Strategy
minimum =
    Internal.minimum


{-| -}
multiply : Strategy
multiply =
    Internal.multiply


{-| -}
maximum : Strategy
maximum =
    Internal.maximum
