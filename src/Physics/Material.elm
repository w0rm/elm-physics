module Physics.Material exposing (Material, default, custom)

{-|

@docs Material, default, custom

-}

import Internal.Material as Internal exposing (Protected(..))


{-| Materials allow to control friction and bounciness.
You can change materials using [Body.withMaterial](Physics-Body#withMaterial).
-}
type alias Material =
    Protected


{-| All bodies initially use this material, it is defined like this:

    default =
        custom { friction = 0.3, bounciness = 0 }

-}
default : Material
default =
    Protected Internal.default


{-| Creates a custom material, e.g.

    slippery =
        custom { friction = 0, bounciness = 0 }

    bouncy =
        custom { friction = 0.3, bounciness = 0.9 }

When two materials collide, their properties are averaged
and clamped between 0 and 1.

-}
custom : { friction : Float, bounciness : Float } -> Material
custom =
    Protected
