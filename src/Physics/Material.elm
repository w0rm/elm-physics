module Physics.Material exposing
    ( Material, HasDensity
    , material, surface
    , wood, rubber, steel, ice
    )

{-|

@docs Material, HasDensity

@docs material, surface

@docs wood, rubber, steel, ice

-}

import Density exposing (Density)
import Internal.Material as Internal exposing (Protected(..))


{-| Material encodes friction, bounciness, and optionally density.

The type parameter uses extensible records to track capabilities:

  - `Material { a | density : () }` — carries density (required for volumetric bodies)
  - `Material a` — any material (accepted by static bodies and particles)

**Friction** controls how much a body resists sliding against another.
0 means frictionless (like ice), 1 means maximum grip (like rubber).

**Bounciness** (coefficient of restitution) controls how much kinetic energy
is preserved after a collision. 0 means no bounce (the body absorbs the impact),
1 means a perfectly elastic bounce.

When two shapes collide, friction and bounciness of both materials are averaged
and clamped to [0, 1].

-}
type alias Material kind =
    Internal.Protected kind


{-| Type constraint indicating that a material carries density.
Used by body constructors that need to compute mass from geometry.
-}
type alias HasDensity =
    { density : () }


{-| Wood. Density 700 kg/m³, friction 0.4, bounciness 0.3.
-}
wood : Material { density : () }
wood =
    Protected Internal.wood


{-| Rubber. Density 1100 kg/m³, friction 0.8, bounciness 0.7.
-}
rubber : Material { density : () }
rubber =
    Protected Internal.rubber


{-| Steel. Density 7800 kg/m³, friction 0.3, bounciness 0.2.
-}
steel : Material { density : () }
steel =
    Protected Internal.steel


{-| Ice. Density 900 kg/m³, friction 0.03, bounciness 0.1.
-}
ice : Material { density : () }
ice =
    Protected Internal.ice


{-| Create a material with density — used for dynamic bodies where mass
is computed from density and geometry.

Density is clamped to at least 1 kg/m³. Friction and bounciness are clamped to [0, 1].

-}
material : { density : Density, friction : Float, bounciness : Float } -> Material { density : () }
material cfg =
    Protected
        { density = max 1 (Density.inKilogramsPerCubicMeter cfg.density)
        , friction = clamp 0 1 cfg.friction
        , bounciness = clamp 0 1 cfg.bounciness
        }


{-| Create a material with surface properties only — friction and bounciness,
no density. Useful for static bodies and particles where mass is either
irrelevant or provided directly.

Friction and bounciness are clamped to [0, 1].

-}
surface : { friction : Float, bounciness : Float } -> Material a
surface cfg =
    Protected
        { density = 0
        , friction = clamp 0 1 cfg.friction
        , bounciness = clamp 0 1 cfg.bounciness
        }
