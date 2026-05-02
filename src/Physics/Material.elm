module Physics.Material exposing
    ( Material
    , wood, rubber, steel, ice, plastic
    , Dense, dense, Surface, surface
    )

{-|

@docs Material

@docs wood, rubber, steel, ice, plastic


# Custom materials

@docs Dense, dense, Surface, surface

-}

import Density exposing (Density)
import Internal.Material as Internal
import Physics.Types as Types


{-| Material encodes friction, bounciness, and optionally density.

The type parameter tracks capabilities:

  - `Material Dense` — carries density (required for volumetric bodies)
  - `Material Surface` — surface properties only (for static bodies and point masses)

**Friction** controls how much a body resists sliding against another.
0 means frictionless (like ice), 1 means maximum grip (like rubber).

**Bounciness** (coefficient of restitution) controls how much kinetic energy
is preserved after a collision. 0 means no bounce (the body absorbs the impact),
1 means a perfectly elastic bounce.

When two shapes collide, their friction values are combined using the geometric
mean √(f1 · f2), so a slippery surface dominates. Bounciness uses the
maximum of the two values, so the bouncier surface wins.

-}
type alias Material kind =
    Types.Material kind


{-| Density 700 kg/m³, friction 0.4, bounciness 0.3.
-}
wood : Material any
wood =
    Types.Material Internal.wood


{-| Density 1100 kg/m³, friction 0.8, bounciness 0.7.
-}
rubber : Material any
rubber =
    Types.Material Internal.rubber


{-| Density 7800 kg/m³, friction 0.3, bounciness 0.2.
-}
steel : Material any
steel =
    Types.Material Internal.steel


{-| Density 900 kg/m³, friction 0.03, bounciness 0.1.
-}
ice : Material any
ice =
    Types.Material Internal.ice


{-| Density 1050 kg/m³, friction 0.35, bounciness 0.45.
-}
plastic : Material any
plastic =
    Types.Material Internal.plastic


{-| Material with density, required for dynamic bodies
where mass is computed from geometry.
-}
type Dense
    = Dense Never


{-| Create a dense material.

Density is clamped to at least 1 kg/m³. Friction and bounciness are clamped to [0, 1].

-}
dense : { density : Density, friction : Float, bounciness : Float } -> Material Dense
dense cfg =
    Types.Material
        { density = max 1 (Density.inKilogramsPerCubicMeter cfg.density)
        , friction = clamp 0 1 cfg.friction
        , bounciness = clamp 0 1 cfg.bounciness
        }


{-| Material with surface properties only — friction and bounciness,
no density. Used for static bodies and point masses.
-}
type Surface
    = Surface Never


{-| Create a surface material.

Friction and bounciness are clamped to [0, 1].

-}
surface : { friction : Float, bounciness : Float } -> Material Surface
surface cfg =
    Types.Material
        { density = 0
        , friction = clamp 0 1 cfg.friction
        , bounciness = clamp 0 1 cfg.bounciness
        }
