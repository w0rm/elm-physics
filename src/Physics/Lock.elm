module Physics.Lock exposing
    ( Lock
    , translateX, translateY, translateZ
    , rotateX, rotateY, rotateZ
    , allTranslation, allRotation
    )

{-| Restrict a body’s degrees of freedom along world axes.

Pass a list of `Lock` tokens to [Physics.lock](Physics#lock). Each entry
removes one degree of freedom from the body. The list fully describes the
body’s lock state — calling `lock` again with a different list replaces the
previous one. An empty list clears all locks.

@docs Lock


# Translation

@docs translateX, translateY, translateZ


# Rotation

@docs rotateX, rotateY, rotateZ


# Presets

@docs allTranslation, allRotation

-}

import Internal.Lock as Internal
import Physics.Types as Types


{-| A single degree of freedom to lock.
-}
type alias Lock =
    Types.Lock


{-| Lock translation along the world X axis.
-}
translateX : Lock
translateX =
    Internal.TranslateX


{-| Lock translation along the world Y axis.
-}
translateY : Lock
translateY =
    Internal.TranslateY


{-| Lock translation along the world Z axis.
-}
translateZ : Lock
translateZ =
    Internal.TranslateZ


{-| Lock rotation about the world X axis.
-}
rotateX : Lock
rotateX =
    Internal.RotateX


{-| Lock rotation about the world Y axis.
-}
rotateY : Lock
rotateY =
    Internal.RotateY


{-| Lock rotation about the world Z axis.
-}
rotateZ : Lock
rotateZ =
    Internal.RotateZ


{-| All three translation axes locked. The body cannot move.
-}
allTranslation : List Lock
allTranslation =
    [ translateX, translateY, translateZ ]


{-| All three rotation axes locked. The body cannot tip or spin.
Useful for character capsules whose orientation is driven outside
of physics.
-}
allRotation : List Lock
allRotation =
    [ rotateX, rotateY, rotateZ ]
