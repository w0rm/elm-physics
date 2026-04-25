module Internal.Lock exposing (Lock(..), masks)

import Internal.Vector3 as Vec3 exposing (Vec3)


type Lock
    = TranslateX
    | TranslateY
    | TranslateZ
    | RotateX
    | RotateY
    | RotateZ


{-| Reduce a list of locks to a pair of (1/0) component-wise masks.
The first Vec3 masks linear velocity; the second masks angular velocity.
A component is 0 when locked, 1 when free. An empty list yields fully free masks.
-}
masks : List Lock -> ( Vec3, Vec3 )
masks locks =
    foldMasks locks Vec3.one Vec3.one


foldMasks : List Lock -> Vec3 -> Vec3 -> ( Vec3, Vec3 )
foldMasks locks linear angular =
    case locks of
        [] ->
            ( linear, angular )

        TranslateX :: rest ->
            foldMasks rest { linear | x = 0 } angular

        TranslateY :: rest ->
            foldMasks rest { linear | y = 0 } angular

        TranslateZ :: rest ->
            foldMasks rest { linear | z = 0 } angular

        RotateX :: rest ->
            foldMasks rest linear { angular | x = 0 }

        RotateY :: rest ->
            foldMasks rest linear { angular | y = 0 }

        RotateZ :: rest ->
            foldMasks rest linear { angular | z = 0 }
