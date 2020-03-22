module Internal.Matrix3 exposing
    ( Mat3
    , identity
    )

{-| -}


{-| 3x3 matrix type
-}
type alias Mat3 =
    { m11 : Float
    , m21 : Float
    , m31 : Float
    , m12 : Float
    , m22 : Float
    , m32 : Float
    , m13 : Float
    , m23 : Float
    , m33 : Float
    }


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat3
identity =
    { m11 = 1
    , m21 = 0
    , m31 = 0
    , m12 = 0
    , m22 = 1
    , m32 = 0
    , m13 = 0
    , m23 = 0
    , m33 = 1
    }
