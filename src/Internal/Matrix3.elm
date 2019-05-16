module Internal.Matrix3 exposing
    ( Mat3
    , identity
    , mul
    , scale
    , transform
    )

import Internal.Vector3 as Vec3 exposing (Vec3)


{-| 3x3 matrix type
-}
type alias Mat3 =
    { m11 : Float, m21 : Float, m31 : Float, m12 : Float, m22 : Float, m32 : Float, m13 : Float, m23 : Float, m33 : Float }


{-| Multiply a vector by a 4x4 matrix: m \* v
-}
transform : Mat3 -> Vec3 -> Vec3
transform m { x, y, z } =
    { x = m.m11 * x + m.m12 * y + m.m13 * z
    , y = m.m21 * x + m.m22 * y + m.m23 * z
    , z = m.m31 * x + m.m32 * y + m.m33 * z
    }


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat3
identity =
    { m11 = 1, m21 = 0, m31 = 0, m12 = 0, m22 = 1, m32 = 0, m13 = 0, m23 = 0, m33 = 1 }


{-| Matrix multiplcation: a \* b
-}
mul : Mat3 -> Mat3 -> Mat3
mul a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31
    , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31
    , m31 = a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31
    , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32
    , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32
    , m32 = a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32
    , m13 = a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33
    , m23 = a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33
    , m33 = a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33
    }


{-| Concatenates a scaling to the given matrix.
-}
scale : Vec3 -> Mat3 -> Mat3
scale { x, y, z } m =
    { m11 = m.m11 * x
    , m21 = m.m21 * x
    , m31 = m.m31 * x
    , m12 = m.m12 * y
    , m22 = m.m22 * y
    , m32 = m.m32 * y
    , m13 = m.m13 * z
    , m23 = m.m23 * z
    , m33 = m.m33 * z
    }
