module AltMath.Matrix4 exposing
    ( Mat4
    , identity
    , makeTranslate
    , mul
    , scale
    , transform
    , transpose
    )

import AltMath.Vector3 as Vec3 exposing (Vec3)


{-| 4x4 matrix type
-}
type alias Mat4 =
    { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float }


{-| Multiply a vector by a 4x4 matrix: m \* v
-}
transform : Mat4 -> Vec3 -> Vec3
transform m { x, y, z } =
    let
        w =
            x * m.m41 + y * m.m42 + z * m.m43 + m.m44
    in
    { x = (m.m11 * x + m.m12 * y + m.m13 * z) / w
    , y = (m.m21 * x + m.m22 * y + m.m23 * z) / w
    , z = (m.m31 * x + m.m32 * y + m.m33 * z) / w
    }


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat4
identity =
    { m11 = 1, m21 = 0, m31 = 0, m41 = 0, m12 = 0, m22 = 1, m32 = 0, m42 = 0, m13 = 0, m23 = 0, m33 = 1, m43 = 0, m14 = 0, m24 = 0, m34 = 0, m44 = 1 }


{-| Matrix multiplcation: a \* b
-}
mul : Mat4 -> Mat4 -> Mat4
mul a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31 + a.m14 * b.m41
    , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31 + a.m24 * b.m41
    , m31 = a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31 + a.m34 * b.m41
    , m41 = a.m41 * b.m11 + a.m42 * b.m21 + a.m43 * b.m31 + a.m44 * b.m41
    , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32 + a.m14 * b.m42
    , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32 + a.m24 * b.m42
    , m32 = a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32 + a.m34 * b.m42
    , m42 = a.m41 * b.m12 + a.m42 * b.m22 + a.m43 * b.m32 + a.m44 * b.m42
    , m13 = a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33 + a.m14 * b.m43
    , m23 = a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33 + a.m24 * b.m43
    , m33 = a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33 + a.m34 * b.m43
    , m43 = a.m41 * b.m13 + a.m42 * b.m23 + a.m43 * b.m33 + a.m44 * b.m43
    , m14 = a.m11 * b.m14 + a.m12 * b.m24 + a.m13 * b.m34 + a.m14 * b.m44
    , m24 = a.m21 * b.m14 + a.m22 * b.m24 + a.m23 * b.m34 + a.m24 * b.m44
    , m34 = a.m31 * b.m14 + a.m32 * b.m24 + a.m33 * b.m34 + a.m34 * b.m44
    , m44 = a.m41 * b.m14 + a.m42 * b.m24 + a.m43 * b.m34 + a.m44 * b.m44
    }


{-| Concatenates a scaling to the given matrix.
-}
scale : Vec3 -> Mat4 -> Mat4
scale { x, y, z } m =
    { m11 = m.m11 * x
    , m21 = m.m21 * x
    , m31 = m.m31 * x
    , m41 = m.m41 * x
    , m12 = m.m12 * y
    , m22 = m.m22 * y
    , m32 = m.m32 * y
    , m42 = m.m42 * y
    , m13 = m.m13 * z
    , m23 = m.m23 * z
    , m33 = m.m33 * z
    , m43 = m.m43 * z
    , m14 = m.m14
    , m24 = m.m24
    , m34 = m.m34
    , m44 = m.m44
    }


{-| Creates a transformation matrix for translating each of the x, y, and z
axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Vec3 -> Mat4
makeTranslate { x, y, z } =
    { m11 = 1
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 1
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = 1
    , m43 = 0
    , m14 = x
    , m24 = y
    , m34 = z
    , m44 = 1
    }


{-| "Flip" the matrix across the diagonal by swapping row index and column
index.
-}
transpose : Mat4 -> Mat4
transpose m =
    { m11 = m.m11
    , m21 = m.m12
    , m31 = m.m13
    , m41 = m.m14
    , m12 = m.m21
    , m22 = m.m22
    , m32 = m.m23
    , m42 = m.m24
    , m13 = m.m31
    , m23 = m.m32
    , m33 = m.m33
    , m43 = m.m34
    , m14 = m.m41
    , m24 = m.m42
    , m34 = m.m43
    , m44 = m.m44
    }
