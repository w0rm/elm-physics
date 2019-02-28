module AltMath.Matrix4 exposing
    ( Mat4, identity
    , inverse, inverseOrthonormal, mul, mulAffine, transpose, makeBasis, transform
    , makeFrustum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt
    , rotate, scale, scale3, translate, translate3
    , makeRotate, makeScale, makeScale3, makeTranslate, makeTranslate3
    , toRecord, fromRecord
    )

{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All matrices are immutable.

This library uses the convention that the prefix `make` is creating a new
array,as without the prefix, you are applying some transform to an
existing matrix.


# Create

@docs Mat4, identity


# Operations

@docs inverse, inverseOrthonormal, mul, mulAffine, transpose, makeBasis, transform


# Projections

@docs makeFrustum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt


# Apply Transformations

@docs rotate, scale, scale3, translate, translate3


# Create Transformations

@docs makeRotate, makeScale, makeScale3, makeTranslate, makeTranslate3


# Conversions

@docs toRecord, fromRecord

-}

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


{-| Computes the inverse of any matrix. This is somewhat computationally
intensive. If the matrix is not invertible, `Nothing` is returned.
-}
inverse : Mat4 -> Maybe Mat4
inverse m =
    let
        r11 =
            m.m22 * m.m33 * m.m44 - m.m22 * m.m34 * m.m34 - m.m23 * m.m32 * m.m44 + m.m23 * m.m42 * m.m34 + m.m24 * m.m32 * m.m34 - m.m24 * m.m42 * m.m33

        r21 =
            -m.m21 * m.m33 * m.m44 + m.m21 * m.m34 * m.m34 + m.m23 * m.m31 * m.m44 - m.m23 * m.m41 * m.m34 - m.m24 * m.m31 * m.m34 + m.m24 * m.m41 * m.m33

        r31 =
            m.m21 * m.m32 * m.m44 - m.m21 * m.m42 * m.m34 - m.m22 * m.m31 * m.m44 + m.m22 * m.m41 * m.m34 + m.m24 * m.m31 * m.m42 - m.m24 * m.m41 * m.m32

        r41 =
            -m.m21 * m.m32 * m.m34 + m.m21 * m.m42 * m.m33 + m.m22 * m.m31 * m.m34 - m.m22 * m.m41 * m.m33 - m.m23 * m.m31 * m.m42 + m.m23 * m.m41 * m.m32

        r12 =
            -m.m12 * m.m33 * m.m44 + m.m12 * m.m34 * m.m34 + m.m13 * m.m32 * m.m44 - m.m13 * m.m42 * m.m34 - m.m14 * m.m32 * m.m34 + m.m14 * m.m42 * m.m33

        r22 =
            m.m11 * m.m33 * m.m44 - m.m11 * m.m34 * m.m34 - m.m13 * m.m31 * m.m44 + m.m13 * m.m41 * m.m34 + m.m14 * m.m31 * m.m34 - m.m14 * m.m41 * m.m33

        r32 =
            -m.m11 * m.m32 * m.m44 + m.m11 * m.m42 * m.m34 + m.m12 * m.m31 * m.m44 - m.m12 * m.m41 * m.m34 - m.m14 * m.m31 * m.m42 + m.m14 * m.m41 * m.m32

        r42 =
            m.m11 * m.m32 * m.m34 - m.m11 * m.m42 * m.m33 - m.m12 * m.m31 * m.m34 + m.m12 * m.m41 * m.m33 + m.m13 * m.m31 * m.m42 - m.m13 * m.m41 * m.m32

        r13 =
            m.m12 * m.m23 * m.m44 - m.m12 * m.m34 * m.m24 - m.m13 * m.m22 * m.m44 + m.m13 * m.m42 * m.m24 + m.m14 * m.m22 * m.m34 - m.m14 * m.m42 * m.m23

        r23 =
            -m.m11 * m.m23 * m.m44 + m.m11 * m.m34 * m.m24 + m.m13 * m.m21 * m.m44 - m.m13 * m.m41 * m.m24 - m.m14 * m.m21 * m.m34 + m.m14 * m.m41 * m.m23

        r33 =
            m.m11 * m.m22 * m.m44 - m.m11 * m.m42 * m.m24 - m.m12 * m.m21 * m.m44 + m.m12 * m.m41 * m.m24 + m.m14 * m.m21 * m.m42 - m.m14 * m.m41 * m.m22

        r43 =
            -m.m11 * m.m22 * m.m34 + m.m11 * m.m42 * m.m23 + m.m12 * m.m21 * m.m34 - m.m12 * m.m41 * m.m23 - m.m13 * m.m21 * m.m42 + m.m13 * m.m41 * m.m22

        r14 =
            -m.m12 * m.m23 * m.m34 + m.m12 * m.m33 * m.m24 + m.m13 * m.m22 * m.m34 - m.m13 * m.m32 * m.m24 - m.m14 * m.m22 * m.m33 + m.m14 * m.m32 * m.m23

        r24 =
            m.m11 * m.m23 * m.m34 - m.m11 * m.m33 * m.m24 - m.m13 * m.m21 * m.m34 + m.m13 * m.m31 * m.m24 + m.m14 * m.m21 * m.m33 - m.m14 * m.m31 * m.m23

        r34 =
            -m.m11 * m.m22 * m.m34 + m.m11 * m.m32 * m.m24 + m.m12 * m.m21 * m.m34 - m.m12 * m.m31 * m.m24 - m.m14 * m.m21 * m.m32 + m.m14 * m.m31 * m.m22

        r44 =
            m.m11 * m.m22 * m.m33 - m.m11 * m.m32 * m.m23 - m.m12 * m.m21 * m.m33 + m.m12 * m.m31 * m.m23 + m.m13 * m.m21 * m.m32 - m.m13 * m.m31 * m.m22

        det =
            m.m11 * r11 + m.m21 * r12 + m.m31 * r13 + m.m41 * r14

        idet =
            1 / det
    in
    if det == 0 then
        Nothing

    else
        Just
            { m11 = r11 * det
            , m21 = r21 * det
            , m31 = r31 * det
            , m41 = r41 * det
            , m12 = r12 * det
            , m22 = r22 * det
            , m32 = r32 * det
            , m42 = r42 * det
            , m13 = r13 * det
            , m23 = r23 * det
            , m33 = r33 * det
            , m43 = r43 * det
            , m14 = r14 * det
            , m24 = r24 * det
            , m34 = r34 * det
            , m44 = r44 * det
            }


{-| Computes the inverse of the given matrix, assuming that the matrix is
orthonormal. This algorithm is more efficient than general matrix inversion, and
has no possibility of failing.
-}
inverseOrthonormal : Mat4 -> Mat4
inverseOrthonormal m =
    { m11 = m.m11
    , m21 = m.m12
    , m31 = m.m13
    , m41 = 0
    , m12 = m.m21
    , m22 = m.m22
    , m32 = m.m23
    , m42 = 0
    , m13 = m.m31
    , m23 = m.m32
    , m33 = m.m33
    , m43 = 0
    , m14 = -(m.m11 * m.m14 + m.m21 * m.m24 + m.m31 * m.m34)
    , m24 = -(m.m12 * m.m14 + m.m22 * m.m24 + m.m32 * m.m34)
    , m34 = -(m.m13 * m.m14 + m.m23 * m.m24 + m.m33 * m.m34)
    , m44 = m.m44
    }


{-| Creates a matrix for a projection frustum with the given parameters.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum
  - znear - the near z distance of the frustum
  - zfar - the far z distance of the frustum

-}
makeFrustum : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeFrustum left right bottom top znear zfar =
    { m11 = 2 * znear / (right - left)
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 2 * znear / (top - bottom)
    , m32 = 0
    , m42 = 0
    , m13 = (right + left) / (right - left)
    , m23 = (top + bottom) / (top - bottom)
    , m33 = -(zfar + znear) / (zfar - znear)
    , m43 = -1
    , m14 = 0
    , m24 = 0
    , m34 = -2 * zfar * znear / (zfar - znear)
    , m44 = 0
    }


{-| Creates a matrix for a perspective projection with the given parameters.

Parameters:

  - fovy - field of view in the y axis, in degrees
  - aspect - aspect ratio
  - znear - the near z distance of the projection
  - zfar - the far z distance of the projection

-}
makePerspective : Float -> Float -> Float -> Float -> Mat4
makePerspective fovy aspect znear zfar =
    let
        ymax =
            znear * tan (fovy * pi / 360.0)

        ymin =
            -ymax

        xmin =
            ymin * aspect

        xmax =
            ymax * aspect
    in
    makeFrustum xmin xmax ymin ymax znear zfar


{-| Creates a matrix for an orthogonal frustum projection with the given parameters.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum
  - znear - the near z distance of the frustum
  - zfar - the far z distance of the frustum

-}
makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeOrtho left right bottom top znear zfar =
    { m11 = 2 / (right - left)
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 2 / (top - bottom)
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = -2 / (zfar - znear)
    , m43 = 0
    , m14 = -(right + left) / (right - left)
    , m24 = -(top + bottom) / (top - bottom)
    , m34 = -(zfar + znear) / (zfar - znear)
    , m44 = 1
    }


{-| Creates a matrix for a 2D orthogonal frustum projection with the given
parameters. `znear` and `zfar` are assumed to be -1 and 1, respectively.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum

-}
makeOrtho2D : Float -> Float -> Float -> Float -> Mat4
makeOrtho2D left right bottom top =
    makeOrtho left right bottom top -1 1


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


{-| Matrix multiplication, assuming a and b are affine: a \* b
-}
mulAffine : Mat4 -> Mat4 -> Mat4
mulAffine a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31
    , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31
    , m31 = a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31
    , m41 = 0
    , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32
    , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32
    , m32 = a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32
    , m42 = 0
    , m13 = a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33
    , m23 = a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33
    , m33 = a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33
    , m43 = 0
    , m14 = a.m11 * b.m14 + a.m12 * b.m24 + a.m13 * b.m34 + a.m14
    , m24 = a.m21 * b.m14 + a.m22 * b.m24 + a.m23 * b.m34 + a.m24
    , m34 = a.m31 * b.m14 + a.m32 * b.m24 + a.m33 * b.m34 + a.m34
    , m44 = 1
    }


{-| Creates a transformation matrix for rotation in radians about the
3-element vector axis.
-}
makeRotate : Float -> Vec3 -> Mat4
makeRotate angle { x, y, z } =
    let
        c =
            cos angle

        c1 =
            1 - c

        s =
            sin angle
    in
    { m11 = x * x * c1 + c
    , m21 = y * x * c1 + z * s
    , m31 = z * x * c1 - y * s
    , m41 = 0
    , m12 = x * y * c1 - z * s
    , m22 = y * y * c1 + c
    , m32 = y * z * c1 + x * s
    , m42 = 0
    , m13 = x * z * c1 + y * s
    , m23 = y * z * c1 - x * s
    , m33 = z * z * c1 + c
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }


{-| Concatenates a rotation in radians about an axis to the given matrix.
-}
rotate : Float -> Vec3 -> Mat4 -> Mat4
rotate angle axis m =
    let
        im =
            1.0 / Vec3.length axis

        x =
            axis.x * im

        y =
            axis.y * im

        z =
            axis.z * im

        c =
            cos angle

        c1 =
            1 - c

        s =
            sin angle

        xs =
            x * s

        ys =
            y * s

        zs =
            z * s

        xyc1 =
            x * y * c1

        xzc1 =
            x * z * c1

        yzc1 =
            y * z * c1

        t11 =
            x * x * c1 + c

        t21 =
            xyc1 + zs

        t31 =
            xzc1 - ys

        t12 =
            xyc1 - zs

        t22 =
            y * y * c1 + c

        t32 =
            yzc1 + xs

        t13 =
            xzc1 + ys

        t23 =
            yzc1 - xs

        t33 =
            z * z * c1 + c
    in
    { m11 = m.m11 * t11 + m.m12 * t21 + m.m13 * t31
    , m21 = m.m21 * t11 + m.m22 * t21 + m.m23 * t31
    , m31 = m.m31 * t11 + m.m32 * t21 + m.m33 * t31
    , m41 = m.m41 * t11 + m.m42 * t21 + m.m43 * t31
    , m12 = m.m11 * t12 + m.m12 * t22 + m.m13 * t32
    , m22 = m.m21 * t12 + m.m22 * t22 + m.m23 * t32
    , m32 = m.m31 * t12 + m.m32 * t22 + m.m33 * t32
    , m42 = m.m41 * t12 + m.m42 * t22 + m.m43 * t32
    , m13 = m.m11 * t13 + m.m12 * t23 + m.m13 * t33
    , m23 = m.m21 * t13 + m.m22 * t23 + m.m23 * t33
    , m33 = m.m31 * t13 + m.m32 * t23 + m.m33 * t33
    , m43 = m.m41 * t13 + m.m42 * t23 + m.m43 * t33
    , m14 = m.m14
    , m24 = m.m24
    , m34 = m.m34
    , m44 = m.m44
    }


{-| Creates a transformation matrix for scaling by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeScale3 : Float -> Float -> Float -> Mat4
makeScale3 x y z =
    { m11 = x
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = y
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = z
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }


{-| Creates a transformation matrix for scaling each of the x, y, and z axes by
the amount given in the corresponding element of the 3-element vector.
-}
makeScale : Vec3 -> Mat4
makeScale { x, y, z } =
    { m11 = x
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = y
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = z
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }


{-| Concatenates a scaling to the given matrix.
-}
scale3 : Float -> Float -> Float -> Mat4 -> Mat4
scale3 x y z m =
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


{-| Creates a transformation matrix for translating by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeTranslate3 : Float -> Float -> Float -> Mat4
makeTranslate3 x y z =
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


{-| Concatenates a translation to the given matrix.
-}
translate3 : Float -> Float -> Float -> Mat4 -> Mat4
translate3 x y z m =
    { m11 = m.m11
    , m21 = m.m21
    , m31 = m.m31
    , m41 = m.m41
    , m12 = m.m12
    , m22 = m.m22
    , m32 = m.m32
    , m42 = m.m42
    , m13 = m.m13
    , m23 = m.m23
    , m33 = m.m33
    , m43 = m.m43
    , m14 = m.m11 * x + m.m12 * y + m.m13 * z + m.m14
    , m24 = m.m21 * x + m.m22 * y + m.m23 * z + m.m24
    , m34 = m.m31 * x + m.m32 * y + m.m33 * z + m.m34
    , m44 = m.m41 * x + m.m42 * y + m.m43 * z + m.m44
    }


{-| Concatenates a translation to the given matrix.
-}
translate : Vec3 -> Mat4 -> Mat4
translate { x, y, z } m =
    { m11 = m.m11
    , m21 = m.m21
    , m31 = m.m31
    , m41 = m.m41
    , m12 = m.m12
    , m22 = m.m22
    , m32 = m.m32
    , m42 = m.m42
    , m13 = m.m13
    , m23 = m.m23
    , m33 = m.m33
    , m43 = m.m43
    , m14 = m.m11 * x + m.m12 * y + m.m13 * z + m.m14
    , m24 = m.m21 * x + m.m22 * y + m.m23 * z + m.m24
    , m34 = m.m31 * x + m.m32 * y + m.m33 * z + m.m34
    , m44 = m.m41 * x + m.m42 * y + m.m43 * z + m.m44
    }


{-| Creates a transformation matrix for a camera.

Parameters:

  - eye - The location of the camera
  - center - The location of the focused object
  - up - The "up" direction according to the camera

-}
makeLookAt : Vec3 -> Vec3 -> Vec3 -> Mat4
makeLookAt eye center up =
    let
        z =
            Vec3.direction eye center

        x =
            Vec3.normalize (Vec3.cross up z)

        y =
            Vec3.normalize (Vec3.cross z x)
    in
    mul
        { m11 = x.x
        , m21 = y.x
        , m31 = z.x
        , m41 = 0
        , m12 = x.y
        , m22 = y.y
        , m32 = z.y
        , m42 = 0
        , m13 = x.z
        , m23 = y.z
        , m33 = z.z
        , m43 = 0
        , m14 = 0
        , m24 = 0
        , m34 = 0
        , m44 = 1
        }
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
        , m14 = -eye.x
        , m24 = -eye.y
        , m34 = -eye.z
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


{-| Creates a transform from a basis consisting of 3 linearly independent vectors.
-}
makeBasis : Vec3 -> Vec3 -> Vec3 -> Mat4
makeBasis vx vy vz =
    { m11 = vx.x
    , m21 = vx.y
    , m31 = vx.z
    , m41 = 0
    , m12 = vy.x
    , m22 = vy.y
    , m32 = vy.z
    , m42 = 0
    , m13 = vz.x
    , m23 = vz.y
    , m33 = vz.z
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }


{-| Convert a matrix to a record. Elements are given by their row and column indices, starting at 1, so `m23` means the element in the second row, third column.
-}
toRecord : Mat4 -> Mat4
toRecord a =
    a


{-| Convert a record to a matrix.
-}
fromRecord : Mat4 -> Mat4
fromRecord a =
    a
