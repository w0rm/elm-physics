module Matrix3Test exposing (inverse)

import Expect
import Extra.Expect as Expect
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Test exposing (Test, describe, test)


inverse : Test
inverse =
    describe "Mat3.inverse"
        [ test "works for identity" <|
            \_ ->
                Expect.mat3 identity (Mat3.inverse identity)
        , test "works for arbitrary" <|
            \_ ->
                Expect.mat3
                    { m11 = -24, m21 = 20, m31 = -5, m12 = 18, m22 = -15, m32 = 4, m13 = 5, m23 = -4, m33 = 1 }
                    (Mat3.inverse { m11 = 1, m21 = 0, m31 = 5, m12 = 2, m22 = 1, m32 = 6, m13 = 3, m23 = 4, m33 = 0 })
        , test "inverse of inverse is the matrix itself" <|
            \_ ->
                Expect.mat3
                    { m11 = 1, m21 = 0, m31 = 5, m12 = 2, m22 = 1, m32 = 6, m13 = 3, m23 = 4, m33 = 0 }
                    (Mat3.inverse (Mat3.inverse { m11 = 1, m21 = 0, m31 = 5, m12 = 2, m22 = 1, m32 = 6, m13 = 3, m23 = 4, m33 = 0 }))
        ]


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
