module EigenDecompositionTest exposing (eigenDecomposition, eigenDecompositionJacobi)

import Expect exposing (FloatingPointTolerance(..))
import Extra.Expect as Expect
import Internal.Body as Body
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Shape as Shape
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex as Convex
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


eigenDecomposition : Test
eigenDecomposition =
    describe "Mat3.eigenDecomposition"
        [ describe "diagonal matrices"
            [ test "identity matrix" <|
                \_ ->
                    let
                        result =
                            Mat3.eigenDecomposition
                                { m11 = 1, m21 = 0, m31 = 0
                                , m12 = 0, m22 = 1, m32 = 0
                                , m13 = 0, m23 = 0, m33 = 1
                                }
                    in
                    Expect.vec3 { x = 1, y = 1, z = 1 } result.eigenvalues
            , test "distinct diagonal entries preserve axis pairing" <|
                \_ ->
                    let
                        result =
                            Mat3.eigenDecomposition
                                { m11 = 3, m21 = 0, m31 = 0
                                , m12 = 0, m22 = 1, m32 = 0
                                , m13 = 0, m23 = 0, m33 = 2
                                }
                    in
                    Expect.vec3 { x = 3, y = 1, z = 2 } result.eigenvalues
            ]
        , describe "symmetric matrices with off-diagonal terms"
            [ test "eigenvalues of a known symmetric matrix" <|
                \_ ->
                    let
                        result =
                            Mat3.eigenDecomposition
                                { m11 = 2, m21 = 1, m31 = 0
                                , m12 = 1, m22 = 3, m32 = 1
                                , m13 = 0, m23 = 1, m33 = 2
                                }
                    in
                    Expect.vec3
                        { x = 4, y = 2, z = 1 }
                        result.eigenvalues
            , test "eigenvalues of a matrix with all off-diagonal terms" <|
                \_ ->
                    let
                        result =
                            Mat3.eigenDecomposition
                                { m11 = 2, m21 = -1, m31 = 0
                                , m12 = -1, m22 = 2, m32 = -1
                                , m13 = 0, m23 = -1, m33 = 2
                                }
                    in
                    Expect.vec3
                        { x = 2 + sqrt 2, y = 2, z = 2 - sqrt 2 }
                        result.eigenvalues
            ]
        , describe "degenerate eigenvalues"
            [ test "axis-aligned cylinder: two equal eigenvalues, unique axis is z" <|
                \_ ->
                    let
                        m =
                            Mat3.cylinderInertia 5.0 1.0 3.0

                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecomposition m
                    in
                    Expect.all
                        [ \_ -> Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
                        , \_ -> expectOrthonormal v1 v2 v3
                        , \_ ->
                            Expect.within (Absolute 0.0001) eigenvalues.x eigenvalues.y
                                |> Expect.onFail "radial eigenvalues should be equal"
                        ]
                        ()
            , test "rotated cylinder: unique eigenvector aligns with cylinder axis" <|
                \_ ->
                    let
                        localInertia =
                            Mat3.cylinderInertia 5.0 1.0 3.0

                        rotation =
                            Transform3d.rotateAroundOwn { x = 1, y = 1, z = 0 } (pi / 3) Transform3d.atOrigin

                        m =
                            Transform3d.inertiaRotateIn rotation localInertia

                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecomposition m

                        cylinderAxis =
                            Transform3d.directionPlaceIn rotation { x = 0, y = 0, z = 1 }

                        uniqueEigenvector =
                            uniqueEigenvectorOf eigenvalues v1 v2 v3
                    in
                    Expect.all
                        [ \_ -> Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
                        , \_ -> expectOrthonormal v1 v2 v3
                        , \_ ->
                            abs (Vec3.dot uniqueEigenvector cylinderAxis)
                                |> Expect.within (Absolute 0.0001) 1.0
                        ]
                        ()
            ]
        , describe "reconstruction R * diag(d) * R^T == original"
            [ test "reconstructs identity" <|
                \_ ->
                    let
                        m =
                            { m11 = 1, m21 = 0, m31 = 0
                            , m12 = 0, m22 = 1, m32 = 0
                            , m13 = 0, m23 = 0, m33 = 1
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
            , test "reconstructs diagonal matrix" <|
                \_ ->
                    let
                        m =
                            { m11 = 5, m21 = 0, m31 = 0
                            , m12 = 0, m22 = 3, m32 = 0
                            , m13 = 0, m23 = 0, m33 = 1
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
            , test "reconstructs symmetric matrix with off-diagonal terms" <|
                \_ ->
                    let
                        m =
                            { m11 = 2, m21 = 1, m31 = 0
                            , m12 = 1, m22 = 3, m32 = 1
                            , m13 = 0, m23 = 1, m33 = 2
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
            , test "reconstructs matrix with all off-diagonal terms" <|
                \_ ->
                    let
                        m =
                            { m11 = 4, m21 = 2, m31 = 1
                            , m12 = 2, m22 = 5, m32 = 3
                            , m13 = 1, m23 = 3, m33 = 6
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
            , test "reconstructs inertia from pointInertia" <|
                \_ ->
                    let
                        m =
                            Mat3.pointInertia 2.5 1.0 2.0 3.0
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
            , test "reconstructs inertia from tetrahedronInertia" <|
                \_ ->
                    let
                        m =
                            Mat3.tetrahedronInertia 1.0
                                { x = 0, y = 0, z = 0 }
                                { x = 1, y = 0, z = 0 }
                                { x = 0, y = 2, z = 0 }
                                { x = 0, y = 0, z = 3 }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecomposition m))
            , test "L-shaped compound body has positive invInertia components" <|
                \_ ->
                    let
                        mat =
                            { friction = 0, bounciness = 0, density = 700 }

                        body =
                            Body.compound
                                [ ( Convex.fromBlock 2 1 1
                                        |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })
                                        |> Shape.Convex
                                  , mat
                                  , 1
                                  )
                                , ( Convex.fromBlock 1 2 1
                                        |> Convex.placeIn (Transform3d.atPoint { x = 0, y = 1, z = 0 })
                                        |> Shape.Convex
                                  , mat
                                  , 1
                                  )
                                ]
                    in
                    Expect.all
                        [ \_ -> body.invInertia.x |> Expect.greaterThan 0
                        , \_ -> body.invInertia.y |> Expect.greaterThan 0
                        , \_ -> body.invInertia.z |> Expect.greaterThan 0
                        ]
                        ()
            ]
        , describe "eigenvectors form an orthonormal basis"
            [ test "columns are orthonormal for off-diagonal matrix" <|
                \_ ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecomposition
                                { m11 = 4, m21 = 2, m31 = 1
                                , m12 = 2, m22 = 5, m32 = 3
                                , m13 = 1, m23 = 3, m33 = 6
                                }
                    in
                    expectOrthonormal v1 v2 v3
            , test "columns are orthonormal for inertia tensor" <|
                \_ ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecomposition
                                (Mat3.pointInertia 2.5 1.0 2.0 3.0)
                    in
                    expectOrthonormal v1 v2 v3
            ]
        , describe "eigenvectors satisfy A*v = λ*v"
            [ test "for symmetric matrix with off-diagonal terms" <|
                \_ ->
                    let
                        m =
                            { m11 = 4, m21 = 2, m31 = 1
                            , m12 = 2, m22 = 5, m32 = 3
                            , m13 = 1, m23 = 3, m33 = 6
                            }

                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecomposition m
                    in
                    Expect.all
                        [ \_ -> Expect.vec3 (Vec3.scale eigenvalues.x v1) (mulVec m v1)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.y v2) (mulVec m v2)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.z v3) (mulVec m v3)
                        ]
                        ()
            ]
        , describe "eigenvectors form a proper rotation (determinant +1)"
            [ test "for off-diagonal matrix" <|
                \_ ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecomposition
                                { m11 = 4, m21 = 2, m31 = 1
                                , m12 = 2, m22 = 5, m32 = 3
                                , m13 = 1, m23 = 3, m33 = 6
                                }
                    in
                    Vec3.dot v1 (Vec3.cross v2 v3)
                        |> Expect.within (Absolute 0.0001) 1.0
            , test "for inertia tensor" <|
                \_ ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecomposition
                                (Mat3.pointInertia 2.5 1.0 2.0 3.0)
                    in
                    Vec3.dot v1 (Vec3.cross v2 v3)
                        |> Expect.within (Absolute 0.0001) 1.0
            ]
        , describe "fuzz"
            [ fuzz rotatedCylinderInertia "reconstructs randomly rotated cylinder inertia" <|
                \{ inertia } ->
                    Expect.mat3 inertia (reconstruct (Mat3.eigenDecomposition inertia))
            , fuzz rotatedCylinderInertia "eigenvectors are orthonormal for randomly rotated cylinder" <|
                \{ inertia } ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecomposition inertia
                    in
                    expectOrthonormal v1 v2 v3
            , fuzz rotatedCylinderInertia "A*v = λ*v for randomly rotated cylinder" <|
                \{ inertia } ->
                    let
                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecomposition inertia
                    in
                    Expect.all
                        [ \_ -> Expect.vec3 (Vec3.scale eigenvalues.x v1) (mulVec inertia v1)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.y v2) (mulVec inertia v2)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.z v3) (mulVec inertia v3)
                        ]
                        ()
            , fuzz rotatedCylinderInertia "unique eigenvector aligns with cylinder axis" <|
                \{ inertia, cylinderAxis } ->
                    let
                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecomposition inertia

                        uniqueEigenvector =
                            uniqueEigenvectorOf eigenvalues v1 v2 v3
                    in
                    abs (Vec3.dot uniqueEigenvector cylinderAxis)
                        |> Expect.within (Absolute 0.0001) 1.0
            ]
        ]


reconstruct : { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 } -> Mat3
reconstruct { eigenvalues, v1, v2, v3 } =
    let
        r =
            { m11 = v1.x, m12 = v2.x, m13 = v3.x
            , m21 = v1.y, m22 = v2.y, m23 = v3.y
            , m31 = v1.z, m32 = v2.z, m33 = v3.z
            }

        rt =
            Mat3.transpose r
    in
    Mat3.mul r (Mat3.mul (diag eigenvalues) rt)


diag : Vec3 -> Mat3
diag { x, y, z } =
    { m11 = x, m21 = 0, m31 = 0
    , m12 = 0, m22 = y, m32 = 0
    , m13 = 0, m23 = 0, m33 = z
    }


mulVec : Mat3 -> Vec3 -> Vec3
mulVec m v =
    { x = m.m11 * v.x + m.m12 * v.y + m.m13 * v.z
    , y = m.m21 * v.x + m.m22 * v.y + m.m23 * v.z
    , z = m.m31 * v.x + m.m32 * v.y + m.m33 * v.z
    }


rotatedCylinderInertia : Fuzzer { inertia : Mat3, cylinderAxis : Vec3 }
rotatedCylinderInertia =
    Fuzz.map2
        (\axisAngle rotAngle ->
            let
                axis =
                    Vec3.normalize
                        { x = cos axisAngle
                        , y = sin axisAngle
                        , z = cos (axisAngle * 1.7)
                        }

                rotation =
                    Transform3d.rotateAroundOwn axis rotAngle Transform3d.atOrigin
            in
            { inertia = Transform3d.inertiaRotateIn rotation (Mat3.cylinderInertia 5.0 1.0 3.0)
            , cylinderAxis = Transform3d.directionPlaceIn rotation { x = 0, y = 0, z = 1 }
            }
        )
        (Fuzz.floatRange 0 (2 * pi))
        (Fuzz.floatRange 0 (2 * pi))


uniqueEigenvectorOf : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3
uniqueEigenvectorOf eigenvalues ev1 ev2 ev3 =
    let
        d12 =
            abs (eigenvalues.x - eigenvalues.y)

        d13 =
            abs (eigenvalues.x - eigenvalues.z)

        d23 =
            abs (eigenvalues.y - eigenvalues.z)
    in
    if d23 <= d12 && d23 <= d13 then
        ev1

    else if d13 <= d12 && d13 <= d23 then
        ev2

    else
        ev3


expectOrthonormal : Vec3 -> Vec3 -> Vec3 -> Expect.Expectation
expectOrthonormal c1 c2 c3 =
    Expect.all
        [ \_ -> Vec3.length c1 |> Expect.within (Absolute 0.0001) 1.0
        , \_ -> Vec3.length c2 |> Expect.within (Absolute 0.0001) 1.0
        , \_ -> Vec3.length c3 |> Expect.within (Absolute 0.0001) 1.0
        , \_ -> Vec3.dot c1 c2 |> Expect.within (Absolute 0.0001) 0.0
        , \_ -> Vec3.dot c1 c3 |> Expect.within (Absolute 0.0001) 0.0
        , \_ -> Vec3.dot c2 c3 |> Expect.within (Absolute 0.0001) 0.0
        ]
        ()


eigenDecompositionJacobi : Test
eigenDecompositionJacobi =
    describe "Mat3.eigenDecompositionJacobi"
        [ describe "reconstruction R * diag(d) * R^T == original"
            [ test "reconstructs identity" <|
                \_ ->
                    let
                        m =
                            { m11 = 1, m21 = 0, m31 = 0
                            , m12 = 0, m22 = 1, m32 = 0
                            , m13 = 0, m23 = 0, m33 = 1
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
            , test "reconstructs diagonal matrix" <|
                \_ ->
                    let
                        m =
                            { m11 = 5, m21 = 0, m31 = 0
                            , m12 = 0, m22 = 3, m32 = 0
                            , m13 = 0, m23 = 0, m33 = 1
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
            , test "reconstructs symmetric matrix with off-diagonal terms" <|
                \_ ->
                    let
                        m =
                            { m11 = 2, m21 = 1, m31 = 0
                            , m12 = 1, m22 = 3, m32 = 1
                            , m13 = 0, m23 = 1, m33 = 2
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
            , test "reconstructs matrix with all off-diagonal terms" <|
                \_ ->
                    let
                        m =
                            { m11 = 4, m21 = 2, m31 = 1
                            , m12 = 2, m22 = 5, m32 = 3
                            , m13 = 1, m23 = 3, m33 = 6
                            }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
            , test "reconstructs inertia from pointInertia" <|
                \_ ->
                    let
                        m =
                            Mat3.pointInertia 2.5 1.0 2.0 3.0
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
            , test "reconstructs inertia from tetrahedronInertia" <|
                \_ ->
                    let
                        m =
                            Mat3.tetrahedronInertia 1.0
                                { x = 0, y = 0, z = 0 }
                                { x = 1, y = 0, z = 0 }
                                { x = 0, y = 2, z = 0 }
                                { x = 0, y = 0, z = 3 }
                    in
                    Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
            ]
        , describe "degenerate eigenvalues"
            [ test "axis-aligned cylinder" <|
                \_ ->
                    let
                        m =
                            Mat3.cylinderInertia 5.0 1.0 3.0

                        { v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi m
                    in
                    Expect.all
                        [ \_ -> Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
                        , \_ -> expectOrthonormal v1 v2 v3
                        ]
                        ()
            , test "rotated cylinder: unique eigenvector aligns with cylinder axis" <|
                \_ ->
                    let
                        localInertia =
                            Mat3.cylinderInertia 5.0 1.0 3.0

                        rotation =
                            Transform3d.rotateAroundOwn { x = 1, y = 1, z = 0 } (pi / 3) Transform3d.atOrigin

                        m =
                            Transform3d.inertiaRotateIn rotation localInertia

                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi m

                        cylinderAxis =
                            Transform3d.directionPlaceIn rotation { x = 0, y = 0, z = 1 }

                        uniqueEigenvector =
                            uniqueEigenvectorOf eigenvalues v1 v2 v3
                    in
                    Expect.all
                        [ \_ -> Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
                        , \_ -> expectOrthonormal v1 v2 v3
                        , \_ ->
                            abs (Vec3.dot uniqueEigenvector cylinderAxis)
                                |> Expect.within (Absolute 0.0001) 1.0
                        ]
                        ()
            , test "sphere inertia" <|
                \_ ->
                    let
                        m =
                            Mat3.sphereInertia 5.0 2.0

                        { v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi m
                    in
                    Expect.all
                        [ \_ -> Expect.mat3 m (reconstruct (Mat3.eigenDecompositionJacobi m))
                        , \_ -> expectOrthonormal v1 v2 v3
                        ]
                        ()
            ]
        , describe "eigenvectors satisfy A*v = λ*v"
            [ test "for symmetric matrix with off-diagonal terms" <|
                \_ ->
                    let
                        m =
                            { m11 = 4, m21 = 2, m31 = 1
                            , m12 = 2, m22 = 5, m32 = 3
                            , m13 = 1, m23 = 3, m33 = 6
                            }

                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi m
                    in
                    Expect.all
                        [ \_ -> Expect.vec3 (Vec3.scale eigenvalues.x v1) (mulVec m v1)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.y v2) (mulVec m v2)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.z v3) (mulVec m v3)
                        ]
                        ()
            ]
        , describe "eigenvectors form a proper rotation (determinant +1)"
            [ test "for off-diagonal matrix" <|
                \_ ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi
                                { m11 = 4, m21 = 2, m31 = 1
                                , m12 = 2, m22 = 5, m32 = 3
                                , m13 = 1, m23 = 3, m33 = 6
                                }
                    in
                    Vec3.dot v1 (Vec3.cross v2 v3)
                        |> Expect.within (Absolute 0.0001) 1.0
            , test "for inertia tensor" <|
                \_ ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi
                                (Mat3.pointInertia 2.5 1.0 2.0 3.0)
                    in
                    Vec3.dot v1 (Vec3.cross v2 v3)
                        |> Expect.within (Absolute 0.0001) 1.0
            ]
        , describe "fuzz"
            [ fuzz rotatedCylinderInertia "reconstructs randomly rotated cylinder inertia" <|
                \{ inertia } ->
                    Expect.mat3 inertia (reconstruct (Mat3.eigenDecompositionJacobi inertia))
            , fuzz rotatedCylinderInertia "eigenvectors are orthonormal for randomly rotated cylinder" <|
                \{ inertia } ->
                    let
                        { v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi inertia
                    in
                    expectOrthonormal v1 v2 v3
            , fuzz rotatedCylinderInertia "A*v = λ*v for randomly rotated cylinder" <|
                \{ inertia } ->
                    let
                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi inertia
                    in
                    Expect.all
                        [ \_ -> Expect.vec3 (Vec3.scale eigenvalues.x v1) (mulVec inertia v1)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.y v2) (mulVec inertia v2)
                        , \_ -> Expect.vec3 (Vec3.scale eigenvalues.z v3) (mulVec inertia v3)
                        ]
                        ()
            , fuzz rotatedCylinderInertia "unique eigenvector aligns with cylinder axis" <|
                \{ inertia, cylinderAxis } ->
                    let
                        { eigenvalues, v1, v2, v3 } =
                            Mat3.eigenDecompositionJacobi inertia

                        uniqueEigenvector =
                            uniqueEigenvectorOf eigenvalues v1 v2 v3
                    in
                    abs (Vec3.dot uniqueEigenvector cylinderAxis)
                        |> Expect.within (Absolute 0.0001) 1.0
            ]
        ]
