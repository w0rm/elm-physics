module EigenDecomposition exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


main : BenchmarkProgram
main =
    program <|
        describe "EigenDecomposition"
            [ sphere
            , rotatedCylinder
            ]


sphere : Benchmark
sphere =
    let
        m =
            Mat3.sphereInertia 5.0 2.0
    in
    Benchmark.compare "sphere"
        "jacobi (Mat3 + flat rotation)"
        (\_ -> Mat3.eigenDecompositionJacobi m)
        "jacobi3 (single record)"
        (\_ -> Mat3.eigenDecompositionJacobi3 m)


rotatedCylinder : Benchmark
rotatedCylinder =
    let
        rotation =
            Transform3d.rotateAroundOwn
                (Vec3.normalize { x = 1, y = 1, z = 0 })
                (pi / 3)
                Transform3d.atOrigin

        m =
            Transform3d.inertiaRotateIn rotation (Mat3.cylinderInertia 5.0 1.0 3.0)
    in
    Benchmark.compare "rotated cylinder"
        "jacobi (Mat3 + flat rotation)"
        (\_ -> Mat3.eigenDecompositionJacobi m)
        "jacobi3 (single record)"
        (\_ -> Mat3.eigenDecompositionJacobi3 m)
