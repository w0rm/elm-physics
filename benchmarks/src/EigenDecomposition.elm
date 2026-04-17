module EigenDecomposition exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


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
        "analytical"
        (\_ -> analyticalEigenDecomposition m)
        "jacobi"
        (\_ -> Mat3.eigenDecomposition m)


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
        "analytical"
        (\_ -> analyticalEigenDecomposition m)
        "jacobi"
        (\_ -> Mat3.eigenDecomposition m)


analyticalEigenDecomposition : Mat3 -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
analyticalEigenDecomposition m =
    let
        p1 =
            m.m12 * m.m12 + m.m13 * m.m13 + m.m23 * m.m23
    in
    if p1 == 0 then
        { eigenvalues = { x = m.m11, y = m.m22, z = m.m33 }
        , v1 = Vec3.xAxis
        , v2 = Vec3.yAxis
        , v3 = Vec3.zAxis
        }

    else
        nonDiagonalEigen m p1


nonDiagonalEigen : Mat3 -> Float -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
nonDiagonalEigen m p1 =
    let
        q =
            (m.m11 + m.m22 + m.m33) / 3

        p2 =
            (m.m11 - q) * (m.m11 - q) + (m.m22 - q) * (m.m22 - q) + (m.m33 - q) * (m.m33 - q) + 2 * p1

        p =
            sqrt (p2 / 6)

        invP =
            1 / p

        b11 =
            (m.m11 - q) * invP

        b22 =
            (m.m22 - q) * invP

        b33 =
            (m.m33 - q) * invP

        b12 =
            m.m12 * invP

        b13 =
            m.m13 * invP

        b23 =
            m.m23 * invP

        detB =
            b11 * (b22 * b33 - b23 * b23) - b12 * (b12 * b33 - b23 * b13) + b13 * (b12 * b23 - b22 * b13)

        r =
            clamp -1 1 (detB / 2)

        phi =
            acos r / 3

        eig1 =
            q + 2 * p * cos phi

        eig3 =
            q + 2 * p * cos (phi + 2 * pi / 3)

        eig2 =
            3 * q - eig1 - eig3

        gap1 =
            eig1 - eig2

        gap2 =
            eig2 - eig3

        eigenvalues =
            { x = eig1, y = eig2, z = eig3 }

        result ev1 ev2 ev3 =
            { eigenvalues = eigenvalues, v1 = ev1, v2 = ev2, v3 = ev3 }
    in
    if gap1 < 1.0e-10 && gap2 < 1.0e-10 then
        result Vec3.xAxis Vec3.yAxis Vec3.zAxis

    else if gap1 < 1.0e-10 then
        let
            u3 =
                eigenvectorForEigenvalue m eig3

            ( t1, t2 ) =
                Vec3.tangents u3
        in
        result t1 t2 u3

    else if gap2 < 1.0e-10 then
        let
            u1 =
                eigenvectorForEigenvalue m eig1

            ( t2, t3 ) =
                Vec3.tangents u1
        in
        result u1 t2 t3

    else
        let
            u1 =
                eigenvectorForEigenvalue m eig1

            u3 =
                eigenvectorForEigenvalue m eig3

            u2 =
                Vec3.cross u3 u1
        in
        result u1 u2 u3


eigenvectorForEigenvalue : Mat3 -> Float -> Vec3
eigenvectorForEigenvalue m eigenvalue =
    let
        a11 =
            m.m11 - eigenvalue

        a22 =
            m.m22 - eigenvalue

        a33 =
            m.m33 - eigenvalue

        row0 =
            { x = a11, y = m.m12, z = m.m13 }

        row1 =
            { x = m.m12, y = a22, z = m.m23 }

        row2 =
            { x = m.m13, y = m.m23, z = a33 }

        r0xr1 =
            Vec3.cross row0 row1

        r0xr2 =
            Vec3.cross row0 row2

        r1xr2 =
            Vec3.cross row1 row2

        d0 =
            Vec3.lengthSquared r0xr1

        d1 =
            Vec3.lengthSquared r0xr2

        d2 =
            Vec3.lengthSquared r1xr2
    in
    if d0 >= d1 && d0 >= d2 then
        Vec3.scale (1 / sqrt d0) r0xr1

    else if d1 >= d2 then
        Vec3.scale (1 / sqrt d1) r0xr2

    else
        Vec3.scale (1 / sqrt d2) r1xr2
