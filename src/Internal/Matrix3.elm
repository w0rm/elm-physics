module Internal.Matrix3 exposing
    ( Mat3
    , add
    , cylinderInertia
    , eigenDecomposition
    , eigenDecompositionJacobi
    , eigenDecompositionJacobi2
    , eigenDecompositionJacobi3
    , inverse
    , mul
    , pointInertia
    , scale
    , sphereInertia
    , sub
    , tetrahedronInertia
    , transpose
    , zero
    )

import Internal.Vector3 as Vec3 exposing (Vec3)


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


zero : Mat3
zero =
    { m11 = 0
    , m21 = 0
    , m31 = 0
    , m12 = 0
    , m22 = 0
    , m32 = 0
    , m13 = 0
    , m23 = 0
    , m33 = 0
    }


inverse : Mat3 -> Mat3
inverse { m11, m21, m31, m12, m22, m32, m13, m23, m33 } =
    let
        det =
            (m11 * (m22 * m33 - m32 * m23))
                - (m12 * (m21 * m33 - m23 * m31))
                + (m13 * (m21 * m32 - m22 * m31))

        invdet =
            1 / det
    in
    if det == 0 then
        zero

    else
        { m11 = (m22 * m33 - m32 * m23) * invdet
        , m12 = (m13 * m32 - m12 * m33) * invdet
        , m13 = (m12 * m23 - m13 * m22) * invdet
        , m21 = (m23 * m31 - m21 * m33) * invdet
        , m22 = (m11 * m33 - m13 * m31) * invdet
        , m23 = (m21 * m13 - m11 * m23) * invdet
        , m31 = (m21 * m32 - m31 * m22) * invdet
        , m32 = (m31 * m12 - m11 * m32) * invdet
        , m33 = (m11 * m22 - m21 * m12) * invdet
        }


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


scale : Float -> Mat3 -> Mat3
scale k m =
    { m11 = k * m.m11
    , m21 = k * m.m21
    , m31 = k * m.m31
    , m12 = k * m.m12
    , m22 = k * m.m22
    , m32 = k * m.m32
    , m13 = k * m.m13
    , m23 = k * m.m23
    , m33 = k * m.m33
    }


add : Mat3 -> Mat3 -> Mat3
add a b =
    { m11 = a.m11 + b.m11
    , m21 = a.m21 + b.m21
    , m31 = a.m31 + b.m31
    , m12 = a.m12 + b.m12
    , m22 = a.m22 + b.m22
    , m32 = a.m32 + b.m32
    , m13 = a.m13 + b.m13
    , m23 = a.m23 + b.m23
    , m33 = a.m33 + b.m33
    }


sub : Mat3 -> Mat3 -> Mat3
sub a b =
    { m11 = a.m11 - b.m11
    , m21 = a.m21 - b.m21
    , m31 = a.m31 - b.m31
    , m12 = a.m12 - b.m12
    , m22 = a.m22 - b.m22
    , m32 = a.m32 - b.m32
    , m13 = a.m13 - b.m13
    , m23 = a.m23 - b.m23
    , m33 = a.m33 - b.m33
    }


{-| Flip the matrix across the diagonal by swapping row index and column
index.
-}
transpose : Mat3 -> Mat3
transpose m =
    { m11 = m.m11
    , m21 = m.m12
    , m31 = m.m13
    , m12 = m.m21
    , m22 = m.m22
    , m32 = m.m23
    , m13 = m.m31
    , m23 = m.m32
    , m33 = m.m33
    }


{-| Calculates the moment of inertia of a point mass at a certain position
-}
pointInertia : Float -> Float -> Float -> Float -> Mat3
pointInertia m x y z =
    let
        m21 =
            -m * x * y

        m31 =
            -m * x * z

        m32 =
            -m * y * z
    in
    { m11 = m * (y * y + z * z)
    , m21 = m21
    , m31 = m31
    , m12 = m21
    , m22 = m * (z * z + x * x)
    , m32 = m32
    , m13 = m31
    , m23 = m32
    , m33 = m * (x * x + y * y)
    }


sphereInertia : Float -> Float -> Mat3
sphereInertia m radius =
    let
        i =
            m * 2 / 5 * radius * radius
    in
    { m11 = i
    , m21 = 0
    , m31 = 0
    , m12 = 0
    , m22 = i
    , m32 = 0
    , m13 = 0
    , m23 = 0
    , m33 = i
    }


cylinderInertia : Float -> Float -> Float -> Mat3
cylinderInertia m radius height =
    let
        a =
            (m * (3 * radius ^ 2 + height ^ 2)) / 12
    in
    { m11 = a
    , m21 = 0
    , m31 = 0
    , m12 = 0
    , m22 = a
    , m32 = 0
    , m13 = 0
    , m23 = 0
    , m33 = (m * radius ^ 2) / 2
    }


tetrahedronInertia : Float -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Mat3
tetrahedronInertia m p0 p1 p2 p3 =
    let
        x1 =
            p1.x - p0.x

        x2 =
            p2.x - p0.x

        x3 =
            p3.x - p0.x

        y1 =
            p1.y - p0.y

        y2 =
            p2.y - p0.y

        y3 =
            p3.y - p0.y

        z1 =
            p1.z - p0.z

        z2 =
            p2.z - p0.z

        z3 =
            p3.z - p0.z

        ix =
            m / 10 * (x1 * x1 + x2 * x2 + x3 * x3 + x1 * x2 + x1 * x3 + x2 * x3)

        iy =
            m / 10 * (y1 * y1 + y2 * y2 + y3 * y3 + y1 * y2 + y1 * y3 + y2 * y3)

        iz =
            m / 10 * (z1 * z1 + z2 * z2 + z3 * z3 + z1 * z2 + z1 * z3 + z2 * z3)

        ixx =
            iy + iz

        iyy =
            ix + iz

        izz =
            ix + iy

        ixy =
            m / 20 * (2 * (x1 * y1 + x2 * y2 + x3 * y3) + x1 * y2 + x2 * y1 + x1 * y3 + x3 * y1 + x2 * y3 + x3 * y2)

        iyz =
            m / 20 * (2 * (z1 * y1 + z2 * y2 + z3 * y3) + z1 * y2 + z2 * y1 + z1 * y3 + z3 * y1 + z2 * y3 + z3 * y2)

        izx =
            m / 20 * (2 * (x1 * z1 + x2 * z2 + x3 * z3) + x1 * z2 + x2 * z1 + x1 * z3 + x3 * z1 + x2 * z3 + x3 * z2)
    in
    { m11 = ixx
    , m12 = -ixy
    , m13 = -izx
    , m21 = -ixy
    , m22 = iyy
    , m23 = -iyz
    , m31 = -izx
    , m32 = -iyz
    , m33 = izz
    }


{-| Eigendecomposition of a symmetric 3x3 matrix.
-}
eigenDecomposition : Mat3 -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
eigenDecomposition m =
    let
        p1 =
            m.m12 * m.m12 + m.m13 * m.m13 + m.m23 * m.m23
    in
    if p1 == 0 then
        diagonalEigen m

    else
        nonDiagonalEigen m p1


diagonalEigen : Mat3 -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
diagonalEigen m =
    { eigenvalues = { x = m.m11, y = m.m22, z = m.m33 }
    , v1 = Vec3.xAxis
    , v2 = Vec3.yAxis
    , v3 = Vec3.zAxis
    }


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
    in
    eigenvectorsFromEigenvalues m eig1 eig2 eig3 gap1 gap2


eigenvectorsFromEigenvalues : Mat3 -> Float -> Float -> Float -> Float -> Float -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
eigenvectorsFromEigenvalues m eig1 eig2 eig3 gap1 gap2 =
    let
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


{-| Eigendecomposition of a symmetric 3x3 matrix using Jacobi iteration.
-}
eigenDecompositionJacobi : Mat3 -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
eigenDecompositionJacobi m =
    let
        result =
            jacobiIterate 30 m 1 0 0 0 1 0 0 0 1
    in
    { eigenvalues = { x = result.d11, y = result.d22, z = result.d33 }
    , v1 = { x = result.r11, y = result.r21, z = result.r31 }
    , v2 = { x = result.r12, y = result.r22, z = result.r32 }
    , v3 = { x = result.r13, y = result.r23, z = result.r33 }
    }


jacobiIterate :
    Int
    -> Mat3
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> { d11 : Float, d22 : Float, d33 : Float, r11 : Float, r21 : Float, r31 : Float, r12 : Float, r22 : Float, r32 : Float, r13 : Float, r23 : Float, r33 : Float }
jacobiIterate steps m r11 r21 r31 r12 r22 r32 r13 r23 r33 =
    if steps <= 0 then
        { d11 = m.m11
        , d22 = m.m22
        , d33 = m.m33
        , r11 = r11
        , r21 = r21
        , r31 = r31
        , r12 = r12
        , r22 = r22
        , r32 = r32
        , r13 = r13
        , r23 = r23
        , r33 = r33
        }

    else
        let
            abs12 =
                abs m.m12

            abs13 =
                abs m.m13

            abs23 =
                abs m.m23
        in
        if abs12 - abs13 >= 0 && abs12 - abs23 >= 0 then
            if abs12 < 1.0e-12 then
                { d11 = m.m11
                , d22 = m.m22
                , d33 = m.m33
                , r11 = r11
                , r21 = r21
                , r31 = r31
                , r12 = r12
                , r22 = r22
                , r32 = r32
                , r13 = r13
                , r23 = r23
                , r33 = r33
                }

            else
                let
                    theta =
                        (m.m22 - m.m11) / (2 * m.m12)

                    t =
                        jacobiT theta

                    c =
                        1 / sqrt (1 + t * t)

                    s =
                        t * c

                    newM =
                        { m11 = m.m11 - t * m.m12
                        , m22 = m.m22 + t * m.m12
                        , m33 = m.m33
                        , m12 = 0
                        , m21 = 0
                        , m13 = c * m.m13 - s * m.m23
                        , m31 = c * m.m13 - s * m.m23
                        , m23 = s * m.m13 + c * m.m23
                        , m32 = s * m.m13 + c * m.m23
                        }
                in
                jacobiIterate (steps - 1)
                    newM
                    (c * r11 - s * r12)
                    (c * r21 - s * r22)
                    (c * r31 - s * r32)
                    (s * r11 + c * r12)
                    (s * r21 + c * r22)
                    (s * r31 + c * r32)
                    r13
                    r23
                    r33

        else if abs13 >= abs23 then
            if abs13 < 1.0e-12 then
                { d11 = m.m11
                , d22 = m.m22
                , d33 = m.m33
                , r11 = r11
                , r21 = r21
                , r31 = r31
                , r12 = r12
                , r22 = r22
                , r32 = r32
                , r13 = r13
                , r23 = r23
                , r33 = r33
                }

            else
                let
                    theta =
                        (m.m33 - m.m11) / (2 * m.m13)

                    t =
                        jacobiT theta

                    c =
                        1 / sqrt (1 + t * t)

                    s =
                        t * c

                    newM =
                        { m11 = m.m11 - t * m.m13
                        , m22 = m.m22
                        , m33 = m.m33 + t * m.m13
                        , m12 = c * m.m12 - s * m.m23
                        , m21 = c * m.m12 - s * m.m23
                        , m13 = 0
                        , m31 = 0
                        , m23 = s * m.m12 + c * m.m23
                        , m32 = s * m.m12 + c * m.m23
                        }
                in
                jacobiIterate (steps - 1)
                    newM
                    (c * r11 - s * r13)
                    (c * r21 - s * r23)
                    (c * r31 - s * r33)
                    r12
                    r22
                    r32
                    (s * r11 + c * r13)
                    (s * r21 + c * r23)
                    (s * r31 + c * r33)

        else if abs23 < 1.0e-12 then
            { d11 = m.m11
            , d22 = m.m22
            , d33 = m.m33
            , r11 = r11
            , r21 = r21
            , r31 = r31
            , r12 = r12
            , r22 = r22
            , r32 = r32
            , r13 = r13
            , r23 = r23
            , r33 = r33
            }

        else
            let
                theta =
                    (m.m33 - m.m22) / (2 * m.m23)

                t =
                    jacobiT theta

                c =
                    1 / sqrt (1 + t * t)

                s =
                    t * c

                newM =
                    { m11 = m.m11
                    , m22 = m.m22 - t * m.m23
                    , m33 = m.m33 + t * m.m23
                    , m12 = c * m.m12 - s * m.m13
                    , m21 = c * m.m12 - s * m.m13
                    , m13 = s * m.m12 + c * m.m13
                    , m31 = s * m.m12 + c * m.m13
                    , m23 = 0
                    , m32 = 0
                    }
            in
            jacobiIterate (steps - 1)
                newM
                r11
                r21
                r31
                (c * r12 - s * r13)
                (c * r22 - s * r23)
                (c * r32 - s * r33)
                (s * r12 + c * r13)
                (s * r22 + c * r23)
                (s * r32 + c * r33)


jacobiT : Float -> Float
jacobiT theta =
    if theta >= 0 then
        1 / (theta + sqrt (1 + theta * theta))

    else
        1 / (theta - sqrt (1 + theta * theta))


{-| Eigendecomposition using Jacobi iteration with flat arguments to avoid record allocation.
-}
eigenDecompositionJacobi2 : Mat3 -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
eigenDecompositionJacobi2 { m11, m22, m33, m12, m13, m23 } =
    jacobiIterate2 30
        m11
        m22
        m33
        m12
        m13
        m23
        1
        0
        0
        0
        1
        0
        0
        0
        1


jacobiIterate2 :
    Int
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
jacobiIterate2 steps d11 d22 d33 a12 a13 a23 r11 r21 r31 r12 r22 r32 r13 r23 r33 =
    let
        abs12 =
            abs a12

        abs13 =
            abs a13

        abs23 =
            abs a23
    in
    if steps <= 0 || (abs12 < 1.0e-12 && abs13 < 1.0e-12 && abs23 < 1.0e-12) then
        { eigenvalues = { x = d11, y = d22, z = d33 }
        , v1 = { x = r11, y = r21, z = r31 }
        , v2 = { x = r12, y = r22, z = r32 }
        , v3 = { x = r13, y = r23, z = r33 }
        }

    else if abs12 - abs13 >= 0 && abs12 - abs23 >= 0 then
        let
            theta =
                (d22 - d11) / (2 * a12)

            t =
                jacobiT theta

            c =
                1 / sqrt (1 + t * t)

            s =
                t * c
        in
        jacobiIterate2 (steps - 1)
            (d11 - t * a12)
            (d22 + t * a12)
            d33
            0
            (c * a13 - s * a23)
            (s * a13 + c * a23)
            (c * r11 - s * r12)
            (c * r21 - s * r22)
            (c * r31 - s * r32)
            (s * r11 + c * r12)
            (s * r21 + c * r22)
            (s * r31 + c * r32)
            r13
            r23
            r33

    else if abs13 >= abs23 then
        let
            theta =
                (d33 - d11) / (2 * a13)

            t =
                jacobiT theta

            c =
                1 / sqrt (1 + t * t)

            s =
                t * c
        in
        jacobiIterate2 (steps - 1)
            (d11 - t * a13)
            d22
            (d33 + t * a13)
            (c * a12 - s * a23)
            0
            (s * a12 + c * a23)
            (c * r11 - s * r13)
            (c * r21 - s * r23)
            (c * r31 - s * r33)
            r12
            r22
            r32
            (s * r11 + c * r13)
            (s * r21 + c * r23)
            (s * r31 + c * r33)

    else
        let
            theta =
                (d33 - d22) / (2 * a23)

            t =
                jacobiT theta

            c =
                1 / sqrt (1 + t * t)

            s =
                t * c
        in
        jacobiIterate2 (steps - 1)
            d11
            (d22 - t * a23)
            (d33 + t * a23)
            (c * a12 - s * a13)
            (s * a12 + c * a13)
            0
            r11
            r21
            r31
            (c * r12 - s * r13)
            (c * r22 - s * r23)
            (c * r32 - s * r33)
            (s * r12 + c * r13)
            (s * r22 + c * r23)
            (s * r32 + c * r33)


{-| Eigendecomposition using Jacobi iteration with everything in a single record.
-}
eigenDecompositionJacobi3 : Mat3 -> { eigenvalues : Vec3, v1 : Vec3, v2 : Vec3, v3 : Vec3 }
eigenDecompositionJacobi3 { m11, m22, m33, m12, m13, m23 } =
    let
        result =
            jacobiIterate3
                { steps = 30
                , d11 = m11
                , d22 = m22
                , d33 = m33
                , a12 = m12
                , a13 = m13
                , a23 = m23
                , r11 = 1
                , r21 = 0
                , r31 = 0
                , r12 = 0
                , r22 = 1
                , r32 = 0
                , r13 = 0
                , r23 = 0
                , r33 = 1
                }
    in
    { eigenvalues = { x = result.d11, y = result.d22, z = result.d33 }
    , v1 = { x = result.r11, y = result.r21, z = result.r31 }
    , v2 = { x = result.r12, y = result.r22, z = result.r32 }
    , v3 = { x = result.r13, y = result.r23, z = result.r33 }
    }


jacobiIterate3 :
    { steps : Int, d11 : Float, d22 : Float, d33 : Float, a12 : Float, a13 : Float, a23 : Float, r11 : Float, r21 : Float, r31 : Float, r12 : Float, r22 : Float, r32 : Float, r13 : Float, r23 : Float, r33 : Float }
    -> { steps : Int, d11 : Float, d22 : Float, d33 : Float, a12 : Float, a13 : Float, a23 : Float, r11 : Float, r21 : Float, r31 : Float, r12 : Float, r22 : Float, r32 : Float, r13 : Float, r23 : Float, r33 : Float }
jacobiIterate3 s =
    let
        abs12 =
            abs s.a12

        abs13 =
            abs s.a13

        abs23 =
            abs s.a23
    in
    if s.steps <= 0 || (abs12 < 1.0e-12 && abs13 < 1.0e-12 && abs23 < 1.0e-12) then
        s

    else if abs12 - abs13 >= 0 && abs12 - abs23 >= 0 then
        let
            theta =
                (s.d22 - s.d11) / (2 * s.a12)

            t =
                jacobiT theta

            c =
                1 / sqrt (1 + t * t)

            k =
                t * c
        in
        jacobiIterate3
            { steps = s.steps - 1
            , d11 = s.d11 - t * s.a12
            , d22 = s.d22 + t * s.a12
            , d33 = s.d33
            , a12 = 0
            , a13 = c * s.a13 - k * s.a23
            , a23 = k * s.a13 + c * s.a23
            , r11 = c * s.r11 - k * s.r12
            , r21 = c * s.r21 - k * s.r22
            , r31 = c * s.r31 - k * s.r32
            , r12 = k * s.r11 + c * s.r12
            , r22 = k * s.r21 + c * s.r22
            , r32 = k * s.r31 + c * s.r32
            , r13 = s.r13
            , r23 = s.r23
            , r33 = s.r33
            }

    else if abs13 >= abs23 then
        let
            theta =
                (s.d33 - s.d11) / (2 * s.a13)

            t =
                jacobiT theta

            c =
                1 / sqrt (1 + t * t)

            k =
                t * c
        in
        jacobiIterate3
            { steps = s.steps - 1
            , d11 = s.d11 - t * s.a13
            , d22 = s.d22
            , d33 = s.d33 + t * s.a13
            , a12 = c * s.a12 - k * s.a23
            , a13 = 0
            , a23 = k * s.a12 + c * s.a23
            , r11 = c * s.r11 - k * s.r13
            , r21 = c * s.r21 - k * s.r23
            , r31 = c * s.r31 - k * s.r33
            , r12 = s.r12
            , r22 = s.r22
            , r32 = s.r32
            , r13 = k * s.r11 + c * s.r13
            , r23 = k * s.r21 + c * s.r23
            , r33 = k * s.r31 + c * s.r33
            }

    else
        let
            theta =
                (s.d33 - s.d22) / (2 * s.a23)

            t =
                jacobiT theta

            c =
                1 / sqrt (1 + t * t)

            k =
                t * c
        in
        jacobiIterate3
            { steps = s.steps - 1
            , d11 = s.d11
            , d22 = s.d22 - t * s.a23
            , d33 = s.d33 + t * s.a23
            , a12 = c * s.a12 - k * s.a13
            , a13 = k * s.a12 + c * s.a13
            , a23 = 0
            , r11 = s.r11
            , r21 = s.r21
            , r31 = s.r31
            , r12 = c * s.r12 - k * s.r13
            , r22 = c * s.r22 - k * s.r23
            , r32 = c * s.r32 - k * s.r33
            , r13 = k * s.r12 + c * s.r13
            , r23 = k * s.r22 + c * s.r23
            , r33 = k * s.r32 + c * s.r33
            }
