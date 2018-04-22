module Physics.Mat3 exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Physics.Quaternion as Quaternion


type alias Mat3 =
    { e0 : Float
    , e1 : Float
    , e2 : Float
    , e3 : Float
    , e4 : Float
    , e5 : Float
    , e6 : Float
    , e7 : Float
    , e8 : Float
    }


mul : Mat3 -> Vec3 -> Vec3
mul { e0, e1, e2, e3, e4, e5, e6, e7, e8 } vec =
    let
        { x, y, z } =
            Vec3.toRecord vec
    in
        vec3
            (e0 * x + e1 * y + e2 * z)
            (e3 * x + e4 * y + e5 * z)
            (e6 * x + e7 * y + e8 * z)


zero : Mat3
zero =
    { e0 = 0
    , e1 = 0
    , e2 = 0
    , e3 = 0
    , e4 = 0
    , e5 = 0
    , e6 = 0
    , e7 = 0
    , e8 = 0
    }


identity : Mat3
identity =
    { e0 = 1
    , e1 = 0
    , e2 = 0
    , e3 = 0
    , e4 = 1
    , e5 = 0
    , e6 = 0
    , e7 = 0
    , e8 = 1
    }


fromQuaternion : Vec4 -> Mat3
fromQuaternion quaternion =
    let
        { x, y, z, w } =
            Vec4.toRecord quaternion

        x2 =
            x + x

        y2 =
            y + y

        z2 =
            z + z

        xx =
            x * x2

        xy =
            x * y2

        xz =
            x * z2

        yy =
            y * y2

        yz =
            y * z2

        zz =
            z * z2

        wx =
            w * x2

        wy =
            w * y2

        wz =
            w * z2
    in
        { e0 = 1 - (yy + zz)
        , e1 = xy - wz
        , e2 = xz + wy
        , e3 = xy + wz
        , e4 = 1 - (xx + zz)
        , e5 = yz - wx
        , e6 = xz - wy
        , e7 = yz + wx
        , e8 = 1 - (xx + yy)
        }


transpose : Mat3 -> Mat3
transpose m =
    { e0 = m.e0
    , e1 = m.e3
    , e2 = m.e6
    , e3 = m.e1
    , e4 = m.e4
    , e5 = m.e7
    , e6 = m.e2
    , e7 = m.e5
    , e8 = m.e8
    }


scale : Vec3 -> Mat3 -> Mat3
scale v m =
    let
        { x, y, z } =
            Vec3.toRecord v
    in
        { e0 = m.e0 * x
        , e1 = m.e1 * x
        , e2 = m.e2 * x
        , e3 = m.e3 * y
        , e4 = m.e4 * y
        , e5 = m.e5 * y
        , e6 = m.e6 * z
        , e7 = m.e7 * z
        , e8 = m.e8 * z
        }


mult : Mat3 -> Mat3 -> Mat3
mult m1 m2 =
    { e0 = m1.e0 * m2.e0 + m1.e1 * m2.e3 + m1.e2 * m2.e6
    , e1 = m1.e0 * m2.e1 + m1.e1 * m2.e4 + m1.e2 * m2.e7
    , e2 = m1.e0 * m2.e2 + m1.e1 * m2.e5 + m1.e2 * m2.e8
    , e3 = m1.e3 * m2.e0 + m1.e4 * m2.e3 + m1.e5 * m2.e6
    , e4 = m1.e3 * m2.e1 + m1.e4 * m2.e4 + m1.e5 * m2.e7
    , e5 = m1.e3 * m2.e2 + m1.e4 * m2.e5 + m1.e5 * m2.e8
    , e6 = m1.e6 * m2.e0 + m1.e7 * m2.e3 + m1.e8 * m2.e6
    , e7 = m1.e6 * m2.e1 + m1.e7 * m2.e4 + m1.e8 * m2.e7
    , e8 = m1.e6 * m2.e2 + m1.e7 * m2.e5 + m1.e8 * m2.e8
    }
