module Physics.Quaternion
    exposing
        ( identity
        , fromAngleAxis
        , toMat4
        , rotate
        , rotateBy
        , conjugate
        , mul
        )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)


identity : Vec4
identity =
    vec4 0 0 0 1


fromAngleAxis : Float -> Vec3 -> Vec4
fromAngleAxis angle axis =
    let
        { x, y, z } =
            Vec3.toRecord (Vec3.normalize axis)

        theta =
            angle / 2.0

        c =
            cos theta

        s =
            sin theta
    in
        vec4 (x * s) (y * s) (z * s) c


toMat4 : Vec4 -> Mat4
toMat4 q =
    let
        { x, y, z, w } =
            Vec4.toRecord q
    in
        Mat4.fromRecord
            { m11 = 1 - 2 * y * y - 2 * z * z
            , m12 = 2 * x * y - 2 * w * z
            , m13 = 2 * x * z + 2 * w * y
            , m14 = 0
            , m21 = 2 * x * y + 2 * w * z
            , m22 = 1 - 2 * x * x - 2 * z * z
            , m23 = 2 * y * z - 2 * w * x
            , m24 = 0
            , m31 = 2 * x * z - 2 * w * y
            , m32 = 2 * y * z + 2 * w * x
            , m33 = 1 - 2 * x * x - 2 * y * y
            , m34 = 0
            , m41 = 0
            , m42 = 0
            , m43 = 0
            , m44 = 1
            }


mul : Vec4 -> Vec4 -> Vec4
mul q1 q2 =
    let
        ( x1, y1, z1, w1 ) =
            Vec4.toTuple q1

        ( x2, y2, z2, w2 ) =
            Vec4.toTuple q2
    in
        vec4
            (x1 * w2 + y1 * z2 - z1 * y2 + w1 * x2)
            (-x1 * z2 + y1 * w2 + z1 * x2 + w1 * y2)
            (x1 * y2 - y1 * x2 + z1 * w2 + w1 * z2)
            (-x1 * x2 - y1 * y2 - z1 * z2 + w1 * w2)


conjugate : Vec4 -> Vec4
conjugate q =
    let
        { x, y, z, w } =
            Vec4.toRecord q
    in
        vec4 -x -y -z w


{-| angularDistance = angularVelocity * dt / 2
-}
rotateBy : Vec3 -> Vec4 -> Vec4
rotateBy angularDistance quaternion =
    let
        ( ax, ay, az ) =
            Vec3.toTuple angularDistance

        ( bx, by, bz, bw ) =
            Vec4.toTuple quaternion
    in
        vec4
            (bx + (ax * bw + ay * bz - az * by))
            (by + (ay * bw + az * bx - ax * bz))
            (bz + (az * bw + ax * by - ay * bx))
            (bw + (-ax * bx - ay * by - az * bz))


rotate : Vec4 -> Vec3 -> Vec3
rotate q v =
    let
        ( x, y, z ) =
            Vec3.toTuple v

        ( qx, qy, qz, qw ) =
            Vec4.toTuple q

        ix =
            qw * x + qy * z - qz * y

        iy =
            qw * y + qz * x - qx * z

        iz =
            qw * z + qx * y - qy * x

        iw =
            -qx * x - qy * y - qz * z
    in
        vec3
            (ix * qw + iw * -qx + iy * -qz - iz * -qy)
            (iy * qw + iw * -qy + iz * -qx - ix * -qz)
            (iz * qw + iw * -qz + ix * -qy - iy * -qx)
