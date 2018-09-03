module AltPhysics.Quaternion exposing
    ( conjugate
    , fromAngleAxis
    , identity
    , mul
    , rotate
    , rotateBy
    , toMat4
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4, vec4)


identity : Vec4
identity =
    vec4 0 0 0 1


fromAngleAxis : Float -> Vec3 -> Vec4
fromAngleAxis angle axis =
    let
        { x, y, z } =
            Vec3.normalize axis

        theta =
            angle / 2.0

        c =
            cos theta

        s =
            sin theta
    in
    vec4 (x * s) (y * s) (z * s) c


toMat4 : Vec4 -> Mat4
toMat4 { x, y, z, w } =
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
    vec4
        (q1.x * q2.w + q1.y * q2.z - q1.z * q2.y + q1.w * q2.x)
        (-q1.x * q2.z + q1.y * q2.w + q1.z * q2.x + q1.w * q2.y)
        (q1.x * q2.y - q1.y * q2.x + q1.z * q2.w + q1.w * q2.z)
        (-q1.x * q2.x - q1.y * q2.y - q1.z * q2.z + q1.w * q2.w)


conjugate : Vec4 -> Vec4
conjugate { x, y, z, w } =
    vec4 -x -y -z w


{-| angularDistance = angularVelocity \* dt / 2
-}
rotateBy : Vec3 -> Vec4 -> Vec4
rotateBy angularDistance quaternion =
    let
        a =
            angularDistance

        b =
            quaternion
    in
    vec4
        (b.x + (a.x * b.w + a.y * b.z - a.z * b.y))
        (b.y + (a.y * b.w + a.z * b.x - a.x * b.z))
        (b.z + (a.z * b.w + a.x * b.y - a.y * b.x))
        (b.w + (-a.x * b.x - a.y * b.y - a.z * b.z))


rotate : Vec4 -> Vec3 -> Vec3
rotate q { x, y, z } =
    let
        ix =
            q.w * x + q.y * z - q.z * y

        iy =
            q.w * y + q.z * x - q.x * z

        iz =
            q.w * z + q.x * y - q.y * x

        iw =
            -q.x * x - q.y * y - q.z * z
    in
    vec3
        (ix * q.w + iw * -q.x + iy * -q.z - iz * -q.y)
        (iy * q.w + iw * -q.y + iz * -q.x - ix * -q.z)
        (iz * q.w + iw * -q.z + ix * -q.y - iy * -q.x)
