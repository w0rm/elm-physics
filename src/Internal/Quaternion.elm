module Internal.Quaternion exposing
    ( Quaternion
    , fromAngleAxis
    , identity
    , mul
    , normalize
    , rotate
    , rotateBy
    , toMat4
    )

import Internal.Matrix4 as Mat4 exposing (Mat4)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Quaternion =
    { x : Float, y : Float, z : Float, w : Float }


identity : Quaternion
identity =
    { x = 0, y = 0, z = 0, w = 1 }


fromAngleAxis : Float -> Vec3 -> Quaternion
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
    { x = x * s, y = y * s, z = z * s, w = c }


toMat4 : Quaternion -> Mat4
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


mul : Quaternion -> Quaternion -> Quaternion
mul q1 q2 =
    { x = q1.x * q2.w + q1.y * q2.z - q1.z * q2.y + q1.w * q2.x
    , y = -q1.x * q2.z + q1.y * q2.w + q1.z * q2.x + q1.w * q2.y
    , z = q1.x * q2.y - q1.y * q2.x + q1.z * q2.w + q1.w * q2.z
    , w = -q1.x * q2.x - q1.y * q2.y - q1.z * q2.z + q1.w * q2.w
    }


{-| angularDistance = angularVelocity \* dt / 2
-}
rotateBy : Vec3 -> Quaternion -> Quaternion
rotateBy angularDistance quaternion =
    let
        a =
            angularDistance

        b =
            quaternion
    in
    { x = b.x + (a.x * b.w + a.y * b.z - a.z * b.y)
    , y = b.y + (a.y * b.w + a.z * b.x - a.x * b.z)
    , z = b.z + (a.z * b.w + a.x * b.y - a.y * b.x)
    , w = b.w + (-a.x * b.x - a.y * b.y - a.z * b.z)
    }


rotate : Quaternion -> Vec3 -> Vec3
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


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Quaternion -> Quaternion
normalize v4 =
    let
        len =
            length v4
    in
    { x = v4.x / len
    , y = v4.y / len
    , z = v4.z / len
    , w = v4.w / len
    }


{-| The length of the given vector: |a|
-}
length : Quaternion -> Float
length { x, y, z, w } =
    sqrt (x * x + y * y + z * z + w * w)
