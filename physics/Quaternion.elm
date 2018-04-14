module Physics.Quaternion exposing (..)

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


{-| Spherical linear interpolation
<http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/index.htm>

Elm implementation copied from <https://github.com/kfish/quaternion>

-}
slerp : Float -> Vec4 -> Vec4 -> Vec4
slerp t qa qb =
    let
        -- Calculate angle between them.
        cosHalfTheta =
            Vec4.dot qa qb

        halfTheta =
            acos cosHalfTheta

        sinHalfTheta =
            sqrt (1.0 - cosHalfTheta * cosHalfTheta)

        ( aw, ax, ay, az ) =
            Vec4.toTuple qa

        ( bw, bx, by, bz ) =
            Vec4.toTuple qb

        hw =
            aw * 0.5 + bw * 0.5

        hx =
            ax * 0.5 + bx * 0.5

        hy =
            ay * 0.5 + by * 0.5

        hz =
            az * 0.5 + bz * 0.5

        ratioA =
            sin ((1 - t) * halfTheta) / sinHalfTheta

        ratioB =
            sin (t * halfTheta) / sinHalfTheta

        mw =
            aw * ratioA + bw * ratioB

        mx =
            ax * ratioA + bx * ratioB

        my =
            ay * ratioA + by * ratioB

        mz =
            az * ratioA + bz * ratioB
    in
        -- if qa=qb or qa=-qb then theta = 0 and we can return qa
        if abs cosHalfTheta >= 1.0 then
            qa
            -- if theta = 180 degrees then result is not fully defined
            -- we could rotate around any axis normal to qa or qb
        else if abs sinHalfTheta < 0.001 then
            vec4 hw hx hy hz
        else
            vec4 mw mx my mz
