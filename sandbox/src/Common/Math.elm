module Common.Math exposing (makeShadow)

{-| Some useful Math utilities.
-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


{-| A "squash" matrix that smashes things to the ground plane,
defined by position, normal, parallel to a given light vector
-}
makeShadow : Vec3 -> Vec3 -> Vec3 -> Mat4
makeShadow position normal light =
    let
        n =
            Vec3.toRecord normal

        nw =
            -(Vec3.dot position normal)

        l =
            Vec3.toRecord light

        d =
            Vec3.dot normal light
    in
    Mat4.fromRecord
        { m11 = l.x * n.x - d
        , m21 = l.y * n.x
        , m31 = l.z * n.x
        , m41 = 0
        , m12 = l.x * n.y
        , m22 = l.y * n.y - d
        , m32 = l.z * n.y
        , m42 = 0
        , m13 = l.x * n.z
        , m23 = l.y * n.z
        , m33 = l.z * n.z - d
        , m43 = 0
        , m14 = l.x * nw
        , m24 = l.y * nw
        , m34 = l.z * nw
        , m44 = -d
        }
