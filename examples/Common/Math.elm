module Common.Math exposing
    ( makeRotateKTo
    , makeShadow
    )

{-| Some useful Math utilities.
-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)


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


{-| A 3D utility function that provides a transform for a rotation that
reorients the z axis to the given direction (unit vector), choosing any
convenient rotation for the xy plane.
-}
makeRotateKTo : Vec3 -> Mat4
makeRotateKTo direction =
    let
        distance =
            Vec3.distance Vec3.k direction
    in
    -- Specially handle the boundary cases that may throw off the general
    -- case trig formulas used below.
    -- These occur when the direction is (almost) vertical
    -- i.e. ~= Vec3.k or ~= (Vec3.negate Vec3.k)
    -- giving extreme distance values of ~0.0 or ~2.0.
    if distance <= precision then
        Mat4.identity

    else if abs (distance - 2.0) <= precision then
        -- A U-turn around the x=y line in the z=0 plane
        -- negates all x y and z values
        Mat4.makeRotate pi (vec3 1.0 1.0 0.0)

    else
        Mat4.makeRotate
            (2.0 * asin (distance / 2.0))
            (Vec3.cross Vec3.k direction)


precision : Float
precision =
    1.0e-6
