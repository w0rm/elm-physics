module Physics.Mat3 exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


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
