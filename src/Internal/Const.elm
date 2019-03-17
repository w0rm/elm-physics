module Internal.Const exposing (maxNumber, precision, zero3)

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)


zero3 : Vec3
zero3 =
    vec3 0 0 0


maxNumber : Float
maxNumber =
    3.40282347e38


precision : Float
precision =
    1.0e-6
