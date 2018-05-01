module Physics.Shape exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)


type alias ShapeId =
    Int


type Shape
    = Box Vec3
    | Plane


transform : Shape -> Mat4
transform shape =
    case shape of
        Plane ->
            Mat4.identity

        Box halfExtents ->
            Mat4.makeScale halfExtents
