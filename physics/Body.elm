module Physics.Body exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Physics.Quaternion as Quaternion
import Dict exposing (Dict)
import Time exposing (Time)
import Physics.Shape exposing (Shape, ShapeId)


type alias BodyId =
    Int


type alias Body =
    { position : Vec3
    , velocity : Vec3
    , angularVelocity : Vec3
    , quaternion : Vec4
    , mass : Float
    , shapes : Dict ShapeId Shape
    , shapeOffsets : Dict ShapeId Vec3
    , shapeOrientations : Dict ShapeId Vec4
    , nextShapeId : ShapeId
    }


body : Body
body =
    { position = zero3
    , velocity = zero3
    , angularVelocity = zero3
    , quaternion = Quaternion.identity
    , mass = 0
    , shapes = Dict.empty
    , shapeOffsets = Dict.empty -- get defaults to zero3
    , shapeOrientations = Dict.empty -- get defaults to Quaternion.identity
    , nextShapeId = 0
    }


setMass : Float -> Body -> Body
setMass mass body =
    { body | mass = mass }


setPosition : Vec3 -> Body -> Body
setPosition position body =
    { body | position = position }


setQuaternion : Vec4 -> Body -> Body
setQuaternion quaternion body =
    { body | quaternion = quaternion }


addShape : Shape -> Body -> Body
addShape shape body =
    { body
        | shapes = Dict.insert body.nextShapeId shape body.shapes
        , nextShapeId = body.nextShapeId + 1
    }


zero3 : Vec3
zero3 =
    vec3 0 0 0
