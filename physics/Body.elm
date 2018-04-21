module Physics.Body exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Physics.Quaternion as Quaternion
import Dict exposing (Dict)
import Physics.Shape exposing (Shape, ShapeId)
import Time exposing (Time)


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
    , force : Vec3
    , torque : Vec3
    }


type alias ShapeTransform =
    { quaternion : Vec4
    , position : Vec3
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
    , force = zero3
    , torque = zero3
    }


addGravity : Vec3 -> Body -> Body
addGravity gravity body =
    { body
        | force =
            gravity
                |> Vec3.scale body.mass
                |> Vec3.add body.force
    }


clearForces : Body -> Body
clearForces body =
    { body | force = zero3, torque = zero3 }


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


shapeWorldTransform : ShapeId -> Body -> ShapeTransform
shapeWorldTransform shapeId { position, quaternion, shapeOffsets, shapeOrientations } =
    { quaternion =
        Dict.get shapeId shapeOrientations
            |> Maybe.withDefault Quaternion.identity
            |> Quaternion.mul quaternion
    , position =
        Dict.get shapeId shapeOffsets
            |> Maybe.withDefault zero3
            |> Quaternion.rotate quaternion
            |> Vec3.add position
    }


zero3 : Vec3
zero3 =
    vec3 0 0 0


tick : Time -> Body -> Body
tick dt body =
    -- TODO: inertia
    let
        invMass =
            if body.mass == 0 then
                0
            else
                1.0 / body.mass

        newVelocity =
            body.force
                |> Vec3.scale (invMass * dt)
                |> Vec3.add body.velocity

        newAngularVelocity =
            body.torque
                |> Vec3.scale (invMass * dt)
                |> Vec3.add body.angularVelocity
    in
        { body
            | velocity = newVelocity
            , angularVelocity = newAngularVelocity
            , position =
                newVelocity
                    |> Vec3.scale dt
                    |> Vec3.add body.position
            , quaternion =
                body.quaternion
                    |> Quaternion.rotateBy (Vec3.scale (dt / 2) newAngularVelocity)
        }
