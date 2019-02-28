module Physics.Body exposing
    ( Body
    , BodyId
    , addGravity
    , addShape
    , body
    , clearForces
    , getNextShapeId
    , setMass
    , shapeWorldTransform
    , tick
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4)
import Physics.AABB as AABB exposing (AABB)
import Physics.Const as Const
import Physics.Quaternion as Quaternion
import Physics.Shape as Shape exposing (Shape(..), ShapeId)
import Physics.Transform as Transform exposing (Transform)
import Dict exposing (Dict)


type alias BodyId =
    Int


type alias Body =
    { position : Vec3
    , velocity : Vec3
    , angularVelocity : Vec3
    , quaternion : Vec4
    , mass : Float
    , shapes : Dict ShapeId Shape
    , shapeTransforms : Dict ShapeId Transform
    , nextShapeId : ShapeId
    , force : Vec3
    , torque : Vec3
    , boundingSphereRadius : Float

    -- mass props
    , invMass : Float
    , inertia : Vec3
    , invInertia : Vec3
    , invInertiaWorld : Mat4
    }


body : Body
body =
    { position = Const.zero3
    , velocity = Const.zero3
    , angularVelocity = Const.zero3
    , quaternion = Quaternion.identity
    , mass = 0
    , shapes = Dict.empty
    , shapeTransforms = Dict.empty -- positions of the shapes inside this body
    , nextShapeId = 0
    , force = Const.zero3
    , torque = Const.zero3
    , boundingSphereRadius = 0

    -- mass props
    , invMass = 0
    , inertia = Const.zero3
    , invInertia = Const.zero3
    , invInertiaWorld = Mat4.identity
    }


addGravity : Vec3 -> Body -> Body
addGravity gravity body_ =
    { body_
        | force =
            gravity
                |> Vec3.scale body_.mass
                |> Vec3.add body_.force
    }


clearForces : Body -> Body
clearForces body_ =
    { body_
        | force = Const.zero3
        , torque = Const.zero3
    }


setMass : Float -> Body -> Body
setMass mass body_ =
    updateMassProperties
        { body_ | mass = mass }


setPosition : Vec3 -> Body -> Body
setPosition position body_ =
    updateMassProperties
        { body_ | position = position }


setQuaternion : Vec4 -> Body -> Body
setQuaternion quaternion body_ =
    updateMassProperties
        { body_ | quaternion = quaternion }


{-| Predict the shape id of the next shape to be added
-}
getNextShapeId : Body -> ShapeId
getNextShapeId =
    .nextShapeId


addShape : Shape -> Body -> Body
addShape shape body_ =
    -- TODO: support shape's position and rotation:
    { body_
        | shapes = Dict.insert body_.nextShapeId shape body_.shapes
        , nextShapeId = body_.nextShapeId + 1
        , boundingSphereRadius = Shape.expandBoundingSphereRadius Transform.identity shape body_.boundingSphereRadius
    }
        |> updateMassProperties


shapeWorldTransform : ShapeId -> Body -> Transform
shapeWorldTransform shapeId { position, quaternion, shapeTransforms } =
    case Dict.get shapeId shapeTransforms of
        Just transform ->
            { quaternion =
                transform.quaternion
                    |> Quaternion.mul quaternion
            , position =
                transform.position
                    |> Quaternion.rotate quaternion
                    |> Vec3.add position
            }

        Nothing ->
            { quaternion = quaternion
            , position = position
            }


tick : Float -> Body -> Body
tick dt body_ =
    let
        invMass =
            if body_.mass == 0 then
                0

            else
                1.0 / body_.mass

        newVelocity =
            body_.force
                |> Vec3.scale (invMass * dt)
                |> Vec3.add body_.velocity

        newAngularVelocity =
            body_.torque
                |> Mat4.transform body_.invInertiaWorld
                |> Vec3.scale dt
                |> Vec3.add body_.angularVelocity
    in
    updateInertiaWorld False
        { body_
            | velocity = newVelocity
            , angularVelocity = newAngularVelocity
            , position =
                newVelocity
                    |> Vec3.scale dt
                    |> Vec3.add body_.position
            , quaternion =
                body_.quaternion
                    |> Quaternion.rotateBy (Vec3.scale (dt / 2) newAngularVelocity)
                    |> Vec4.normalize
        }


{-| Should be called whenever you change the body shape or mass.
-}
updateMassProperties : Body -> Body
updateMassProperties ({ mass } as body_) =
    let
        invMass =
            if mass == 0 then
                0

            else
                1.0 / mass

        e =
            body_
                |> computeAABB
                |> AABB.toHalfExtends

        ix =
            1.0 / 12.0 * mass * (2 * e.y * 2 * e.y + 2 * e.z * 2 * e.z)

        iy =
            1.0 / 12.0 * mass * (2 * e.x * 2 * e.x + 2 * e.z * 2 * e.z)

        iz =
            1.0 / 12.0 * mass * (2 * e.y * 2 * e.y + 2 * e.x * 2 * e.x)

        inertia =
            vec3 ix iy iz

        invInertia =
            vec3
                (if ix > 0 then
                    1.0 / ix

                 else
                    0
                )
                (if iy > 0 then
                    1.0 / iy

                 else
                    0
                )
                (if iz > 0 then
                    1.0 / iz

                 else
                    0
                )
    in
    updateInertiaWorld True
        { body_
            | invMass = invMass
            , inertia = inertia
            , invInertia = invInertia
        }


updateInertiaWorld : Bool -> Body -> Body
updateInertiaWorld force ({ invInertia, quaternion } as body_) =
    if not force && Vec3.getX invInertia == Vec3.getY invInertia && Vec3.getY invInertia == Vec3.getZ invInertia then
        body_

    else
        let
            m =
                Quaternion.toMat4 quaternion
        in
        { body_
            | invInertiaWorld =
                Mat4.mul
                    (Mat4.transpose m)
                    (Mat4.scale invInertia m)
        }


computeAABB : Body -> AABB
computeAABB body_ =
    Dict.foldl
        (\shapeId shape ->
            shapeWorldTransform shapeId body_
                |> Shape.aabbClosure shape
                |> AABB.extend
        )
        AABB.impossible
        body_.shapes
