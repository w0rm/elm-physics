module Physics.Body
    exposing
        ( Body
        , BodyId
        , addGravity
        , clearForces
        , shapeWorldTransform
        , tick
        , body
        , setMass
        , getNextShapeId
        , addShape
        )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)
import Physics.AABB as AABB exposing (AABB)
import Physics.Const as Const
import Dict exposing (Dict)
import Physics.Shape as Shape exposing (Shape, ShapeId)
import Time exposing (Time)
import Physics.Const as Const


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
addGravity gravity body =
    { body
        | force =
            gravity
                |> Vec3.scale body.mass
                |> Vec3.add body.force
    }


clearForces : Body -> Body
clearForces body =
    { body
        | force = Const.zero3
        , torque = Const.zero3
    }


setMass : Float -> Body -> Body
setMass mass body =
    updateMassProperties
        { body | mass = mass }


setPosition : Vec3 -> Body -> Body
setPosition position body =
    updateMassProperties
        { body | position = position }


setQuaternion : Vec4 -> Body -> Body
setQuaternion quaternion body =
    updateMassProperties
        { body | quaternion = quaternion }


{-| Predict the shape id of the next shape to be added
-}
getNextShapeId : Body -> ShapeId
getNextShapeId body =
    body.nextShapeId

addShape : Shape -> Body -> Body
addShape shape body =
    { body
        | shapes = Dict.insert body.nextShapeId shape body.shapes
        , nextShapeId = body.nextShapeId + 1
    }
        |> updateMassProperties
        |> updateBoundingSphereRadius


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


tick : Time -> Body -> Body
tick dt body =
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
                |> Mat4.transform body.invInertiaWorld
                |> Vec3.scale dt
                |> Vec3.add body.angularVelocity
    in
        updateInertiaWorld False
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
                        |> Vec4.normalize
            }


{-| Should be called whenever you add or remove shapes.
-}
updateBoundingSphereRadius : Body -> Body
updateBoundingSphereRadius body =
    { body
        | boundingSphereRadius = computeBoundingSphereRadius body
    }


{-| Should be called whenever you change the body shape or mass.
-}
updateMassProperties : Body -> Body
updateMassProperties ({ mass } as body) =
    let
        invMass =
            if mass == 0 then
                0
            else
                1.0 / mass

        ( ex, ey, ez ) =
            body
                |> computeAABB
                |> AABB.toHalfExtends
                |> Vec3.toTuple

        ix =
            (1.0 / 12.0 * mass * (2 * ey * 2 * ey + 2 * ez * 2 * ez))

        iy =
            (1.0 / 12.0 * mass * (2 * ex * 2 * ex + 2 * ez * 2 * ez))

        iz =
            (1.0 / 12.0 * mass * (2 * ey * 2 * ey + 2 * ex * 2 * ex))

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
            { body
                | invMass = invMass
                , inertia = inertia
                , invInertia = invInertia
            }


updateInertiaWorld : Bool -> Body -> Body
updateInertiaWorld force ({ invInertia, quaternion } as body) =
    if not force && Vec3.getX invInertia == Vec3.getY invInertia && Vec3.getY invInertia == Vec3.getZ invInertia then
        body
    else
        let
            m =
                Quaternion.toMat4 quaternion
        in
            { body
                | invInertiaWorld =
                    Mat4.mul
                        (Mat4.transpose m)
                        (Mat4.scale invInertia m)
            }


computeAABB : Body -> AABB
computeAABB body =
    Dict.foldl
        (\shapeId shape ->
                shapeWorldTransform shapeId body
                    |> Shape.aabbClosure shape
                    |> AABB.extend
        )
        AABB.impossible
        body.shapes


computeBoundingSphereRadius : Body -> Float
computeBoundingSphereRadius body =
    Dict.foldl
        (\shapeId shape radius ->
            shapeWorldTransform shapeId body
                |> Shape.boundingSphereRadiusClosure shape 
                |> max radius
        )
        0
        body.shapes
