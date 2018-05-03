module Physics.Body exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)
import Physics.AABB as AABB exposing (AABB)
import Dict exposing (Dict)
import Physics.Shape exposing (Shape(..), ShapeId)
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

    -- mass props
    , invMass : Float
    , inertia : Vec3
    , invInertia : Vec3
    , invInertiaWorld : Mat4
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
    , invMass = 0
    , inertia = zero3
    , invInertia = zero3
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
    { body | force = zero3, torque = zero3 }


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


addShape : Shape -> Body -> Body
addShape shape body =
    updateMassProperties
        { body
            | shapes = Dict.insert body.nextShapeId shape body.shapes
            , nextShapeId = body.nextShapeId + 1
        }


shapeWorldTransform : ShapeId -> Body -> Transform
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
                , quaternion = body.quaternion
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
            let
                transform =
                    shapeWorldTransform shapeId body
            in
                AABB.extend <|
                    case shape of
                        Convex convexPolyhedron ->
                            AABB.convexPolyhedron transform convexPolyhedron

                        Plane ->
                            AABB.plane transform
        )
        AABB.impossible
        body.shapes
