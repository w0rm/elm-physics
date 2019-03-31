module Internal.Body exposing
    ( Body
    , BodyId
    , Protected(..)
    , addGravity
    , clearForces
    , compound
    , shapeWorldTransform
    , tick
    , updateMassProperties
    )

import Dict exposing (Dict)
import Internal.AABB as AABB exposing (AABB)
import Internal.Const as Const
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Shape as Shape exposing (Shape)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


type alias BodyId =
    Int


type Protected data
    = Protected (Body data)


type alias Body data =
    { id : BodyId
    , data : data
    , position : Vec3
    , velocity : Vec3
    , angularVelocity : Vec3
    , orientation : Quaternion
    , mass : Float
    , shapes : List Shape
    , force : Vec3
    , torque : Vec3
    , boundingSphereRadius : Float

    -- mass props
    , invMass : Float
    , inertia : Vec3
    , invInertia : Vec3
    , invInertiaWorld : Mat3
    }


compound : List Shape -> data -> Body data
compound shapes data =
    updateMassProperties
        { id = -1
        , data = data
        , position = Const.zero3
        , velocity = Const.zero3
        , angularVelocity = Const.zero3
        , orientation = Quaternion.identity
        , mass = 0
        , shapes = shapes
        , force = Const.zero3
        , torque = Const.zero3
        , boundingSphereRadius = List.foldl Shape.expandBoundingSphereRadius 0 shapes

        -- mass props
        , invMass = 0
        , inertia = Const.zero3
        , invInertia = Const.zero3
        , invInertiaWorld = Mat3.identity
        }


addGravity : Vec3 -> Body data -> Body data
addGravity gravity body_ =
    { body_
        | force =
            gravity
                |> Vec3.scale body_.mass
                |> Vec3.add body_.force
    }


clearForces : Body data -> Body data
clearForces body_ =
    { body_
        | force = Const.zero3
        , torque = Const.zero3
    }


shapeWorldTransform : Shape -> Body data -> Transform
shapeWorldTransform shape { position, orientation } =
    { orientation =
        Quaternion.mul orientation shape.orientation
    , position =
        Vec3.add position (Quaternion.rotate orientation shape.position)
    }


tick : Float -> Body data -> Body data
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
                |> Mat3.transform body_.invInertiaWorld
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
            , orientation =
                body_.orientation
                    |> Quaternion.rotateBy (Vec3.scale (dt / 2) newAngularVelocity)
                    |> Quaternion.normalize
        }


{-| Should be called whenever you change the body shapes or mass.
-}
updateMassProperties : Body data -> Body data
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


updateInertiaWorld : Bool -> Body data -> Body data
updateInertiaWorld force ({ invInertia, orientation } as body_) =
    if not force && invInertia.x == invInertia.y && invInertia.y == invInertia.z then
        body_

    else
        let
            m =
                Quaternion.toMat3 orientation
        in
        { body_
            | invInertiaWorld =
                Mat3.mul
                    (Mat3.transpose m)
                    (Mat3.scale invInertia m)
        }


computeAABB : Body data -> AABB
computeAABB body_ =
    List.foldl
        (\shape ->
            shapeWorldTransform shape body_
                |> Shape.aabbClosure shape.kind
                |> AABB.extend
        )
        AABB.impossible
        body_.shapes
