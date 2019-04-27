module Internal.Body exposing
    ( Body
    , BodyId
    , Protected(..)
    , addGravity
    , compound
    , raycast
    , shapeWorldTransform
    , tick
    , updateMassProperties
    )

import Internal.AABB as AABB exposing (AABB)
import Internal.Const as Const
import Internal.Material as Material exposing (Material)
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
    , material : Material
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
        , material = Material.default
        , position = Vec3.zero
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , orientation = Quaternion.identity
        , mass = 0
        , shapes = shapes
        , force = Vec3.zero
        , torque = Vec3.zero
        , boundingSphereRadius = List.foldl Shape.expandBoundingSphereRadius 0 shapes

        -- mass props
        , invMass = 0
        , inertia = Vec3.zero
        , invInertia = Vec3.zero
        , invInertiaWorld = Mat3.identity
        }


addGravity : Vec3 -> Body data -> Body data
addGravity gravity body =
    { body
        | force =
            gravity
                |> Vec3.scale body.mass
                |> Vec3.add body.force
    }


shapeWorldTransform : Shape -> Body data -> Transform
shapeWorldTransform shape { position, orientation } =
    { orientation =
        Quaternion.mul orientation shape.orientation
    , position =
        Vec3.add position (Quaternion.rotate orientation shape.position)
    }


tick : Float -> Body data -> Body data
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
                |> Mat3.transform body.invInertiaWorld
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
            , orientation =
                body.orientation
                    |> Quaternion.rotateBy (Vec3.scale (dt / 2) newAngularVelocity)
                    |> Quaternion.normalize

            -- clear forces
            , force = Vec3.zero
            , torque = Vec3.zero
        }


{-| Should be called whenever you change the body shapes or mass.
-}
updateMassProperties : Body data -> Body data
updateMassProperties ({ mass } as body) =
    let
        invMass =
            if mass == 0 then
                0

            else
                1.0 / mass

        e =
            body
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
        { body
            | invMass = invMass
            , inertia = inertia
            , invInertia = invInertia
        }


updateInertiaWorld : Bool -> Body data -> Body data
updateInertiaWorld force ({ invInertia, orientation } as body) =
    if not force && invInertia.x == invInertia.y && invInertia.y == invInertia.z then
        body

    else
        let
            m =
                Quaternion.toMat3 orientation
        in
        { body
            | invInertiaWorld =
                Mat3.mul
                    (Mat3.transpose m)
                    (Mat3.scale invInertia m)
        }


computeAABB : Body data -> AABB
computeAABB body =
    List.foldl
        (\shape ->
            shapeWorldTransform shape body
                |> Shape.aabbClosure shape.kind
                |> AABB.extend
        )
        AABB.impossible
        body.shapes


raycast :
    { from : Vec3, direction : Vec3 }
    -> Body data
    -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray body =
    List.foldl
        (\shape maybeClosestRaycastResult ->
            case Shape.raycast ray (shapeWorldTransform shape body) shape of
                Just raycastResult ->
                    case maybeClosestRaycastResult of
                        Just closestRaycastResult ->
                            if raycastResult.distance - closestRaycastResult.distance < 0 then
                                Just raycastResult

                            else
                                maybeClosestRaycastResult

                        Nothing ->
                            Just raycastResult

                Nothing ->
                    maybeClosestRaycastResult
        )
        Nothing
        body.shapes
