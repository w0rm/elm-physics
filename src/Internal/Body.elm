module Internal.Body exposing
    ( Body
    , Protected(..)
    , addGravity
    , compound
    , raycast
    , shapeWorldTransform
    , update
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


type Protected data
    = Protected (Body data)


type alias Body data =
    { id : Int
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


update : Float -> Vec3 -> Vec3 -> Body data -> Body data
update dt vlambda wlambda body =
    let
        newVelocity =
            body.force
                |> Vec3.scale (body.invMass * dt)
                |> Vec3.add body.velocity
                -- from the solver
                |> Vec3.add vlambda

        newAngularVelocity =
            body.torque
                |> Mat3.transform body.invInertiaWorld
                |> Vec3.scale dt
                |> Vec3.add body.angularVelocity
                -- from the solver
                |> Vec3.add wlambda

        newOrientation =
            body.orientation
                |> Quaternion.rotateBy (Vec3.scale (dt / 2) newAngularVelocity)
                |> Quaternion.normalize
    in
    { id = body.id
    , data = body.data
    , material = body.material
    , velocity = newVelocity
    , angularVelocity = newAngularVelocity
    , position =
        newVelocity
            |> Vec3.scale dt
            |> Vec3.add body.position
    , orientation = newOrientation
    , mass = body.mass
    , shapes = body.shapes
    , boundingSphereRadius = body.boundingSphereRadius
    , invMass = body.invMass
    , inertia = body.inertia
    , invInertia = body.invInertia
    , invInertiaWorld =
        updateInvInertiaWorld False
            body.invInertia
            newOrientation
            body.invInertiaWorld

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
            AABB.dimesions (computeAABB body)

        ix =
            1.0 / 12.0 * mass * (e.y * e.y + e.z * e.z)

        iy =
            1.0 / 12.0 * mass * (e.x * e.x + e.z * e.z)

        iz =
            1.0 / 12.0 * mass * (e.y * e.y + e.x * e.x)

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
    { body
        | invMass = invMass
        , inertia = inertia
        , invInertia = invInertia
        , invInertiaWorld =
            updateInvInertiaWorld True
                invInertia
                body.orientation
                body.invInertiaWorld
    }


updateInvInertiaWorld : Bool -> Vec3 -> Quaternion -> Mat3 -> Mat3
updateInvInertiaWorld force invInertia orientation invInertiaWorld =
    if not force && invInertia.x == invInertia.y && invInertia.y == invInertia.z then
        invInertiaWorld

    else
        let
            m =
                Quaternion.toMat3 orientation
        in
        Mat3.mul
            (Mat3.transpose m)
            (Mat3.scale invInertia m)


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
