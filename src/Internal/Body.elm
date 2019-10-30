module Internal.Body exposing
    ( Body
    , Protected(..)
    , addGravity
    , compound
    , raycast
    , update
    , updateMassProperties
    )

import Angle
import Axis3d
import Direction3d
import Frame3d exposing (Frame3d)
import Internal.AABB as AABB exposing (AABB)
import Internal.Material as Material exposing (Material)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Shape as Shape exposing (Shape)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Point3d
import Vector3d


type Protected data
    = Protected (Body data)


type alias Body data =
    { id : Int
    , data : data
    , material : Material
    , frame3d : Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
    , velocity : Vec3
    , angularVelocity : Vec3
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
        , frame3d = defaultFrame
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
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


defaultFrame : Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
defaultFrame =
    Frame3d.atPoint Point3d.origin


addGravity : Vec3 -> Body data -> Body data
addGravity gravity body =
    { body
        | force =
            {- gravity
               |> Vec3.scale body.mass
               |> Vec3.add body.force
            -}
            { x = gravity.x * body.mass + body.force.x
            , y = gravity.y * body.mass + body.force.y
            , z = gravity.z * body.mass + body.force.z
            }
    }


update : Float -> Vec3 -> Vec3 -> Body data -> Body data
update dt vlambda wlambda body =
    let
        newVelocity =
            {- body.force
               |> Vec3.scale (body.invMass * dt)
               |> Vec3.add body.velocity
               -- from the solver
               |> Vec3.add vlambda
            -}
            { x = body.force.x * body.invMass * dt + body.velocity.x + vlambda.x
            , y = body.force.y * body.invMass * dt + body.velocity.y + vlambda.y
            , z = body.force.z * body.invMass * dt + body.velocity.z + vlambda.z
            }

        newAngularVelocity =
            {-
               body.torque
                   |> Mat3.transform body.invInertiaWorld
                   |> Vec3.scale dt
                   |> Vec3.add body.angularVelocity
                   -- from the solver
                   |> Vec3.add wlambda
            -}
            { x = (body.invInertiaWorld.m11 * body.torque.x + body.invInertiaWorld.m12 * body.torque.y + body.invInertiaWorld.m13 * body.torque.z) * dt + body.angularVelocity.x + wlambda.x
            , y = (body.invInertiaWorld.m21 * body.torque.x + body.invInertiaWorld.m22 * body.torque.y + body.invInertiaWorld.m23 * body.torque.z) * dt + body.angularVelocity.y + wlambda.y
            , z = (body.invInertiaWorld.m31 * body.torque.x + body.invInertiaWorld.m32 * body.torque.y + body.invInertiaWorld.m33 * body.torque.z) * dt + body.angularVelocity.z + wlambda.z
            }

        newFrame3d =
            case Vector3d.direction (Vector3d.fromMeters newAngularVelocity) of
                Just direction ->
                    body.frame3d
                        |> Frame3d.rotateAround (Axis3d.through (Frame3d.originPoint body.frame3d) direction) (Angle.radians (Vec3.length newAngularVelocity * dt))
                        |> Frame3d.translateBy (Vector3d.meters (newVelocity.x * dt) (newVelocity.y * dt) (newVelocity.z * dt))

                Nothing ->
                    body.frame3d
                        |> Frame3d.translateBy (Vector3d.meters (newVelocity.x * dt) (newVelocity.y * dt) (newVelocity.z * dt))
    in
    { id = body.id
    , data = body.data
    , material = body.material
    , velocity = newVelocity
    , angularVelocity = newAngularVelocity
    , frame3d = newFrame3d
    , mass = body.mass
    , shapes = body.shapes
    , boundingSphereRadius = body.boundingSphereRadius
    , invMass = body.invMass
    , inertia = body.inertia
    , invInertia = body.invInertia
    , invInertiaWorld =
        updateInvInertiaWorld False
            body.invInertia
            newFrame3d
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
            AABB.dimensions (computeAABB body)

        ix =
            1.0 / 12.0 * mass * (e.y * e.y + e.z * e.z)

        iy =
            1.0 / 12.0 * mass * (e.x * e.x + e.z * e.z)

        iz =
            1.0 / 12.0 * mass * (e.y * e.y + e.x * e.x)

        inertia =
            { x = ix, y = iy, z = iz }

        invInertia =
            { x =
                if ix > 0 then
                    1.0 / ix

                else
                    0
            , y =
                if iy > 0 then
                    1.0 / iy

                else
                    0
            , z =
                if iz > 0 then
                    1.0 / iz

                else
                    0
            }
    in
    { body
        | invMass = invMass
        , inertia = inertia
        , invInertia = invInertia
        , invInertiaWorld =
            updateInvInertiaWorld True
                invInertia
                body.frame3d
                body.invInertiaWorld
    }


updateInvInertiaWorld : Bool -> Vec3 -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Mat3 -> Mat3
updateInvInertiaWorld force invInertia frame3d invInertiaWorld =
    if not force && invInertia.x == invInertia.y && invInertia.y == invInertia.z then
        invInertiaWorld

    else
        {-
           let
               m =
                   Quaternion.toMat3 orientation
           in
           Mat3.mul
               (Mat3.transpose m)
               (Mat3.scale invInertia m)
        -}
        let
            xDirection =
                Direction3d.unwrap (Frame3d.xDirection frame3d)

            yDirection =
                Direction3d.unwrap (Frame3d.yDirection frame3d)

            zDirection =
                Direction3d.unwrap (Frame3d.zDirection frame3d)

            am11 =
                xDirection.x

            am21 =
                xDirection.y

            am31 =
                xDirection.z

            am12 =
                yDirection.x

            am22 =
                yDirection.y

            am32 =
                yDirection.z

            am13 =
                zDirection.x

            am23 =
                zDirection.y

            am33 =
                zDirection.z

            bm11 =
                am11 * invInertia.x

            bm21 =
                am12 * invInertia.x

            bm31 =
                am13 * invInertia.x

            bm12 =
                am21 * invInertia.y

            bm22 =
                am22 * invInertia.y

            bm32 =
                am23 * invInertia.y

            bm13 =
                am31 * invInertia.z

            bm23 =
                am32 * invInertia.z

            bm33 =
                am33 * invInertia.z
        in
        { m11 = am11 * bm11 + am12 * bm21 + am13 * bm31
        , m21 = am21 * bm11 + am22 * bm21 + am23 * bm31
        , m31 = am31 * bm11 + am32 * bm21 + am33 * bm31
        , m12 = am11 * bm12 + am12 * bm22 + am13 * bm32
        , m22 = am21 * bm12 + am22 * bm22 + am23 * bm32
        , m32 = am31 * bm12 + am32 * bm22 + am33 * bm32
        , m13 = am11 * bm13 + am12 * bm23 + am13 * bm33
        , m23 = am21 * bm13 + am22 * bm23 + am23 * bm33
        , m33 = am31 * bm13 + am32 * bm23 + am33 * bm33
        }


computeAABB : Body data -> AABB
computeAABB body =
    List.foldl
        (\shape ->
            Shape.aabbClosure shape.kind shape.frame3d
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
            case Shape.raycast ray (Frame3d.placeIn body.frame3d shape.frame3d) shape of
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
