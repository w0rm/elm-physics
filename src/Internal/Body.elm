module Internal.Body exposing
    ( Body
    , Protected(..)
    , addGravity
    , applyForce
    , applyImpulse
    , centerOfMass
    , compound
    , raycast
    , update
    , updateMassProperties
    )

import Internal.AABB as AABB exposing (AABB)
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Material as Material exposing (Material)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Shape as Shape exposing (Shape)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)


type Protected data
    = Protected (Body data)


type alias Body data =
    { id : Int
    , data : data
    , material : Material
    , transform3d : Transform3d WorldCoordinates { defines : CenterOfMassCoordinates }
    , centerOfMassTransform3d : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    , velocity : Vec3
    , angularVelocity : Vec3
    , mass : Float
    , shapes : List (Shape CenterOfMassCoordinates)
    , force : Vec3
    , torque : Vec3
    , boundingSphereRadius : Float

    -- damping
    , linearDamping : Float
    , angularDamping : Float

    -- mass props
    , invMass : Float
    , inertia : Vec3
    , invInertia : Vec3
    , invInertiaWorld : Mat3
    }


centerOfMass : List (Shape BodyCoordinates) -> Vec3
centerOfMass shapes =
    let
        totalVolume =
            List.foldl (\{ volume } sum -> sum + volume) 0 shapes
    in
    if totalVolume > 0 then
        List.foldl
            (\shape ->
                Vec3.add
                    (Vec3.scale
                        (shape.volume / totalVolume)
                        (Transform3d.originPoint shape.transform3d)
                    )
            )
            Vec3.zero
            shapes

    else
        Vec3.zero


compound : List (Shape BodyCoordinates) -> data -> Body data
compound shapes data =
    let
        centerOfMassPoint =
            centerOfMass shapes

        centerOfMassTransform3d : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
        centerOfMassTransform3d =
            Transform3d.atPoint centerOfMassPoint

        inverseCenterOfMassTransform3d : Transform3d CenterOfMassCoordinates { defines : BodyCoordinates }
        inverseCenterOfMassTransform3d =
            Transform3d.inverse centerOfMassTransform3d

        bodyTransform3d : Transform3d WorldCoordinates { defines : BodyCoordinates }
        bodyTransform3d =
            Transform3d.atOrigin

        transform3d : Transform3d WorldCoordinates { defines : CenterOfMassCoordinates }
        transform3d =
            Transform3d.placeIn bodyTransform3d centerOfMassTransform3d

        movedShapes : List (Shape CenterOfMassCoordinates)
        movedShapes =
            List.map
                (\shape ->
                    { kind = shape.kind
                    , volume = shape.volume
                    , transform3d = Transform3d.placeIn inverseCenterOfMassTransform3d shape.transform3d
                    }
                )
                shapes
    in
    updateMassProperties
        { id = -1
        , data = data
        , material = Material.default
        , transform3d = transform3d
        , centerOfMassTransform3d = centerOfMassTransform3d
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , mass = 0
        , shapes = movedShapes
        , force = Vec3.zero
        , torque = Vec3.zero
        , boundingSphereRadius = List.foldl Shape.expandBoundingSphereRadius 0 movedShapes

        -- default damping
        , linearDamping = 0.01
        , angularDamping = 0.01

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
        -- Apply damping https://code.google.com/archive/p/bullet/issues/74
        ld =
            (1.0 - body.linearDamping) ^ dt

        ad =
            (1.0 - body.angularDamping) ^ dt

        newVelocity =
            {- body.force
               |> Vec3.scale (body.invMass * dt)
               |> Vec3.add body.velocity
               -- from the solver
               |> Vec3.add vlambda
            -}
            { x = body.force.x * body.invMass * dt + body.velocity.x * ld + vlambda.x
            , y = body.force.y * body.invMass * dt + body.velocity.y * ld + vlambda.y
            , z = body.force.z * body.invMass * dt + body.velocity.z * ld + vlambda.z
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
            { x = (body.invInertiaWorld.m11 * body.torque.x + body.invInertiaWorld.m12 * body.torque.y + body.invInertiaWorld.m13 * body.torque.z) * dt + body.angularVelocity.x * ad + wlambda.x
            , y = (body.invInertiaWorld.m21 * body.torque.x + body.invInertiaWorld.m22 * body.torque.y + body.invInertiaWorld.m23 * body.torque.z) * dt + body.angularVelocity.y * ad + wlambda.y
            , z = (body.invInertiaWorld.m31 * body.torque.x + body.invInertiaWorld.m32 * body.torque.y + body.invInertiaWorld.m33 * body.torque.z) * dt + body.angularVelocity.z * ad + wlambda.z
            }

        newTransform3d =
            body.transform3d
                |> Transform3d.rotateBy { x = newAngularVelocity.x * dt, y = newAngularVelocity.y * dt, z = newAngularVelocity.z * dt }
                |> Transform3d.translateBy { x = newVelocity.x * dt, y = newVelocity.y * dt, z = newVelocity.z * dt }
                |> Transform3d.normalize
    in
    { id = body.id
    , data = body.data
    , material = body.material
    , velocity = newVelocity
    , angularVelocity = newAngularVelocity
    , transform3d = newTransform3d
    , centerOfMassTransform3d = body.centerOfMassTransform3d
    , mass = body.mass
    , shapes = body.shapes
    , boundingSphereRadius = body.boundingSphereRadius
    , linearDamping = body.linearDamping
    , angularDamping = body.angularDamping
    , invMass = body.invMass
    , inertia = body.inertia
    , invInertia = body.invInertia
    , invInertiaWorld =
        updateInvInertiaWorld False
            body.invInertia
            newTransform3d
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
                body.transform3d
                body.invInertiaWorld
    }


updateInvInertiaWorld : Bool -> Vec3 -> Transform3d WorldCoordinates { defines : CenterOfMassCoordinates } -> Mat3 -> Mat3
updateInvInertiaWorld force invInertia transform3d invInertiaWorld =
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
            { m11, m21, m31, m12, m22, m32, m13, m23, m33 } =
                Transform3d.orientation transform3d

            bm11 =
                m11 * invInertia.x

            bm21 =
                m12 * invInertia.x

            bm31 =
                m13 * invInertia.x

            bm12 =
                m21 * invInertia.y

            bm22 =
                m22 * invInertia.y

            bm32 =
                m23 * invInertia.y

            bm13 =
                m31 * invInertia.z

            bm23 =
                m32 * invInertia.z

            bm33 =
                m33 * invInertia.z
        in
        { m11 = m11 * bm11 + m12 * bm21 + m13 * bm31
        , m21 = m21 * bm11 + m22 * bm21 + m23 * bm31
        , m31 = m31 * bm11 + m32 * bm21 + m33 * bm31
        , m12 = m11 * bm12 + m12 * bm22 + m13 * bm32
        , m22 = m21 * bm12 + m22 * bm22 + m23 * bm32
        , m32 = m31 * bm12 + m32 * bm22 + m33 * bm32
        , m13 = m11 * bm13 + m12 * bm23 + m13 * bm33
        , m23 = m21 * bm13 + m22 * bm23 + m23 * bm33
        , m33 = m31 * bm13 + m32 * bm23 + m33 * bm33
        }


applyImpulse : Float -> Vec3 -> Vec3 -> Body data -> Body data
applyImpulse amount direction point body =
    let
        relativePoint =
            Vec3.sub point (Transform3d.originPoint body.transform3d)

        impulse =
            Vec3.scale amount direction

        velocity =
            Vec3.scale body.invMass impulse

        angularVelocity =
            Mat3.transform body.invInertiaWorld (Vec3.cross relativePoint impulse)
    in
    { body
        | velocity = Vec3.add body.velocity velocity
        , angularVelocity = Vec3.add body.angularVelocity angularVelocity
    }


applyForce : Float -> Vec3 -> Vec3 -> Body data -> Body data
applyForce amount direction point body =
    let
        relativePoint =
            Vec3.sub point (Transform3d.originPoint body.transform3d)

        force =
            Vec3.scale amount direction

        torque =
            Vec3.cross relativePoint force
    in
    { body | force = force, torque = torque }


computeAABB : Body data -> AABB
computeAABB body =
    List.foldl
        (\shape ->
            Shape.aabbClosure shape.kind shape.transform3d
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
            case Shape.raycast ray (Transform3d.placeIn body.transform3d shape.transform3d) shape of
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
