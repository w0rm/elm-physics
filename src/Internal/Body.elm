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
            {- gravity
               |> Vec3.scale body.mass
               |> Vec3.add body.force
            -}
            { x = gravity.x * body.mass + body.force.x
            , y = gravity.y * body.mass + body.force.y
            , z = gravity.z * body.mass + body.force.z
            }
    }


shapeWorldTransform : Shape -> Body data -> Transform
shapeWorldTransform shape { position, orientation } =
    { orientation = Quaternion.mul orientation shape.orientation
    , position =
        {- Vec3.add
           position
           (Quaternion.rotate orientation shape.position)
        -}
        let
            ix =
                orientation.w * shape.position.x + orientation.y * shape.position.z - orientation.z * shape.position.y

            iy =
                orientation.w * shape.position.y + orientation.z * shape.position.x - orientation.x * shape.position.z

            iz =
                orientation.w * shape.position.z + orientation.x * shape.position.y - orientation.y * shape.position.x

            iw =
                -orientation.x * shape.position.x - orientation.y * shape.position.y - orientation.z * shape.position.z
        in
        { x = ix * orientation.w + iw * -orientation.x + iy * -orientation.z - iz * -orientation.y + position.x
        , y = iy * orientation.w + iw * -orientation.y + iz * -orientation.x - ix * -orientation.z + position.y
        , z = iz * orientation.w + iw * -orientation.z + ix * -orientation.y - iy * -orientation.x + position.z
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

        newPosition =
            {- newVelocity
               |> Vec3.scale dt
               |> Vec3.add body.position
            -}
            { x = newVelocity.x * dt + body.position.x
            , y = newVelocity.y * dt + body.position.y
            , z = newVelocity.z * dt + body.position.z
            }

        newOrientation =
            {-
               body.orientation
                   |> Quaternion.rotateBy (Vec3.scale (dt / 2) newAngularVelocity)
                   |> Quaternion.normalize
            -}
            { x = x / len
            , y = y / len
            , z = z / len
            , w = w / len
            }

        x =
            body.orientation.x + (newAngularVelocity.x * body.orientation.w + newAngularVelocity.y * body.orientation.z - newAngularVelocity.z * body.orientation.y) * (dt / 2)

        y =
            body.orientation.y + (newAngularVelocity.y * body.orientation.w + newAngularVelocity.z * body.orientation.x - newAngularVelocity.x * body.orientation.z) * (dt / 2)

        z =
            body.orientation.z + (newAngularVelocity.z * body.orientation.w + newAngularVelocity.x * body.orientation.y - newAngularVelocity.y * body.orientation.x) * (dt / 2)

        w =
            body.orientation.w + (-newAngularVelocity.x * body.orientation.x - newAngularVelocity.y * body.orientation.y - newAngularVelocity.z * body.orientation.z) * (dt / 2)

        len =
            sqrt (x * x + y * y + z * z + w * w)
    in
    { id = body.id
    , data = body.data
    , material = body.material
    , velocity = newVelocity
    , angularVelocity = newAngularVelocity
    , position = newPosition
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
            AABB.dimensions (computeAABB body)

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
            am11 =
                1 - 2 * orientation.y * orientation.y - 2 * orientation.z * orientation.z

            am21 =
                2 * orientation.x * orientation.y - 2 * orientation.w * orientation.z

            am31 =
                2 * orientation.x * orientation.z + 2 * orientation.w * orientation.y

            am12 =
                2 * orientation.x * orientation.y + 2 * orientation.w * orientation.z

            am22 =
                1 - 2 * orientation.x * orientation.x - 2 * orientation.z * orientation.z

            am32 =
                2 * orientation.y * orientation.z - 2 * orientation.w * orientation.x

            am13 =
                2 * orientation.x * orientation.z - 2 * orientation.w * orientation.y

            am23 =
                2 * orientation.y * orientation.z + 2 * orientation.w * orientation.x

            am33 =
                1 - 2 * orientation.x * orientation.x - 2 * orientation.y * orientation.y

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
