module Internal.SolverBody exposing
    ( SolverBody
    , addToWlambda
    , fromBody
    , toBody
    )

import Internal.Body as Body exposing (Body)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias SolverBody data =
    { body : Body data
    , vX : Float
    , vY : Float
    , vZ : Float
    , wX : Float
    , wY : Float
    , wZ : Float
    }


fromBody : Body data -> SolverBody data
fromBody body =
    { body = body
    , vX = 0
    , vY = 0
    , vZ = 0
    , wX = 0
    , wY = 0
    , wZ = 0
    }


toBody : Float -> SolverBody data -> Body data
toBody dt { body, vX, vY, vZ, wX, wY, wZ } =
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
            { x = body.force.x * body.invMass * dt + body.velocity.x * ld + vX
            , y = body.force.y * body.invMass * dt + body.velocity.y * ld + vY
            , z = body.force.z * body.invMass * dt + body.velocity.z * ld + vZ
            }

        velocityLength =
            Vec3.length newVelocity

        -- This hack is needed to minimize tunnelling
        -- we don’t let the body to move more
        -- than half of its bounding radius in a frame
        cappedVelocity =
            if
                (velocityLength == 0)
                    || (body.boundingSphereRadius == 0)
                    || (velocityLength * dt - body.boundingSphereRadius < 0)
            then
                newVelocity

            else
                Vec3.scale (body.boundingSphereRadius / (velocityLength * dt)) newVelocity

        newAngularVelocity =
            {-
               body.torque
                   |> Mat3.transform body.invInertiaWorld
                   |> Vec3.scale dt
                   |> Vec3.add body.angularVelocity
                   -- from the solver
                   |> Vec3.add wlambda
            -}
            { x = (body.invInertiaWorld.m11 * body.torque.x + body.invInertiaWorld.m12 * body.torque.y + body.invInertiaWorld.m13 * body.torque.z) * dt + body.angularVelocity.x * ad + wX
            , y = (body.invInertiaWorld.m21 * body.torque.x + body.invInertiaWorld.m22 * body.torque.y + body.invInertiaWorld.m23 * body.torque.z) * dt + body.angularVelocity.y * ad + wY
            , z = (body.invInertiaWorld.m31 * body.torque.x + body.invInertiaWorld.m32 * body.torque.y + body.invInertiaWorld.m33 * body.torque.z) * dt + body.angularVelocity.z * ad + wZ
            }

        newTransform3d =
            body.transform3d
                |> Transform3d.rotateBy { x = newAngularVelocity.x * dt, y = newAngularVelocity.y * dt, z = newAngularVelocity.z * dt }
                |> Transform3d.translateBy { x = cappedVelocity.x * dt, y = cappedVelocity.y * dt, z = cappedVelocity.z * dt }
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
        Body.updateInvInertiaWorld False
            body.invInertia
            newTransform3d
            body.invInertiaWorld

    -- clear forces
    , force = Vec3.zero
    , torque = Vec3.zero
    }


addToWlambda : Float -> Float -> Vec3 -> Vec3 -> SolverBody data -> SolverBody data
addToWlambda deltalambda k spatial rotational { body, vX, vY, vZ, wX, wY, wZ } =
    { body = body
    , vX = k * spatial.x * deltalambda * body.invMass + vX
    , vY = k * spatial.y * deltalambda * body.invMass + vY
    , vZ = k * spatial.z * deltalambda * body.invMass + vZ
    , wX = (body.invInertiaWorld.m11 * rotational.x + body.invInertiaWorld.m12 * rotational.y + body.invInertiaWorld.m13 * rotational.z) * deltalambda + wX
    , wY = (body.invInertiaWorld.m21 * rotational.x + body.invInertiaWorld.m22 * rotational.y + body.invInertiaWorld.m23 * rotational.z) * deltalambda + wY
    , wZ = (body.invInertiaWorld.m31 * rotational.x + body.invInertiaWorld.m32 * rotational.y + body.invInertiaWorld.m33 * rotational.z) * deltalambda + wZ
    }
