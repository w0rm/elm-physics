module Internal.SolverBody exposing
    ( SolverBody
    , fromBody
    , toBody
    )

import Internal.Body exposing (Body)
import Internal.Shape as Shape
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


toBody : { dt : Float, gravity : Vec3, gravityLength : Float } -> SolverBody data -> Body data
toBody { dt, gravity } { body, vX, vY, vZ, wX, wY, wZ } =
    let
        -- Apply damping https://code.google.com/archive/p/bullet/issues/74
        ld =
            (1.0 - body.linearDamping) ^ dt

        ad =
            (1.0 - body.angularDamping) ^ dt

        newVelocity =
            { x = (gravity.x + body.force.x * body.invMass) * dt + body.velocity.x * ld + vX
            , y = (gravity.y + body.force.y * body.invMass) * dt + body.velocity.y * ld + vY
            , z = (gravity.z + body.force.z * body.invMass) * dt + body.velocity.z * ld + vZ
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
    , worldShapes = Shape.shapesPlaceIn newTransform3d body.shapes
    , boundingSphereRadius = body.boundingSphereRadius
    , linearDamping = body.linearDamping
    , angularDamping = body.angularDamping
    , invMass = body.invMass
    , invInertia = body.invInertia
    , invInertiaWorld = Transform3d.invertedInertiaRotateIn newTransform3d body.invInertia

    -- clear forces
    , force = Vec3.zero
    , torque = Vec3.zero
    }
