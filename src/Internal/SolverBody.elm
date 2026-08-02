module Internal.SolverBody exposing
    ( SolverBody
    , fromBodies
    , keepsIslandAwake
    , markAsleep
    , sentinel
    , solved
    )

import Array exposing (Array)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Matrix3 as Mat3
import Internal.Shape as Shape
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias SolverBody id =
    { body : Body
    , extId : id
    , vX : Float
    , vY : Float
    , vZ : Float
    , wX : Float
    , wY : Float
    , wZ : Float
    }


{-| Max rotation integrated in one frame (rad).
-}
maxRotationPerStep : Float
maxRotationPerStep =
    0.25 * pi


{-| True if this body forces its island to keep simulating. A dynamic body does
so until its rest timer reaches the limit. A _moving_ kinematic body does too:
it drags the dynamics it touches or is constrained to via the solve, so they
must not sleep while it moves (a parked kinematic and any static body need not).
-}
keepsIslandAwake : SolverBody id -> Bool
keepsIslandAwake { body } =
    case body.kindInt of
        2 ->
            body.sleepTime - Const.sleepTimeLimit < 0

        3 ->
            Vec3.lengthSquared body.velocity > 0 || Vec3.lengthSquared body.angularVelocity > 0

        _ ->
            False


{-| Stamp the island-asleep marker (`Const.maxNumber`) and zero the motion, so
`solved` holds the body's pose instead of integrating it. The body is rebuilt
as a literal to keep its hidden class identical to every other `Body` flowing
into `solved`.
-}
markAsleep : SolverBody id -> SolverBody id
markAsleep solverBody =
    let
        body =
            solverBody.body
    in
    { body =
        { id = body.id
        , kindInt = body.kindInt
        , transform3d = body.transform3d
        , centerOfMassTransform3d = body.centerOfMassTransform3d
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , mass = body.mass
        , geometry = body.geometry
        , worldShapesWithMaterials = body.worldShapesWithMaterials
        , force = Vec3.zero
        , torque = Vec3.zero
        , linearDamping = body.linearDamping
        , angularDamping = body.angularDamping
        , invMass = body.invMass
        , invInertia = body.invInertia
        , invInertiaWorld = body.invInertiaWorld
        , linearLock = body.linearLock
        , angularLock = body.angularLock
        , sleepTime = Const.maxNumber
        }
    , extId = solverBody.extId
    , vX = solverBody.vX
    , vY = solverBody.vY
    , vZ = solverBody.vZ
    , wX = solverBody.wX
    , wY = solverBody.wY
    , wZ = solverBody.wZ
    }


{-| Initialize a solver body with its free velocity — the total velocity it
would have after this step with no constraints: damped stored velocity plus
one tick of gravity and applied forces. Rows measure and correct these totals
directly.

The asleep marker only survives a frame if the solver re-confirms the
island, so normalize it to the limit on entry: a body whose island got merged
or dismantled then integrates again, one whose island still rests is re-marked.

-}
fromBody : Float -> Vec3 -> id -> Body -> SolverBody id
fromBody dt gravity extId body =
    if body.kindInt == 2 then
        let
            ld =
                (1.0 - body.linearDamping) ^ dt

            ad =
                (1.0 - body.angularDamping) ^ dt

            invI =
                body.invInertiaWorld
        in
        { body =
            if body.sleepTime - Const.sleepTimeLimit > 0 then
                { id = body.id
                , kindInt = body.kindInt
                , transform3d = body.transform3d
                , centerOfMassTransform3d = body.centerOfMassTransform3d
                , velocity = body.velocity
                , angularVelocity = body.angularVelocity
                , mass = body.mass
                , geometry = body.geometry
                , worldShapesWithMaterials = body.worldShapesWithMaterials
                , force = body.force
                , torque = body.torque
                , linearDamping = body.linearDamping
                , angularDamping = body.angularDamping
                , invMass = body.invMass
                , invInertia = body.invInertia
                , invInertiaWorld = body.invInertiaWorld
                , linearLock = body.linearLock
                , angularLock = body.angularLock
                , sleepTime = Const.sleepTimeLimit
                }

            else
                body
        , extId = extId
        , vX = (gravity.x + body.force.x * body.invMass) * dt + body.velocity.x * ld
        , vY = (gravity.y + body.force.y * body.invMass) * dt + body.velocity.y * ld
        , vZ = (gravity.z + body.force.z * body.invMass) * dt + body.velocity.z * ld
        , wX = (invI.m11 * body.torque.x + invI.m12 * body.torque.y + invI.m13 * body.torque.z) * dt + body.angularVelocity.x * ad
        , wY = (invI.m21 * body.torque.x + invI.m22 * body.torque.y + invI.m23 * body.torque.z) * dt + body.angularVelocity.y * ad
        , wZ = (invI.m31 * body.torque.x + invI.m32 * body.torque.y + invI.m33 * body.torque.z) * dt + body.angularVelocity.z * ad
        }

    else
        -- static: zeros; kinematic: the user-set velocity, unaffected by
        -- forces and impulses but felt by contacting dynamic bodies
        { body = body
        , extId = extId
        , vX = body.velocity.x
        , vY = body.velocity.y
        , vZ = body.velocity.z
        , wX = body.angularVelocity.x
        , wY = body.angularVelocity.y
        , wZ = body.angularVelocity.z
        }


{-| Sparse array indexed by body.id (IDs may be non-consecutive when
bodies are added mid-simulation). Unused slots are filled with the sentinel.
-}
fromBodies : Float -> Vec3 -> Int -> List ( id, Body ) -> Array (SolverBody id)
fromBodies dt gravity maxId bodiesWithIds =
    case bodiesWithIds of
        [] ->
            Array.empty

        ( firstExtId, _ ) :: _ ->
            List.foldl
                (\( extId, body ) arr -> Array.set body.id (fromBody dt gravity extId body) arr)
                (Array.repeat (maxId + 1) (sentinel firstExtId))
                bodiesWithIds


{-| Fills unused slots in the solver body array. id = -1 is impossible for real
bodies, so any array lookup that returns this sentinel can be ignored.
-}
sentinel : id -> SolverBody id
sentinel extId =
    { body =
        { id = -1
        , kindInt = 1
        , transform3d = Transform3d.atOrigin
        , centerOfMassTransform3d = Transform3d.atOrigin
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , mass = 0
        , geometry = { volume = 0, shapesWithMaterials = [], boundingSphereRadius = 0, minWidth = 0 }
        , worldShapesWithMaterials = []
        , force = Vec3.zero
        , torque = Vec3.zero
        , linearDamping = 0
        , angularDamping = 0
        , invMass = 0
        , invInertia = Vec3.zero
        , invInertiaWorld = Mat3.zero
        , linearLock = Vec3.one
        , angularLock = Vec3.one
        , sleepTime = 0
        }
    , extId = extId
    , vX = 0
    , vY = 0
    , vZ = 0
    , wX = 0
    , wY = 0
    , wZ = 0
    }


{-| Integrate a solved body to its next-frame state, returning the `extId`
paired with the new `Body` — the shape both the output list and `contactPoints`
consume, so no `SolverBody` wrapper is rebuilt. Run once after solving. Static
bodies don't move, so their existing body is reused (only the pair is allocated).

The solver body carries total velocities (gravity, forces, and damping folded
in at init), so integration just applies the motion locks and moves the
transform.

-}
solved : Float -> SolverBody id -> ( id, Body )
solved dt ({ body } as solverBody) =
    case body.kindInt of
        1 ->
            -- Static: nothing to integrate, reuse the body as-is.
            ( solverBody.extId, body )

        3 ->
            -- Kinematic
            let
                v =
                    body.velocity

                w =
                    body.angularVelocity

                newTransform3d =
                    Transform3d.normalize
                        (Transform3d.translateBy { x = v.x * dt, y = v.y * dt, z = v.z * dt }
                            (Transform3d.rotateBy { x = w.x * dt, y = w.y * dt, z = w.z * dt }
                                body.transform3d
                            )
                        )
            in
            ( solverBody.extId
            , { id = body.id
              , kindInt = body.kindInt
              , velocity = body.velocity
              , angularVelocity = body.angularVelocity
              , transform3d = newTransform3d
              , centerOfMassTransform3d = body.centerOfMassTransform3d
              , mass = body.mass
              , geometry = body.geometry
              , worldShapesWithMaterials = List.map (\( s, m ) -> ( Shape.placeIn newTransform3d s, m )) body.geometry.shapesWithMaterials
              , linearDamping = body.linearDamping
              , angularDamping = body.angularDamping
              , invMass = body.invMass
              , invInertia = body.invInertia
              , invInertiaWorld = body.invInertiaWorld
              , linearLock = body.linearLock
              , angularLock = body.angularLock
              , sleepTime = 0

              -- clear forces
              , force = Vec3.zero
              , torque = Vec3.zero
              }
            )

        _ ->
            -- Dynamic (or any other; only Dynamic is the live case).
            -- The asleep marker means the solver re-confirmed this body's
            -- island at rest this frame: skip the integrate and re-place
            -- entirely — motion and forces were zeroed when it fell asleep,
            -- so the body is reused as-is.
            if body.sleepTime - Const.sleepTimeLimit > 0 then
                ( solverBody.extId, body )

            else
                integrateDynamic dt solverBody body


{-| Integrate an awake dynamic body to its next-frame state and advance its
rest timer (the continuous near-rest time the solver later reads to decide
whether the body's whole island can sleep).
-}
integrateDynamic : Float -> SolverBody id -> Body -> ( id, Body )
integrateDynamic dt solverBody body =
    let
        newVelocity =
            { x = solverBody.vX * body.linearLock.x
            , y = solverBody.vY * body.linearLock.y
            , z = solverBody.vZ * body.linearLock.z
            }

        velocityLength =
            Vec3.length newVelocity

        -- Tunnelling guard: cap travel at half the thinnest extent per frame,
        -- so first contact lands short of the midpoint and resolution can't
        -- eject the body out the far side
        halfMinWidth =
            0.5 * body.geometry.minWidth

        linearStep =
            if
                (velocityLength == 0)
                    || (halfMinWidth == 0)
                    || (velocityLength * dt - halfMinWidth < 0)
            then
                dt

            else
                halfMinWidth / velocityLength

        newAngularVelocity =
            { x = solverBody.wX * body.angularLock.x
            , y = solverBody.wY * body.angularLock.y
            , z = solverBody.wZ * body.angularLock.z
            }

        -- cap rotation at a quarter turn per frame, so thin features can't
        -- sweep through contacts between samples
        angularSpeedSquared =
            Vec3.lengthSquared newAngularVelocity

        angularStep =
            if angularSpeedSquared * dt * dt - maxRotationPerStep * maxRotationPerStep < 0 then
                dt

            else
                maxRotationPerStep / sqrt angularSpeedSquared

        -- Sleep hysteresis: a step counts toward sleep when the speed of the
        -- body's farthest point, |v| + |w|·r, is below the rest speed (tested
        -- in squares to avoid a sqrt); a single step above resets the timer.
        -- This per-body timer only expresses that the body *wants* to sleep;
        -- the solver decides per island whether it actually does.
        sleepMargin =
            Const.sleepSpeedLimit - velocityLength

        boundingSphereRadius =
            body.geometry.boundingSphereRadius

        nextSleepTime =
            if
                (sleepMargin > 0)
                    && (Vec3.lengthSquared newAngularVelocity * boundingSphereRadius * boundingSphereRadius - sleepMargin * sleepMargin < 0)
            then
                if body.sleepTime + dt - Const.sleepTimeLimit > 0 then
                    Const.sleepTimeLimit

                else
                    body.sleepTime + dt

            else
                0

        newTransform3d =
            Transform3d.normalize
                (Transform3d.translateBy
                    { x = newVelocity.x * linearStep
                    , y = newVelocity.y * linearStep
                    , z = newVelocity.z * linearStep
                    }
                    (Transform3d.rotateBy
                        { x = newAngularVelocity.x * angularStep
                        , y = newAngularVelocity.y * angularStep
                        , z = newAngularVelocity.z * angularStep
                        }
                        body.transform3d
                    )
                )
    in
    ( solverBody.extId
    , { id = body.id
      , kindInt = body.kindInt
      , velocity = newVelocity
      , angularVelocity = newAngularVelocity
      , transform3d = newTransform3d
      , centerOfMassTransform3d = body.centerOfMassTransform3d
      , mass = body.mass
      , geometry = body.geometry
      , worldShapesWithMaterials = List.map (\( s, m ) -> ( Shape.placeIn newTransform3d s, m )) body.geometry.shapesWithMaterials
      , linearDamping = body.linearDamping
      , angularDamping = body.angularDamping
      , invMass = body.invMass
      , invInertia = body.invInertia
      , invInertiaWorld = Transform3d.invertedInertiaRotateIn newTransform3d body.invInertia
      , linearLock = body.linearLock
      , angularLock = body.angularLock
      , sleepTime = nextSleepTime

      -- clear forces
      , force = Vec3.zero
      , torque = Vec3.zero
      }
    )
