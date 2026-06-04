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


{-| Square of the linear+angular speed below which a frame counts toward sleep.
~0.1 m/s — above a resting PGS stack's jitter, below genuine motion.
-}
sleepThresholdSq : Float
sleepThresholdSq =
    0.01


{-| Consecutive near-rest frames before a body wants to sleep (≈1 s at 60 fps).
A `sleepFrames` of exactly this means "wants to sleep"; the solver stamps
`sleepFrameLimit + 1` to mean "island-confirmed asleep, hold the pose".
-}
sleepFrameLimit : Int
sleepFrameLimit =
    60


{-| True if this body forces its island to keep simulating. A dynamic body does
so until its rest timer reaches the limit. A _moving_ kinematic body does too:
it drags the dynamics it touches or is constrained to via the solve, so they
must not sleep while it moves (a parked kinematic and any static body need not).
-}
keepsIslandAwake : SolverBody id -> Bool
keepsIslandAwake { body } =
    case body.kindInt of
        2 ->
            body.sleepFrames - sleepFrameLimit < 0

        3 ->
            Vec3.lengthSquared body.velocity > 0 || Vec3.lengthSquared body.angularVelocity > 0

        _ ->
            False


{-| Stamp the island-asleep marker (`sleepFrameLimit + 1`) so `solved` holds the
body's pose instead of integrating it. The body is rebuilt as a literal to keep
its hidden class identical to every other `Body` flowing into `solved`.
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
        , sleepFrames = sleepFrameLimit + 1
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
        { body = body
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
        , geometry = { volume = 0, shapesWithMaterials = [], boundingSphereRadius = 0 }
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
        , sleepFrames = 0
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
              , sleepFrames = 0

              -- clear forces
              , force = Vec3.zero
              , torque = Vec3.zero
              }
            )

        _ ->
            -- Dynamic (or any other; only Dynamic is the live case).
            -- A `sleepFrames` above the limit is the solver's island-asleep
            -- marker: the body's whole island is at rest, so skip the integrate
            -- and re-place entirely and hold the pose. Reset the marker to the
            -- limit so that if the island wakes (the solver won't re-mark it)
            -- this body integrates again next frame.
            if body.sleepFrames > sleepFrameLimit then
                ( solverBody.extId
                , { id = body.id
                  , kindInt = body.kindInt
                  , velocity = Vec3.zero
                  , angularVelocity = Vec3.zero
                  , transform3d = body.transform3d
                  , centerOfMassTransform3d = body.centerOfMassTransform3d
                  , mass = body.mass
                  , geometry = body.geometry
                  , worldShapesWithMaterials = body.worldShapesWithMaterials
                  , linearDamping = body.linearDamping
                  , angularDamping = body.angularDamping
                  , invMass = body.invMass
                  , invInertia = body.invInertia
                  , invInertiaWorld = body.invInertiaWorld
                  , linearLock = body.linearLock
                  , angularLock = body.angularLock
                  , sleepFrames = sleepFrameLimit

                  -- clear forces
                  , force = Vec3.zero
                  , torque = Vec3.zero
                  }
                )

            else
                integrateDynamic dt solverBody body


{-| Integrate an awake dynamic body to its next-frame state and advance its
sleep timer (the consecutive-near-rest-frame count the solver later reads to
decide whether the body's whole island can sleep).
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

        -- This hack is needed to minimize tunnelling
        -- we don't let the body to move more
        -- than half of its bounding radius in a frame
        boundingSphereRadius =
            body.geometry.boundingSphereRadius

        cappedVelocity =
            if
                (velocityLength == 0)
                    || (boundingSphereRadius == 0)
                    || (velocityLength * dt - boundingSphereRadius < 0)
            then
                newVelocity

            else
                Vec3.scale (boundingSphereRadius / (velocityLength * dt)) newVelocity

        newAngularVelocity =
            { x = solverBody.wX * body.angularLock.x
            , y = solverBody.wY * body.angularLock.y
            , z = solverBody.wZ * body.angularLock.z
            }

        -- Sleep hysteresis: count consecutive near-rest frames; a single
        -- frame above the threshold resets it. This per-body timer only
        -- expresses that the body *wants* to sleep; the solver decides
        -- per island whether it actually does.
        restingSq =
            velocityLength * velocityLength + Vec3.lengthSquared newAngularVelocity

        nextSleepFrames =
            if restingSq - sleepThresholdSq < 0 then
                min sleepFrameLimit (body.sleepFrames + 1)

            else
                0

        newTransform3d =
            Transform3d.normalize
                (Transform3d.translateBy
                    { x = cappedVelocity.x * dt
                    , y = cappedVelocity.y * dt
                    , z = cappedVelocity.z * dt
                    }
                    (Transform3d.rotateBy
                        { x = newAngularVelocity.x * dt
                        , y = newAngularVelocity.y * dt
                        , z = newAngularVelocity.z * dt
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
      , sleepFrames = nextSleepFrames

      -- clear forces
      , force = Vec3.zero
      , torque = Vec3.zero
      }
    )
