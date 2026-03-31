module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Constraint exposing (ConstraintGroup)
import Internal.Contact exposing (ContactGroup)
import Internal.Equation as Equation exposing (EquationsGroup, SolverEquation)
import Internal.Matrix3 as Mat3
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Fills unused slots in the solver body array. id = -1 is impossible for real
bodies, so any array lookup that returns this sentinel can be ignored.
-}
sentinel : id -> SolverBody id
sentinel extId =
    { body =
        { id = -1
        , transform3d = Transform3d.atOrigin
        , centerOfMassTransform3d = Transform3d.atOrigin
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , mass = 0
        , volume = 0
        , shapesWithMaterials = []
        , worldShapesWithMaterials = []
        , force = Vec3.zero
        , torque = Vec3.zero
        , boundingSphereRadius = 0
        , linearDamping = 0
        , angularDamping = 0
        , invMass = 0
        , invInertia = Mat3.zero
        , invInertiaWorld = Mat3.zero
        }
    , extId = extId
    , vX = 0
    , vY = 0
    , vZ = 0
    , wX = 0
    , wY = 0
    , wZ = 0
    }


{-| Build a sparse array indexed by body.id.

Bodies may have non-consecutive IDs (e.g. 0, 2, 5) when some were added mid-simulation.
Unused slots are filled with the sentinel. Returns Nothing for an empty body list.

-}
makeSolverBodies : Int -> List ( id, Body ) -> Array (SolverBody id)
makeSolverBodies maxId bodiesWithIds =
    case bodiesWithIds of
        [] ->
            Array.empty

        ( firstExtId, _ ) :: _ ->
            List.foldl
                (\( extId, body ) arr -> Array.set body.id (SolverBody.fromBody extId body) arr)
                (Array.repeat (maxId + 1) (sentinel firstExtId))
                bodiesWithIds


solve : Float -> Vec3 -> Int -> List ConstraintGroup -> List ContactGroup -> Int -> List ( id, Body ) -> Array (SolverBody id)
solve dt gravity iterations constraintGroups contactGroups maxId bodiesWithIds =
    case bodiesWithIds of
        [] ->
            Array.empty

        ( firstExtId, _ ) :: _ ->
            let
                ctx =
                    { dt = dt
                    , gravity = gravity
                    , gravityLength = Vec3.length gravity
                    }

                fillingBody =
                    sentinel firstExtId

                solverBodies =
                    makeSolverBodies maxId bodiesWithIds

                -- make equations from contacts
                contactEquationsGroups =
                    List.foldl
                        (\contactGroup groups ->
                            Equation.contactEquationsGroup ctx contactGroup :: groups
                        )
                        []
                        contactGroups

                -- add equations from constraints
                equationsGroups =
                    List.foldl
                        (\{ bodyId1, bodyId2, constraints } groups ->
                            case Array.get bodyId1 solverBodies of
                                Nothing ->
                                    groups

                                Just body1 ->
                                    case Array.get bodyId2 solverBodies of
                                        Nothing ->
                                            groups

                                        Just body2 ->
                                            Equation.constraintEquationsGroup
                                                ctx
                                                body1.body
                                                body2.body
                                                constraints
                                                :: groups
                        )
                        contactEquationsGroups
                        constraintGroups
            in
            step iterations 0 [] equationsGroups fillingBody solverBodies


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> SolverBody id -> Array (SolverBody id) -> Array (SolverBody id)
step number deltalambdaTot equationsGroups currentEquationsGroups prevBody1 solverBodies =
    case currentEquationsGroups of
        [] ->
            if number == 0 || deltalambdaTot - Const.precision < 0 then
                -- the max number of steps elapsed or tolerance reached
                -- put back the first body from the last equation
                Array.set prevBody1.body.id prevBody1 solverBodies

            else
                -- requeue equationsGroups for the next step
                step (number - 1) 0 [] (List.reverse equationsGroups) prevBody1 solverBodies

        { bodyId1, bodyId2, equations } :: remainingEquationsGroups ->
            let
                body1 =
                    if prevBody1.body.id - bodyId1 == 0 then
                        -- if the next equations group has the same body
                        -- then no need to get it from the array
                        prevBody1

                    else
                        case Array.get bodyId1 solverBodies of
                            Just nextBody ->
                                nextBody

                            Nothing ->
                                -- this shouldn’t happen, we just
                                -- don’t want to allocate a Just
                                prevBody1

                newSolverBodies =
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.mass == 0 then
                        -- if the next equations group has the same body,
                        -- then no need to set it to the array
                        -- also no need to update the static body
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies

                body2 =
                    case Array.get bodyId2 newSolverBodies of
                        Just nextBody ->
                            nextBody

                        Nothing ->
                            -- this shouldn’t happen, we just
                            -- don’t want to allocate a Just
                            prevBody1

                groupContext =
                    solveEquationsGroup
                        body1
                        body2
                        []
                        deltalambdaTot
                        equations
            in
            step number
                groupContext.deltalambdaTot
                ({ bodyId1 = bodyId1
                 , bodyId2 = bodyId2
                 , equations = groupContext.equations
                 }
                    :: equationsGroups
                )
                remainingEquationsGroups
                -- we don’t put body1 in the array, because we might need it
                -- in the next iteration, because this is the order in
                -- which we generated the contact equation groups (b1, b2), (b1, b3)
                -- this lets us reduce Array.set operations
                groupContext.body1
                (if groupContext.body2.body.mass > 0 then
                    Array.set bodyId2 groupContext.body2 newSolverBodies

                 else
                    -- static bodies don’t change
                    newSolverBodies
                )


type alias GroupSolveResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , equations : List SolverEquation
    , deltalambdaTot : Float
    }


solveEquationsGroup : SolverBody id -> SolverBody id -> List SolverEquation -> Float -> List SolverEquation -> GroupSolveResult id
solveEquationsGroup body1 body2 equations deltalambdaTot currentEquations =
    case currentEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , equations = equations -- reversing doesn't impact simulation
            , deltalambdaTot = deltalambdaTot
            }

        { solverLambda, equation } :: remainingEquations ->
            let
                { wA, vB, wB, minForce, maxForce, solverB, spookEps, solverInvC } =
                    equation

                -- G x Wlambda, where W are the body velocities
                gWlambda =
                    -(vB.x * body1.vX + vB.y * body1.vY + vB.z * body1.vZ)
                        + (wA.x * body1.wX + wA.y * body1.wY + wA.z * body1.wZ)
                        + (vB.x * body2.vX + vB.y * body2.vY + vB.z * body2.vZ)
                        + (wB.x * body2.wX + wB.y * body2.wY + wB.z * body2.wZ)

                deltalambdaPrev =
                    solverInvC * (solverB - gWlambda - spookEps * solverLambda)

                deltalambda =
                    if solverLambda + deltalambdaPrev - minForce < 0 then
                        minForce - solverLambda

                    else if solverLambda + deltalambdaPrev - maxForce > 0 then
                        maxForce - solverLambda

                    else
                        deltalambdaPrev

                newBody1 =
                    if body1.body.mass > 0 then
                        let
                            invI1 =
                                body1.body.invInertiaWorld

                            k1 =
                                deltalambda * body1.body.invMass
                        in
                        { body = body1.body
                        , extId = body1.extId
                        , vX = body1.vX - k1 * vB.x
                        , vY = body1.vY - k1 * vB.y
                        , vZ = body1.vZ - k1 * vB.z
                        , wX = body1.wX + (invI1.m11 * wA.x + invI1.m12 * wA.y + invI1.m13 * wA.z) * deltalambda
                        , wY = body1.wY + (invI1.m21 * wA.x + invI1.m22 * wA.y + invI1.m23 * wA.z) * deltalambda
                        , wZ = body1.wZ + (invI1.m31 * wA.x + invI1.m32 * wA.y + invI1.m33 * wA.z) * deltalambda
                        }

                    else
                        -- static bodies don't move
                        body1

                newBody2 =
                    if body2.body.mass > 0 then
                        let
                            invI2 =
                                body2.body.invInertiaWorld

                            k2 =
                                deltalambda * body2.body.invMass
                        in
                        { body = body2.body
                        , extId = body2.extId
                        , vX = body2.vX + k2 * vB.x
                        , vY = body2.vY + k2 * vB.y
                        , vZ = body2.vZ + k2 * vB.z
                        , wX = body2.wX + (invI2.m11 * wB.x + invI2.m12 * wB.y + invI2.m13 * wB.z) * deltalambda
                        , wY = body2.wY + (invI2.m21 * wB.x + invI2.m22 * wB.y + invI2.m23 * wB.z) * deltalambda
                        , wZ = body2.wZ + (invI2.m31 * wB.x + invI2.m32 * wB.y + invI2.m33 * wB.z) * deltalambda
                        }

                    else
                        -- static bodies don't move
                        body2
            in
            solveEquationsGroup
                newBody1
                newBody2
                ({ solverLambda = solverLambda + deltalambda
                 , equation = equation
                 }
                    :: equations
                )
                (deltalambdaTot + abs deltalambda)
                remainingEquations
