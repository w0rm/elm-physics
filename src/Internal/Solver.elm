module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Equation as Equation exposing (EquationsGroup, SolverEquation)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.World exposing (World)


maxIterations : Int
maxIterations =
    20


makeSolverBodies : Int -> List (Body data) -> ( Array (SolverBody data), Maybe (SolverBody data) )
makeSolverBodies nextBodyId bodies =
    case bodies of
        [] ->
            ( Array.empty, Nothing )

        firstBody :: _ ->
            let
                -- a hack to use an empty body to fill the gaps
                -- in the array and avoid extra boxing (Array (Maybe (SolverBody data)))
                fillingBody =
                    SolverBody.fromBody { firstBody | id = -1 }

                allBodies =
                    Array.repeat nextBodyId fillingBody
            in
            ( List.foldl
                (\body -> Array.set body.id (SolverBody.fromBody body))
                allBodies
                bodies
            , Just fillingBody
            )


solve : Float -> World data -> World data
solve dt world =
    let
        ctx =
            { dt = dt
            , gravity = world.gravity
            , gravityLength = Vec3.length world.gravity
            }

        -- make equations from contacts
        contactEquationsGroups =
            List.foldl
                (\contactGroup groups ->
                    Equation.contactEquationsGroup
                        ctx
                        contactGroup
                        :: groups
                )
                []
                world.contactGroups

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
                world.constraints

        ( solverBodies, maybeFillingBody ) =
            makeSolverBodies world.nextBodyId world.bodies

        solvedBodies =
            case maybeFillingBody of
                Just fillingBody ->
                    step maxIterations 0 [] equationsGroups fillingBody solverBodies

                Nothing ->
                    solverBodies
    in
    updateBodies ctx solvedBodies world


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> SolverBody data -> Array (SolverBody data) -> Array (SolverBody data)
step number deltalambdaTot equationsGroups currentEquationsGroups prevBody1 solverBodies =
    case currentEquationsGroups of
        [] ->
            if number == 0 || deltalambdaTot - Const.precision < 0 then
                -- the max number of steps elapsed or tolerance reached
                -- put back the first body from the last equation
                Array.set prevBody1.body.id prevBody1 solverBodies

            else
                -- requeue equationsGropus for the next step
                step (number - 1) 0 [] (List.reverse equationsGroups) prevBody1 solverBodies

        { bodyId1, bodyId2, equations } :: remainingEquationsGroups ->
            let
                maybeBody1 =
                    if bodyId1 - prevBody1.body.id == 0 then
                        -- if the next equations group has the same body
                        -- then no need to get it from the array
                        Just prevBody1

                    else
                        Array.get bodyId1 solverBodies

                newSolverBodies =
                    if bodyId1 - prevBody1.body.id == 0 || prevBody1.body.mass == 0 then
                        -- if the next equations group has the same body,
                        -- then no need to set it to the array
                        -- also no need to update the static body
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies
            in
            case maybeBody1 of
                Just body1 ->
                    case Array.get bodyId2 newSolverBodies of
                        Just body2 ->
                            let
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
                                -- we don't put body1 in the array, because we might need it
                                -- in the next iteration, because this is the order in
                                -- which we generated the contact equation groups (b1, b2), (b1, b3)
                                -- this let's us reduce Array operations
                                groupContext.body1
                                (if groupContext.body2.body.mass > 0 then
                                    Array.set bodyId2 groupContext.body2 newSolverBodies

                                 else
                                    -- static bodies don’t change
                                    newSolverBodies
                                )

                        _ ->
                            -- Should never happen
                            step number
                                deltalambdaTot
                                equationsGroups
                                remainingEquationsGroups
                                prevBody1
                                newSolverBodies

                _ ->
                    -- Should never happen
                    step number
                        deltalambdaTot
                        equationsGroups
                        remainingEquationsGroups
                        prevBody1
                        newSolverBodies


type alias GroupSolveResult data =
    { body1 : SolverBody data
    , body2 : SolverBody data
    , equations : List SolverEquation
    , deltalambdaTot : Float
    }


solveEquationsGroup : SolverBody data -> SolverBody data -> List SolverEquation -> Float -> List SolverEquation -> GroupSolveResult data
solveEquationsGroup body1 body2 equations deltalambdaTot currentEquations =
    case currentEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , equations = equations -- reversing doesn’t impact simulation
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

                invI1 =
                    body1.body.invInertiaWorld

                invI2 =
                    body2.body.invInertiaWorld

                k1 =
                    deltalambda * body1.body.invMass

                k2 =
                    deltalambda * body2.body.invMass

                newBody1 =
                    if body1.body.mass > 0 then
                        { body = body1.body
                        , vX = body1.vX - k1 * vB.x
                        , vY = body1.vY - k1 * vB.y
                        , vZ = body1.vZ - k1 * vB.z
                        , wX = body1.wX + (invI1.m11 * wA.x + invI1.m12 * wA.y + invI1.m13 * wA.z) * deltalambda
                        , wY = body1.wY + (invI1.m21 * wA.x + invI1.m22 * wA.y + invI1.m23 * wA.z) * deltalambda
                        , wZ = body1.wZ + (invI1.m31 * wA.x + invI1.m32 * wA.y + invI1.m33 * wA.z) * deltalambda
                        }

                    else
                        -- static bodies don’t move
                        body1

                newBody2 =
                    if body2.body.mass > 0 then
                        { body = body2.body
                        , vX = body2.vX + k2 * vB.x
                        , vY = body2.vY + k2 * vB.y
                        , vZ = body2.vZ + k2 * vB.z
                        , wX = body2.wX + (invI2.m11 * wB.x + invI2.m12 * wB.y + invI2.m13 * wB.z) * deltalambda
                        , wY = body2.wY + (invI2.m21 * wB.x + invI2.m22 * wB.y + invI2.m23 * wB.z) * deltalambda
                        , wZ = body2.wZ + (invI2.m31 * wB.x + invI2.m32 * wB.y + invI2.m33 * wB.z) * deltalambda
                        }

                    else
                        -- static bodies don’t move
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


updateBodies : { dt : Float, gravity : Vec3, gravityLength : Float } -> Array (SolverBody data) -> World data -> World data
updateBodies ctx bodies world =
    let
        simulatedBodies =
            Array.map
                (\solverBody ->
                    -- id == -1 is to skip the filling body to avoid (Array (Maybe (SolverBody data)))
                    if solverBody.body.id + 1 > 0 then
                        if solverBody.body.mass > 0 then
                            SolverBody.toBody ctx solverBody

                        else
                            -- static bodies don’t move
                            solverBody.body

                    else
                        solverBody.body
                )
                bodies
    in
    { world
        | bodies =
            Array.toList simulatedBodies
                |> List.filter (\{ id } -> id + 1 > 0)
        , simulatedBodies = simulatedBodies
    }
