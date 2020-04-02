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


makeSolverBodies : Int -> List (Body data) -> Array (SolverBody data)
makeSolverBodies nextBodyId bodies =
    case bodies of
        [] ->
            Array.empty

        firstBody :: _ ->
            let
                -- a hack to use an empty body to fill the gaps
                -- in the array and avoid extra boxing (Array (Maybe (SolverBody data)))
                allBodies =
                    { firstBody | id = -1 }
                        |> SolverBody.fromBody
                        |> Array.repeat nextBodyId
            in
            List.foldl
                (\body -> Array.set body.id (SolverBody.fromBody body))
                allBodies
                bodies


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

        solverBodies =
            makeSolverBodies world.nextBodyId world.bodies

        solvedBodies =
            step maxIterations 0 [] equationsGroups solverBodies
    in
    updateBodies ctx solvedBodies world


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> Array (SolverBody data) -> Array (SolverBody data)
step number deltalambdaTot equationsGroups currentEquationsGroups solverBodies =
    case currentEquationsGroups of
        [] ->
            if number == 0 || deltalambdaTot - Const.precision < 0 then
                -- the max number of steps elapsed or tolerance reached
                Array.map
                    (\b ->
                        { body = b.body
                        , vX = b.vX_
                        , vY = b.vY_
                        , vZ = b.vZ_
                        , wX = b.wX_
                        , wY = b.wY_
                        , wZ = b.wZ_
                        , vX_ = b.vX_
                        , vY_ = b.vY_
                        , vZ_ = b.vZ_
                        , wX_ = b.wX_
                        , wY_ = b.wY_
                        , wZ_ = b.wZ_
                        }
                    )
                    solverBodies

            else
                -- requeue equationsGropus for the next step
                step (number - 1)
                    0
                    []
                    (List.reverse equationsGroups)
                    (Array.map
                        (\b ->
                            { body = b.body
                            , vX = b.vX_
                            , vY = b.vY_
                            , vZ = b.vZ_
                            , wX = b.wX_
                            , wY = b.wY_
                            , wZ = b.wZ_
                            , vX_ = b.vX_
                            , vY_ = b.vY_
                            , vZ_ = b.vZ_
                            , wX_ = b.wX_
                            , wY_ = b.wY_
                            , wZ_ = b.wZ_
                            }
                        )
                        solverBodies
                    )

        { bodyId1, bodyId2, equations } :: remainingEquationsGroups ->
            case Array.get bodyId1 solverBodies of
                Just body1 ->
                    case Array.get bodyId2 solverBodies of
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
                                (solverBodies
                                    |> Array.set bodyId1 groupContext.body1
                                    |> Array.set bodyId2 groupContext.body2
                                )

                        _ ->
                            -- Should never happen
                            step number
                                deltalambdaTot
                                equationsGroups
                                remainingEquationsGroups
                                solverBodies

                _ ->
                    -- Should never happen
                    step number
                        deltalambdaTot
                        equationsGroups
                        remainingEquationsGroups
                        solverBodies


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
            in
            solveEquationsGroup
                { body = body1.body
                , vX = body1.vX
                , vY = body1.vY
                , vZ = body1.vZ
                , wX = body1.wX
                , wY = body1.wY
                , wZ = body1.wZ
                , vX_ = body1.vX_ - k1 * vB.x
                , vY_ = body1.vY_ - k1 * vB.y
                , vZ_ = body1.vZ_ - k1 * vB.z
                , wX_ = body1.wX_ + (invI1.m11 * wA.x + invI1.m12 * wA.y + invI1.m13 * wA.z) * deltalambda
                , wY_ = body1.wY_ + (invI1.m21 * wA.x + invI1.m22 * wA.y + invI1.m23 * wA.z) * deltalambda
                , wZ_ = body1.wZ_ + (invI1.m31 * wA.x + invI1.m32 * wA.y + invI1.m33 * wA.z) * deltalambda
                }
                { body = body2.body
                , vX = body2.vX
                , vY = body2.vY
                , vZ = body2.vZ
                , wX = body2.wX
                , wY = body2.wY
                , wZ = body2.wZ
                , vX_ = body2.vX_ + k2 * vB.x
                , vY_ = body2.vY_ + k2 * vB.y
                , vZ_ = body2.vZ_ + k2 * vB.z
                , wX_ = body2.wX_ + (invI2.m11 * wB.x + invI2.m12 * wB.y + invI2.m13 * wB.z) * deltalambda
                , wY_ = body2.wY_ + (invI2.m21 * wB.x + invI2.m22 * wB.y + invI2.m23 * wB.z) * deltalambda
                , wZ_ = body2.wZ_ + (invI2.m31 * wB.x + invI2.m32 * wB.y + invI2.m33 * wB.z) * deltalambda
                }
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
                        -- only dynamic bodies are updated
                        if solverBody.body.mass > 0 then
                            SolverBody.toBody ctx solverBody

                        else
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
