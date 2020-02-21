module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.Equation as Equation exposing (EquationsGroup, SolverEquation)
import Internal.SolverBody as SolverBody exposing (SolverBody)
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
        -- make equations from contacts
        contactEquationsGroups =
            List.foldl
                (\contactGroup groups ->
                    Equation.contactEquationsGroup
                        dt
                        world.gravity
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
                                        dt
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
    updateBodies dt solvedBodies world


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> Array (SolverBody data) -> Array (SolverBody data)
step number deltalambdaTot equationsGroups currentEquationsGroups solverBodies =
    case currentEquationsGroups of
        [] ->
            if number == 0 || deltalambdaTot - Const.precision < 0 then
                -- the max number of steps elapsed or tolerance reached
                solverBodies

            else
                -- requeue equationsGropus for the next step
                step (number - 1) 0 [] (List.reverse equationsGroups) solverBodies

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
            , equations = List.reverse equations
            , deltalambdaTot = deltalambdaTot
            }

        { solverLambda, equation } :: remainingEquations ->
            let
                gWlambda =
                    Equation.computeGWlambda body1 body2 equation

                deltalambdaPrev =
                    equation.solverInvCs * (equation.solverBs - gWlambda - equation.spookEps * solverLambda)

                deltalambda =
                    if solverLambda + deltalambdaPrev - equation.minForce < 0 then
                        equation.minForce - solverLambda

                    else if solverLambda + deltalambdaPrev - equation.maxForce > 0 then
                        equation.maxForce - solverLambda

                    else
                        deltalambdaPrev
            in
            solveEquationsGroup
                (SolverBody.addToWlambda deltalambda equation.jacobianElementA body1)
                (SolverBody.addToWlambda deltalambda equation.jacobianElementB body2)
                ({ solverLambda = solverLambda + deltalambda
                 , equation = equation
                 }
                    :: equations
                )
                (deltalambdaTot + abs deltalambda)
                remainingEquations


updateBodies : Float -> Array (SolverBody data) -> World data -> World data
updateBodies dt bodies world =
    let
        simulatedBodies =
            Array.map
                (\{ body, vlambda, wlambda } ->
                    -- id == -1 is to skip the filling body to avoid (Array (Maybe (SolverBody data)))
                    if body.id + 1 > 0 then
                        -- only dynamic bodies are updated
                        if body.mass > 0 then
                            Body.update
                                dt
                                vlambda
                                wlambda
                                body

                        else
                            body

                    else
                        body
                )
                bodies
    in
    { world
        | bodies = Array.toList simulatedBodies
        , simulatedBodies = simulatedBodies
    }
