module Internal.Solver exposing (solve)

import Dict exposing (Dict)
import Internal.Body as Body exposing (Body, BodyId)
import Internal.Const as Const
import Internal.ContactEquation as ContactEquation exposing (ContactEquation)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.SolverEquation as SolverEquation exposing (SolverEquation)
import Internal.Vector3 as Vec3
import Internal.World as World exposing (World)


maxIterations : Int
maxIterations =
    20


solve : Float -> List (ContactEquation data) -> World data -> World data
solve dt contactEquations world =
    if contactEquations == [] then
        world

    else
        world
            |> solveContext dt contactEquations
            |> solveHelp maxIterations
            |> updateVelocities


solveHelp : Int -> SolveContext data -> SolveContext data
solveHelp number context =
    if number == 0 || context.deltalambdaTot < Const.precision then
        context

    else
        solveHelp (number - 1) (solveStep context)


updateVelocities : SolveContext data -> World data
updateVelocities { bodies, world } =
    { world
        | bodies =
            List.map
                (\body ->
                    Dict.get body.id bodies
                        |> Maybe.map
                            (\{ vlambda, wlambda } ->
                                { body
                                    | velocity = Vec3.add body.velocity vlambda
                                    , angularVelocity = Vec3.add body.angularVelocity wlambda
                                }
                            )
                        |> Maybe.withDefault body
                )
                world.bodies
    }


solveStep : SolveContext data -> SolveContext data
solveStep context =
    List.foldl
        (\equation newContext ->
            Maybe.withDefault newContext <|
                Maybe.map2
                    (\bi bj ->
                        let
                            gWlambda =
                                SolverEquation.computeGWlambda bi bj equation

                            deltalambdaPrev =
                                equation.solverInvCs * (equation.solverBs - gWlambda - equation.spookEps * equation.solverLambda)

                            deltalambda =
                                if equation.solverLambda + deltalambdaPrev < equation.minForce then
                                    equation.minForce - equation.solverLambda

                                else if equation.solverLambda + deltalambdaPrev > equation.maxForce then
                                    equation.maxForce - equation.solverLambda

                                else
                                    deltalambdaPrev

                            newEquation =
                                { kind = equation.kind
                                , bodyId1 = equation.bodyId1
                                , bodyId2 = equation.bodyId2
                                , minForce = equation.minForce
                                , maxForce = equation.maxForce
                                , solverLambda = equation.solverLambda + deltalambda
                                , solverBs = equation.solverBs
                                , solverInvCs = equation.solverInvCs
                                , spookA = equation.spookA
                                , spookB = equation.spookB
                                , spookEps = equation.spookEps
                                , jacobianElementA = equation.jacobianElementA
                                , jacobianElementB = equation.jacobianElementB
                                }
                        in
                        { world = newContext.world
                        , deltalambdaTot = newContext.deltalambdaTot + abs deltalambda
                        , equations = newEquation :: newContext.equations
                        , bodies =
                            newContext.bodies
                                |> Dict.insert equation.bodyId1 (SolverBody.addToWlambda deltalambda newEquation.jacobianElementA bi)
                                |> Dict.insert equation.bodyId2 (SolverBody.addToWlambda deltalambda newEquation.jacobianElementB bj)
                        }
                    )
                    (Dict.get equation.bodyId1 newContext.bodies)
                    (Dict.get equation.bodyId2 newContext.bodies)
        )
        { context | equations = [], deltalambdaTot = 0 }
        context.equations


type alias SolveContext data =
    { bodies : Dict BodyId (SolverBody data)
    , equations : List (SolverEquation data)
    , deltalambdaTot : Float
    , world : World data
    }


solveContext : Float -> List (ContactEquation data) -> World data -> SolveContext data
solveContext dt contactEquations world =
    let
        solverBodies =
            world.bodies
                |> List.map (\body -> ( body.id, SolverBody.fromBody body ))
                |> Dict.fromList
    in
    { bodies = solverBodies
    , equations =
        List.foldl
            (\contactEquation solverEquations ->
                Maybe.withDefault solverEquations <|
                    Maybe.map2
                        (\bi bj ->
                            solverEquations
                                |> SolverEquation.addContactEquation dt world.gravity bi bj contactEquation
                                |> SolverEquation.addFrictionEquations dt world.gravity bi bj contactEquation
                        )
                        (Dict.get contactEquation.body1.id solverBodies)
                        (Dict.get contactEquation.body2.id solverBodies)
            )
            []
            contactEquations
    , deltalambdaTot = Const.maxNumber -- large number initially
    , world = world
    }
