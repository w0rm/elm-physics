module Physics.Solver exposing (solve)

import AltMath.Vector3 as Vec3
import Physics.Body as Body exposing (Body, BodyId)
import Physics.Const as Const
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.SolverBody as SolverBody exposing (SolverBody)
import Physics.SolverEquation as SolverEquation exposing (SolverEquation)
import Physics.World as World exposing (World)
import Dict exposing (Dict)


maxIterations : Int
maxIterations =
    20


solve : Float -> List ContactEquation -> World -> World
solve dt contactEquations world =
    if contactEquations == [] then
        world

    else
        world
            |> solveContext dt contactEquations
            |> solveHelp maxIterations
            |> updateVelocities


solveHelp : Int -> SolveContext -> SolveContext
solveHelp number context =
    if number == 0 || context.deltalambdaTot < Const.precision then
        context

    else
        solveHelp (number - 1) (solveStep context)


updateVelocities : SolveContext -> World
updateVelocities { bodies, world } =
    { world
        | bodies =
            Dict.map
                (\bodyId body ->
                    Dict.get bodyId bodies
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


solveStep : SolveContext -> SolveContext
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


type alias SolveContext =
    { bodies : Dict BodyId SolverBody
    , equations : List SolverEquation
    , deltalambdaTot : Float
    , world : World
    }


solveContext : Float -> List ContactEquation -> World -> SolveContext
solveContext dt contactEquations world =
    let
        solverBodies =
            Dict.map (\_ -> SolverBody.fromBody) world.bodies
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
                        (Dict.get contactEquation.bodyId1 solverBodies)
                        (Dict.get contactEquation.bodyId2 solverBodies)
            )
            []
            contactEquations
    , deltalambdaTot = Const.maxNumber -- large number initially
    , world = world
    }
