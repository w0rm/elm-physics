module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body as Body exposing (Body, BodyId)
import Internal.Const as Const
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.SolverEquation as SolverEquation exposing (ContactEquation, SolverEquation, addSolverEquations)
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
                    Array.get body.id bodies
                        |> Maybe.andThen identity
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
                                |> Array.set equation.bodyId1 (Just (SolverBody.addToWlambda deltalambda newEquation.jacobianElementA bi))
                                |> Array.set equation.bodyId2 (Just (SolverBody.addToWlambda deltalambda newEquation.jacobianElementB bj))
                        }
                    )
                    (Array.get equation.bodyId1 newContext.bodies |> Maybe.andThen identity)
                    (Array.get equation.bodyId2 newContext.bodies |> Maybe.andThen identity)
        )
        { context | equations = [], deltalambdaTot = 0 }
        context.equations


type alias SolveContext data =
    { bodies : Array (Maybe (SolverBody data))
    , equations : List (SolverEquation data)
    , deltalambdaTot : Float
    , world : World data
    }


solveContext : Float -> List (ContactEquation data) -> World data -> SolveContext data
solveContext dt contactEquations world =
    { bodies =
        List.foldl
            (\body -> Array.set body.id (Just (SolverBody.fromBody body)))
            (Array.initialize world.nextBodyId (always Nothing))
            world.bodies
    , equations =
        List.foldl
            (addSolverEquations dt world.gravity)
            []
            contactEquations
    , deltalambdaTot = Const.maxNumber -- large number initially
    , world = world
    }
