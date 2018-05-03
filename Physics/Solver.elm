module Physics.Solver exposing (solve)

import Physics.World as World exposing (World)
import Physics.Body as Body exposing (Body, BodyId)
import Physics.SolverBody as SolverBody exposing (SolverBody)
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.SolverEquation as SolverEquation exposing (SolverEquation)
import Time exposing (Time)
import Dict exposing (Dict)
import Math.Vector3 as Vec3


maxIterations : Int
maxIterations =
    20


solve : Time -> List ContactEquation -> World -> World
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
    if number == 0 || context.deltalambdaTot < 1.0e-7 then
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


fromJust : Maybe a -> a
fromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            Debug.crash ""


solveStep : SolveContext -> SolveContext
solveStep context =
    List.foldl
        (\equation acc ->
            let
                bi =
                    Dict.get equation.bodyId1 acc.bodies
                        |> fromJust

                bj =
                    Dict.get equation.bodyId2 acc.bodies
                        |> fromJust

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
                    { equation | solverLambda = equation.solverLambda + deltalambda }
            in
                { acc
                    | deltalambdaTot = acc.deltalambdaTot + abs deltalambda
                    , equations = newEquation :: acc.equations
                    , bodies =
                        acc.bodies
                            |> Dict.insert equation.bodyId1 (SolverBody.addToWlambda deltalambda newEquation.jacobianElementA bi)
                            |> Dict.insert equation.bodyId2 (SolverBody.addToWlambda deltalambda newEquation.jacobianElementB bj)
                }
        )
        { context | equations = [], deltalambdaTot = 0 }
        context.equations


type alias SolveContext =
    { bodies : Dict BodyId SolverBody
    , equations : List SolverEquation
    , deltalambdaTot : Float
    , world : World
    }


solveContext : Time -> List ContactEquation -> World -> SolveContext
solveContext dt contactEquations world =
    let
        solverBodies =
            Dict.map (\_ -> SolverBody.fromBody) world.bodies
    in
        { bodies = solverBodies
        , equations =
            List.foldl
                (\contactEquation ->
                    let
                        bi =
                            Dict.get contactEquation.bodyId1 solverBodies
                                |> fromJust

                        bj =
                            Dict.get contactEquation.bodyId2 solverBodies
                                |> fromJust
                    in
                        SolverEquation.addContactEquation dt world.gravity bi bj contactEquation
                            >> SolverEquation.addFrictionEquations dt world.gravity bi bj contactEquation
                )
                []
                contactEquations
        , deltalambdaTot = 10000000 -- large number initially
        , world = world
        }
