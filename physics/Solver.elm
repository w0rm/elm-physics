module Physics.Solver exposing (..)

import Physics.World as World exposing (World)
import Physics.Body as Body exposing (Body, BodyId)
import Physics.SolverBody as SolverBody exposing (SolverBody)
import Physics.NarrowPhase as NarrowPhase exposing (NarrowPhaseResult)
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.SolverEquation as SolverEquation exposing (SolverEquation)
import Time exposing (Time)
import Dict exposing (Dict)
import Math.Vector3 as Vec3


solve : Time -> NarrowPhaseResult -> World -> World
solve dt { contactEquations } world =
    if contactEquations == [] then
        world
    else
        (solveContext dt contactEquations world)
            |> solveHelp iterations
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
                    { body
                        | velocity =
                            Dict.get bodyId bodies
                                |> Maybe.map (\{ vlambda } -> Vec3.add body.velocity vlambda)
                                |> Maybe.withDefault body.velocity
                        , angularVelocity =
                            Dict.get bodyId bodies
                                |> Maybe.map (\{ wlambda } -> Vec3.add body.angularVelocity wlambda)
                                |> Maybe.withDefault body.angularVelocity
                    }
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

                minForce =
                    0

                maxForce =
                    1000000

                deltalambdaPrev =
                    equation.solverInvCs * (equation.solverBs - gWlambda - equation.spookEps * equation.solverLambda)

                deltalambda =
                    if equation.solverLambda + deltalambdaPrev < minForce then
                        minForce - equation.solverLambda
                    else if equation.solverLambda + deltalambdaPrev > maxForce then
                        maxForce - equation.solverLambda
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
    { dt : Time
    , bodies : Dict BodyId SolverBody
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
        { dt = dt
        , bodies = solverBodies
        , equations =
            List.map
                (SolverEquation.fromContactEquation dt solverBodies)
                (List.reverse contactEquations)
        , deltalambdaTot = 10000000 -- large number initially
        , world = world
        }


iterations : Int
iterations =
    20
