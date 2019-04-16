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
            List.foldl
                (\maybeSolverBody result ->
                    case maybeSolverBody of
                        Just { body, vlambda, wlambda } ->
                            { id = body.id
                            , data = body.data
                            , material = body.material
                            , position = body.position
                            , velocity = Vec3.add body.velocity vlambda
                            , angularVelocity = Vec3.add body.angularVelocity wlambda
                            , orientation = body.orientation
                            , mass = body.mass
                            , shapes = body.shapes
                            , force = body.force
                            , torque = body.torque
                            , boundingSphereRadius = body.boundingSphereRadius
                            , invMass = body.invMass
                            , inertia = body.inertia
                            , invInertia = body.invInertia
                            , invInertiaWorld = body.invInertiaWorld
                            }
                                :: result

                        Nothing ->
                            result
                )
                []
                (Array.toList bodies)
    }


solveStep : SolveContext data -> SolveContext data
solveStep context =
    List.foldl
        (\( equationSolverLambda, equation ) newContext ->
            case Array.get equation.bodyId1 newContext.bodies of
                Just (Just bi) ->
                    case Array.get equation.bodyId2 newContext.bodies of
                        Just (Just bj) ->
                            let
                                gWlambda =
                                    SolverEquation.computeGWlambda bi bj equation

                                deltalambdaPrev =
                                    equation.solverInvCs * (equation.solverBs - gWlambda - equation.spookEps * equationSolverLambda)

                                deltalambda =
                                    if equationSolverLambda + deltalambdaPrev < equation.minForce then
                                        equation.minForce - equationSolverLambda

                                    else if equationSolverLambda + deltalambdaPrev > equation.maxForce then
                                        equation.maxForce - equationSolverLambda

                                    else
                                        deltalambdaPrev
                            in
                            { world = newContext.world
                            , deltalambdaTot = newContext.deltalambdaTot + abs deltalambda
                            , equations = ( equationSolverLambda, equation ) :: newContext.equations
                            , bodies =
                                newContext.bodies
                                    |> Array.set equation.bodyId1 (Just (SolverBody.addToWlambda deltalambda equation.jacobianElementA bi))
                                    |> Array.set equation.bodyId2 (Just (SolverBody.addToWlambda deltalambda equation.jacobianElementB bj))
                            }

                        _ ->
                            newContext

                _ ->
                    newContext
        )
        { context | equations = [], deltalambdaTot = 0 }
        context.equations


type alias SolveContext data =
    { bodies : Array (Maybe (SolverBody data))
    , equations : List ( Float, SolverEquation data )
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
            |> List.map (Tuple.pair 0)
    , deltalambdaTot = Const.maxNumber -- large number initially
    , world = world
    }
