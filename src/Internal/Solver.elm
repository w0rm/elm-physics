module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body as Body exposing (Body, BodyId)
import Internal.Const as Const
import Internal.NarrowPhase exposing (Contact, ContactGroup)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.SolverEquation as SolverEquation exposing (ContactEquation, SolverEquation, addSolverEquations)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.World as World exposing (World)


maxIterations : Int
maxIterations =
    20


solve : Float -> List (ContactGroup data) -> World data -> World data
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


type alias GroupContext data =
    { body1 : SolverBody data
    , body2 : SolverBody data
    , solverEquations : List ( Float, SolverEquation )
    , deltalambdaTot : Float
    }


solveEquationsGroup : SolverBody data -> SolverBody data -> List ( Float, SolverEquation ) -> Float -> List ( Float, SolverEquation ) -> GroupContext data
solveEquationsGroup body1 body2 solverEquations deltalambdaTot currentSolverEquations =
    case currentSolverEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , solverEquations = solverEquations
            , deltalambdaTot = deltalambdaTot
            }

        ( equationSolverLambda, solverEquation ) :: remainingSolverEquations ->
            let
                gWlambda =
                    SolverEquation.computeGWlambda body1 body2 solverEquation

                deltalambdaPrev =
                    solverEquation.solverInvCs * (solverEquation.solverBs - gWlambda - solverEquation.spookEps * equationSolverLambda)

                deltalambda =
                    if equationSolverLambda + deltalambdaPrev < solverEquation.minForce then
                        solverEquation.minForce - equationSolverLambda

                    else if equationSolverLambda + deltalambdaPrev > solverEquation.maxForce then
                        solverEquation.maxForce - equationSolverLambda

                    else
                        deltalambdaPrev
            in
            solveEquationsGroup
                (SolverBody.addToWlambda deltalambda solverEquation.jacobianElementA body1)
                (SolverBody.addToWlambda deltalambda solverEquation.jacobianElementB body2)
                (( equationSolverLambda + deltalambda, solverEquation ) :: solverEquations)
                (deltalambdaTot + abs deltalambda)
                remainingSolverEquations


solveStep : SolveContext data -> SolveContext data
solveStep context =
    List.foldl
        (\{ bodyId1, bodyId2, solverEquations } newContext ->
            case Array.get bodyId1 newContext.bodies of
                Just (Just body1) ->
                    case Array.get bodyId2 newContext.bodies of
                        Just (Just body2) ->
                            let
                                groupContext =
                                    solveEquationsGroup
                                        body1
                                        body2
                                        []
                                        newContext.deltalambdaTot
                                        solverEquations
                            in
                            { world = newContext.world
                            , deltalambdaTot = groupContext.deltalambdaTot
                            , solverEquationsGroups =
                                { bodyId1 = bodyId1
                                , bodyId2 = bodyId2
                                , solverEquations = groupContext.solverEquations
                                }
                                    :: newContext.solverEquationsGroups
                            , bodies =
                                newContext.bodies
                                    |> Array.set bodyId1 (Just groupContext.body1)
                                    |> Array.set bodyId2 (Just groupContext.body2)
                            }

                        _ ->
                            newContext

                _ ->
                    newContext
        )
        { context | solverEquationsGroups = [], deltalambdaTot = 0 }
        context.solverEquationsGroups


type alias SolveContext data =
    { bodies : Array (Maybe (SolverBody data))
    , solverEquationsGroups : List SolverEquationsGroup
    , deltalambdaTot : Float
    , world : World data
    }


solveContext : Float -> List (ContactGroup data) -> World data -> SolveContext data
solveContext dt contactGroups world =
    { bodies =
        List.foldl
            (\body -> Array.set body.id (Just (SolverBody.fromBody body)))
            (Array.initialize world.nextBodyId (always Nothing))
            world.bodies
    , solverEquationsGroups = List.map (solverEquationsGroup dt world.gravity) contactGroups
    , deltalambdaTot = Const.maxNumber -- large number initially
    , world = world
    }


type alias SolverEquationsGroup =
    { bodyId1 : BodyId
    , bodyId2 : BodyId
    , solverEquations : List ( Float, SolverEquation )
    }


solverEquationsGroup : Float -> Vec3 -> ContactGroup data -> SolverEquationsGroup
solverEquationsGroup dt gravity { body1, body2, contacts } =
    { bodyId1 = body1.id
    , bodyId2 = body2.id
    , solverEquations =
        contacts
            |> List.foldl (addSolverEquations dt gravity body1 body2) []
            |> List.map (Tuple.pair 0)
    }
