module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body as Body exposing (Body, BodyId)
import Internal.Const as Const
import Internal.Equation as Equation exposing (Equation, addEquations)
import Internal.NarrowPhase exposing (Contact, ContactGroup)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.World as World exposing (World)


maxIterations : Int
maxIterations =
    20


solve : Float -> List (ContactGroup data) -> World data -> World data
solve dt contactGroups world =
    let
        solverBodies =
            List.foldl
                (\body -> Array.set body.id (Just (SolverBody.fromBody body)))
                (Array.initialize world.nextBodyId (always Nothing))
                world.bodies

        equationsGroups =
            List.map
                (solverEquationsGroup dt world.gravity)
                contactGroups

        solvedBodies =
            step maxIterations 0 [] equationsGroups solverBodies
    in
    updateVelocities solvedBodies world


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> Array (Maybe (SolverBody data)) -> Array (Maybe (SolverBody data))
step number deltalambdaTot equationsGroups currentEquationsGroups solverBodies =
    case currentEquationsGroups of
        [] ->
            if number == 0 || deltalambdaTot - Const.precision < 0 then
                solverBodies

            else
                step (number - 1) 0 [] equationsGroups solverBodies

        { bodyId1, bodyId2, solverEquations } :: remainingEquationsGroups ->
            case Array.get bodyId1 solverBodies of
                Just (Just body1) ->
                    case Array.get bodyId2 solverBodies of
                        Just (Just body2) ->
                            let
                                groupContext =
                                    solveEquationsGroup
                                        body1
                                        body2
                                        []
                                        deltalambdaTot
                                        solverEquations
                            in
                            step number
                                groupContext.deltalambdaTot
                                ({ bodyId1 = bodyId1
                                 , bodyId2 = bodyId2
                                 , solverEquations = groupContext.solverEquations
                                 }
                                    :: equationsGroups
                                )
                                remainingEquationsGroups
                                (solverBodies
                                    |> Array.set bodyId1 (Just groupContext.body1)
                                    |> Array.set bodyId2 (Just groupContext.body2)
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


updateVelocities : Array (Maybe (SolverBody data)) -> World data -> World data
updateVelocities bodies world =
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
    , solverEquations : List ( Float, Equation )
    , deltalambdaTot : Float
    }


solveEquationsGroup : SolverBody data -> SolverBody data -> List ( Float, Equation ) -> Float -> List ( Float, Equation ) -> GroupContext data
solveEquationsGroup body1 body2 solverEquations deltalambdaTot currentEquations =
    case currentEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , solverEquations = solverEquations
            , deltalambdaTot = deltalambdaTot
            }

        ( equationSolverLambda, solverEquation ) :: remainingEquations ->
            let
                gWlambda =
                    Equation.computeGWlambda body1 body2 solverEquation

                deltalambdaPrev =
                    solverEquation.solverInvCs * (solverEquation.solverBs - gWlambda - solverEquation.spookEps * equationSolverLambda)

                deltalambda =
                    if equationSolverLambda + deltalambdaPrev - solverEquation.minForce < 0 then
                        solverEquation.minForce - equationSolverLambda

                    else if equationSolverLambda + deltalambdaPrev - solverEquation.maxForce > 0 then
                        solverEquation.maxForce - equationSolverLambda

                    else
                        deltalambdaPrev
            in
            solveEquationsGroup
                (SolverBody.addToWlambda deltalambda solverEquation.jacobianElementA body1)
                (SolverBody.addToWlambda deltalambda solverEquation.jacobianElementB body2)
                (( equationSolverLambda + deltalambda, solverEquation ) :: solverEquations)
                (deltalambdaTot + abs deltalambda)
                remainingEquations


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
                            , equationsGroups =
                                { bodyId1 = bodyId1
                                , bodyId2 = bodyId2
                                , solverEquations = groupContext.solverEquations
                                }
                                    :: newContext.equationsGroups
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
        { context | equationsGroups = [], deltalambdaTot = 0 }
        context.equationsGroups


type alias SolveContext data =
    { bodies : Array (Maybe (SolverBody data))
    , equationsGroups : List EquationsGroup
    , deltalambdaTot : Float
    , world : World data
    }


type alias EquationsGroup =
    { bodyId1 : BodyId
    , bodyId2 : BodyId
    , solverEquations : List ( Float, Equation )
    }


solverEquationsGroup : Float -> Vec3 -> ContactGroup data -> EquationsGroup
solverEquationsGroup dt gravity { body1, body2, contacts } =
    { bodyId1 = body1.id
    , bodyId2 = body2.id
    , solverEquations =
        contacts
            |> List.foldl (addEquations dt gravity body1 body2) []
            |> List.map (Tuple.pair 0)
    }
