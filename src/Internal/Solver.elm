module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.Equation as Equation exposing (Equation, EquationsGroup)
import Internal.NarrowPhase exposing (Contact, ContactGroup)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.World as World exposing (World)


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
                -- in the array and avoid an extra boxing (Array (Maybe (SolverBody data)))
                allBodies =
                    { firstBody | id = -1 }
                        |> SolverBody.fromBody
                        |> Array.repeat nextBodyId
            in
            List.foldl
                (\body -> Array.set body.id (SolverBody.fromBody body))
                allBodies
                bodies


solve : Float -> List (ContactGroup data) -> World data -> World data
solve dt contactGroups world =
    let
        solverBodies =
            makeSolverBodies world.nextBodyId world.bodies

        equationsGroups =
            List.foldl
                (\contactGroup -> (::) (Equation.equationsGroup dt world.gravity contactGroup))
                []
                contactGroups

        solvedBodies =
            step maxIterations 0 [] equationsGroups solverBodies
    in
    updateVelocities solvedBodies world


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> Array (SolverBody data) -> Array (SolverBody data)
step number deltalambdaTot equationsGroups currentEquationsGroups solverBodies =
    case currentEquationsGroups of
        [] ->
            if number == 0 || deltalambdaTot - Const.precision < 0 then
                solverBodies

            else
                step (number - 1) 0 [] equationsGroups solverBodies

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
    , equations : List ( Float, Equation )
    , deltalambdaTot : Float
    }


solveEquationsGroup : SolverBody data -> SolverBody data -> List ( Float, Equation ) -> Float -> List ( Float, Equation ) -> GroupSolveResult data
solveEquationsGroup body1 body2 equations deltalambdaTot currentEquations =
    case currentEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , equations = equations
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
                (( equationSolverLambda + deltalambda, solverEquation ) :: equations)
                (deltalambdaTot + abs deltalambda)
                remainingEquations


updateVelocities : Array (SolverBody data) -> World data -> World data
updateVelocities bodies world =
    { world
        | bodies =
            List.foldl
                (\solverBody result ->
                    -- id == -1 is to skip the filling body to avoid (Array (Maybe (SolverBody data)))
                    if solverBody.body.id + 1 > 0 then
                        let
                            { body, vlambda, wlambda } =
                                solverBody
                        in
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

                    else
                        result
                )
                []
                (Array.toList bodies)
    }
