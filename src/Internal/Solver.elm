module Internal.Solver exposing (solve)

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.Contact exposing (PairGroup)
import Internal.Equation as Equation exposing (EquationsGroup, SolverEquation)
import Internal.Matrix3 as Mat3
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Fills unused slots in the solver body array. id = -1 is impossible for real
bodies, so any array lookup that returns this sentinel can be ignored.
-}
sentinel : id -> SolverBody id
sentinel extId =
    { body =
        { id = -1
        , kind = Body.Static
        , transform3d = Transform3d.atOrigin
        , centerOfMassTransform3d = Transform3d.atOrigin
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , mass = 0
        , volume = 0
        , shapesWithMaterials = []
        , worldShapesWithMaterials = []
        , force = Vec3.zero
        , torque = Vec3.zero
        , boundingSphereRadius = 0
        , linearDamping = 0
        , angularDamping = 0
        , invMass = 0
        , invInertia = Vec3.zero
        , invInertiaWorld = Mat3.zero
        , linearLock = Vec3.one
        , angularLock = Vec3.one
        }
    , extId = extId
    , vX = 0
    , vY = 0
    , vZ = 0
    , wX = 0
    , wY = 0
    , wZ = 0
    }


{-| Sparse array indexed by body.id (IDs may be non-consecutive when
bodies are added mid-simulation). Unused slots are filled with the sentinel.
-}
makeSolverBodies : Int -> List ( id, Body ) -> Array (SolverBody id)
makeSolverBodies maxId bodiesWithIds =
    case bodiesWithIds of
        [] ->
            Array.empty

        ( firstExtId, _ ) :: _ ->
            List.foldl
                (\( extId, body ) arr -> Array.set body.id (SolverBody.fromBody extId body) arr)
                (Array.repeat (maxId + 1) (sentinel firstExtId))
                bodiesWithIds


{-| Apply the impulse corresponding to a seeded lambda to both solver bodies.
This pre-loads the body delta-v so the solver starts from a warm state.
-}
applyGroupWarmStart : SolverBody id -> SolverBody id -> List SolverEquation -> ( SolverBody id, SolverBody id )
applyGroupWarmStart body1 body2 equations =
    case equations of
        [] ->
            ( body1, body2 )

        { solverLambda, equation } :: rest ->
            if solverLambda == 0 then
                applyGroupWarmStart body1 body2 rest

            else
                let
                    { vB, wA, wB } =
                        equation

                    newBody1 =
                        case body1.body.kind of
                            Body.Dynamic ->
                                let
                                    invI1 =
                                        body1.body.invInertiaWorld

                                    k1 =
                                        solverLambda * body1.body.invMass
                                in
                                { body = body1.body
                                , extId = body1.extId
                                , vX = body1.vX - k1 * vB.x
                                , vY = body1.vY - k1 * vB.y
                                , vZ = body1.vZ - k1 * vB.z
                                , wX = body1.wX + (invI1.m11 * wA.x + invI1.m12 * wA.y + invI1.m13 * wA.z) * solverLambda
                                , wY = body1.wY + (invI1.m21 * wA.x + invI1.m22 * wA.y + invI1.m23 * wA.z) * solverLambda
                                , wZ = body1.wZ + (invI1.m31 * wA.x + invI1.m32 * wA.y + invI1.m33 * wA.z) * solverLambda
                                }

                            _ ->
                                body1

                    newBody2 =
                        case body2.body.kind of
                            Body.Dynamic ->
                                let
                                    invI2 =
                                        body2.body.invInertiaWorld

                                    k2 =
                                        solverLambda * body2.body.invMass
                                in
                                { body = body2.body
                                , extId = body2.extId
                                , vX = body2.vX + k2 * vB.x
                                , vY = body2.vY + k2 * vB.y
                                , vZ = body2.vZ + k2 * vB.z
                                , wX = body2.wX + (invI2.m11 * wB.x + invI2.m12 * wB.y + invI2.m13 * wB.z) * solverLambda
                                , wY = body2.wY + (invI2.m21 * wB.x + invI2.m22 * wB.y + invI2.m23 * wB.z) * solverLambda
                                , wZ = body2.wZ + (invI2.m31 * wB.x + invI2.m32 * wB.y + invI2.m33 * wB.z) * solverLambda
                                }

                            _ ->
                                body2
                in
                applyGroupWarmStart newBody1 newBody2 rest


applyWarmStart : SolverBody id -> Array (SolverBody id) -> List EquationsGroup -> Array (SolverBody id)
applyWarmStart prevBody1 solverBodies equationsGroups =
    case equationsGroups of
        [] ->
            Array.set prevBody1.body.id prevBody1 solverBodies

        { bodyId1, bodyId2, equations } :: rest ->
            let
                body1 =
                    if prevBody1.body.id - bodyId1 == 0 then
                        prevBody1

                    else
                        case Array.get bodyId1 solverBodies of
                            Just b ->
                                b

                            Nothing ->
                                prevBody1

                newSolverBodies =
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.kind /= Body.Dynamic then
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies

                body2 =
                    case Array.get bodyId2 newSolverBodies of
                        Just b ->
                            b

                        Nothing ->
                            prevBody1

                ( newBody1, newBody2 ) =
                    applyGroupWarmStart body1 body2 equations
            in
            applyWarmStart
                newBody1
                (if newBody2.body.kind == Body.Dynamic then
                    Array.set bodyId2 newBody2 newSolverBodies

                 else
                    newSolverBodies
                )
                rest


solve : Float -> Vec3 -> Int -> List PairGroup -> Int -> List ( id, Body ) -> Dict String Float -> ( Array (SolverBody id), Dict String Float, Int )
solve dt gravity iterations pairGroups maxId bodiesWithIds lambdas =
    case bodiesWithIds of
        [] ->
            ( Array.empty, Dict.empty, 0 )

        ( firstExtId, _ ) :: _ ->
            let
                ctx =
                    { dt = dt
                    , gravity = gravity
                    , gravityLength = Vec3.length gravity
                    , lambdas = lambdas
                    }

                fillingBody =
                    sentinel firstExtId

                solverBodies =
                    makeSolverBodies maxId bodiesWithIds

                equationsGroups =
                    List.foldl
                        (\pairGroup groups ->
                            Equation.pairEquationsGroup ctx pairGroup :: groups
                        )
                        []
                        pairGroups

                -- warm start: apply cached lambdas as impulses to body delta-v
                warmStartedBodies =
                    case equationsGroups of
                        [] ->
                            solverBodies

                        { bodyId1 } :: _ ->
                            case Array.get bodyId1 solverBodies of
                                Just firstBody ->
                                    applyWarmStart firstBody solverBodies equationsGroups

                                Nothing ->
                                    solverBodies

                bodiesList =
                    List.filterMap
                        (\( _, b ) -> Array.get b.id warmStartedBodies)
                        bodiesWithIds

                ( finalBodiesList, finalEquationsGroups, remainingIterations ) =
                    runIterations iterations bodiesList equationsGroups

                finalSolverBodies =
                    listToArray maxId firstExtId finalBodiesList

                iterationsUsed =
                    max 1 (iterations - remainingIterations)

                finalLambdas =
                    List.foldl
                        (\{ equations } acc ->
                            List.foldl
                                (\{ equation, solverLambda } lambdaAcc ->
                                    if equation.id == "" then
                                        lambdaAcc

                                    else
                                        Dict.insert equation.id solverLambda lambdaAcc
                                )
                                acc
                                equations
                        )
                        Dict.empty
                        finalEquationsGroups
            in
            ( finalSolverBodies, finalLambdas, iterationsUsed )


step : Int -> Float -> List EquationsGroup -> List EquationsGroup -> SolverBody id -> Array (SolverBody id) -> ( Array (SolverBody id), List EquationsGroup, Int )
step remainingIterations deltalambdaTot equationsGroups currentEquationsGroups prevBody1 solverBodies =
    case currentEquationsGroups of
        [] ->
            if remainingIterations == 0 then
                -- the max number of iterations elapsed
                ( Array.set prevBody1.body.id prevBody1 solverBodies, equationsGroups, 0 )

            else if deltalambdaTot - Const.precision < 0 then
                -- tolerance reached
                ( Array.set prevBody1.body.id prevBody1 solverBodies, equationsGroups, remainingIterations - 1 )

            else
                -- requeue equationsGroups for the next step
                step (remainingIterations - 1) 0 [] (List.reverse equationsGroups) prevBody1 solverBodies

        { bodyId1, bodyId2, equations } :: remainingEquationsGroups ->
            let
                body1 =
                    if prevBody1.body.id - bodyId1 == 0 then
                        -- if the next equations group has the same body
                        -- then no need to get it from the array
                        prevBody1

                    else
                        case Array.get bodyId1 solverBodies of
                            Just nextBody ->
                                nextBody

                            Nothing ->
                                -- this shouldn’t happen, we just
                                -- don’t want to allocate a Just
                                prevBody1

                newSolverBodies =
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.kind /= Body.Dynamic then
                        -- if the next equations group has the same body,
                        -- then no need to set it to the array
                        -- also no need to update non-dynamic bodies (static, kinematic)
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies

                body2 =
                    case Array.get bodyId2 newSolverBodies of
                        Just nextBody ->
                            nextBody

                        Nothing ->
                            -- this shouldn’t happen, we just
                            -- don’t want to allocate a Just
                            prevBody1

                groupContext =
                    solveEquationsGroup
                        body1
                        body2
                        []
                        deltalambdaTot
                        equations
            in
            step remainingIterations
                groupContext.deltalambdaTot
                ({ bodyId1 = bodyId1
                 , bodyId2 = bodyId2
                 , equations = groupContext.equations
                 }
                    :: equationsGroups
                )
                remainingEquationsGroups
                -- we don’t put body1 in the array, because we might need it
                -- in the next iteration, because this is the order in
                -- which we generated the contact equation groups (b1, b2), (b1, b3)
                -- this lets us reduce Array.set operations
                groupContext.body1
                (if groupContext.body2.body.kind == Body.Dynamic then
                    Array.set bodyId2 groupContext.body2 newSolverBodies

                 else
                    -- static and kinematic bodies don’t change
                    newSolverBodies
                )


type alias GroupSolveResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , equations : List SolverEquation
    , deltalambdaTot : Float
    }


solveEquationsGroup : SolverBody id -> SolverBody id -> List SolverEquation -> Float -> List SolverEquation -> GroupSolveResult id
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
                { wA, vB, wB, minForce, maxForce, solverB, spookEps, solverInvC } =
                    equation

                -- G x Wlambda, where W are the body velocities
                gWlambda =
                    -(vB.x * body1.vX + vB.y * body1.vY + vB.z * body1.vZ)
                        + (wA.x * body1.wX + wA.y * body1.wY + wA.z * body1.wZ)
                        + (vB.x * body2.vX + vB.y * body2.vY + vB.z * body2.vZ)
                        + (wB.x * body2.wX + wB.y * body2.wY + wB.z * body2.wZ)

                deltalambdaPrev =
                    solverInvC * (solverB - gWlambda - spookEps * solverLambda)

                deltalambda =
                    if solverLambda + deltalambdaPrev - minForce < 0 then
                        minForce - solverLambda

                    else if solverLambda + deltalambdaPrev - maxForce > 0 then
                        maxForce - solverLambda

                    else
                        deltalambdaPrev

                newBody1 =
                    case body1.body.kind of
                        Body.Dynamic ->
                            let
                                invI1 =
                                    body1.body.invInertiaWorld

                                k1 =
                                    deltalambda * body1.body.invMass
                            in
                            { body = body1.body
                            , extId = body1.extId
                            , vX = body1.vX - k1 * vB.x
                            , vY = body1.vY - k1 * vB.y
                            , vZ = body1.vZ - k1 * vB.z
                            , wX = body1.wX + (invI1.m11 * wA.x + invI1.m12 * wA.y + invI1.m13 * wA.z) * deltalambda
                            , wY = body1.wY + (invI1.m21 * wA.x + invI1.m22 * wA.y + invI1.m23 * wA.z) * deltalambda
                            , wZ = body1.wZ + (invI1.m31 * wA.x + invI1.m32 * wA.y + invI1.m33 * wA.z) * deltalambda
                            }

                        _ ->
                            -- static and kinematic bodies don’t respond to impulses
                            body1

                newBody2 =
                    case body2.body.kind of
                        Body.Dynamic ->
                            let
                                invI2 =
                                    body2.body.invInertiaWorld

                                k2 =
                                    deltalambda * body2.body.invMass
                            in
                            { body = body2.body
                            , extId = body2.extId
                            , vX = body2.vX + k2 * vB.x
                            , vY = body2.vY + k2 * vB.y
                            , vZ = body2.vZ + k2 * vB.z
                            , wX = body2.wX + (invI2.m11 * wB.x + invI2.m12 * wB.y + invI2.m13 * wB.z) * deltalambda
                            , wY = body2.wY + (invI2.m21 * wB.x + invI2.m22 * wB.y + invI2.m23 * wB.z) * deltalambda
                            , wZ = body2.wZ + (invI2.m31 * wB.x + invI2.m32 * wB.y + invI2.m33 * wB.z) * deltalambda
                            }

                        _ ->
                            -- static and kinematic bodies don’t respond to impulses
                            body2
            in
            solveEquationsGroup
                newBody1
                newBody2
                ({ solverLambda = solverLambda + deltalambda
                 , equation = equation
                 }
                    :: equations
                )
                (deltalambdaTot + abs deltalambda)
                remainingEquations



-- LIST-BASED SOLVER (prototype)
--
-- The body list is kept in gravity-sort order — the same order BroadPhase
-- walked when generating equation pairs — so the equations are in CSR
-- order over body-list positions. Each PGS iteration walks both lists in
-- lockstep with pure forward linear search.


listToArray : Int -> id -> List (SolverBody id) -> Array (SolverBody id)
listToArray maxId firstExtId bodies =
    List.foldl
        (\b arr -> Array.set b.body.id b arr)
        (Array.repeat (maxId + 1) (sentinel firstExtId))
        bodies


runIterations : Int -> List (SolverBody id) -> List EquationsGroup -> ( List (SolverBody id), List EquationsGroup, Int )
runIterations remainingIterations bodies groups =
    let
        ( newBodies, newGroups, deltaTot ) =
            listIteration groups bodies [] [] 0

        nextGroups =
            List.reverse newGroups
    in
    if remainingIterations - 1 == 0 then
        ( newBodies, nextGroups, 0 )

    else if deltaTot - Const.precision < 0 then
        ( newBodies, nextGroups, remainingIterations - 1 )

    else
        runIterations (remainingIterations - 1) newBodies nextGroups


listIteration : List EquationsGroup -> List (SolverBody id) -> List EquationsGroup -> List (SolverBody id) -> Float -> ( List (SolverBody id), List EquationsGroup, Float )
listIteration groupsIn bodiesIn groupsOut bodiesOut deltaTot =
    case groupsIn of
        [] ->
            ( List.reverse bodiesOut ++ bodiesIn, groupsOut, deltaTot )

        firstGroup :: _ ->
            advanceToPivot firstGroup.bodyId1 groupsIn bodiesIn groupsOut bodiesOut deltaTot


advanceToPivot : Int -> List EquationsGroup -> List (SolverBody id) -> List EquationsGroup -> List (SolverBody id) -> Float -> ( List (SolverBody id), List EquationsGroup, Float )
advanceToPivot pivotId groupsIn bodiesIn groupsOut bodiesOut deltaTot =
    case bodiesIn of
        [] ->
            ( List.reverse bodiesOut, groupsOut, deltaTot )

        body :: restBodies ->
            if body.body.id - pivotId == 0 then
                processRow body restBodies pivotId groupsIn groupsOut bodiesOut [] deltaTot

            else
                advanceToPivot pivotId groupsIn restBodies groupsOut (body :: bodiesOut) deltaTot


processRow : SolverBody id -> List (SolverBody id) -> Int -> List EquationsGroup -> List EquationsGroup -> List (SolverBody id) -> List (SolverBody id) -> Float -> ( List (SolverBody id), List EquationsGroup, Float )
processRow pivot bodies pivotId groupsIn groupsOut bodiesOut rowBuffer deltaTot =
    case groupsIn of
        firstGroup :: restGroups ->
            if firstGroup.bodyId1 - pivotId == 0 then
                advanceToPartner pivot bodies pivotId firstGroup restGroups groupsOut bodiesOut rowBuffer deltaTot

            else
                listIteration groupsIn (List.reverse rowBuffer ++ bodies) groupsOut (pivot :: bodiesOut) deltaTot

        [] ->
            ( List.reverse (pivot :: bodiesOut) ++ List.reverse rowBuffer ++ bodies, groupsOut, deltaTot )


advanceToPartner : SolverBody id -> List (SolverBody id) -> Int -> EquationsGroup -> List EquationsGroup -> List EquationsGroup -> List (SolverBody id) -> List (SolverBody id) -> Float -> ( List (SolverBody id), List EquationsGroup, Float )
advanceToPartner pivot bodies pivotId group restGroups groupsOut bodiesOut rowBuffer deltaTot =
    case bodies of
        [] ->
            listIteration restGroups (List.reverse rowBuffer) groupsOut (pivot :: bodiesOut) deltaTot

        body :: restBodies ->
            if body.body.id - group.bodyId2 == 0 then
                let
                    result =
                        solveEquationsGroup pivot body [] deltaTot group.equations

                    updatedGroup =
                        { bodyId1 = group.bodyId1
                        , bodyId2 = group.bodyId2
                        , equations = result.equations
                        }
                in
                processRow result.body1 restBodies pivotId restGroups (updatedGroup :: groupsOut) bodiesOut (result.body2 :: rowBuffer) result.deltalambdaTot

            else
                advanceToPartner pivot restBodies pivotId group restGroups groupsOut bodiesOut (body :: rowBuffer) deltaTot
