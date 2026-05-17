module Internal.Solver exposing (solve)

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Body exposing (Body)
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
        , kindInt = 1
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
                    newBody1 =
                        if body1.body.kindInt == 2 then
                            let
                                invI1 =
                                    body1.body.invInertiaWorld

                                k1 =
                                    solverLambda * body1.body.invMass
                            in
                            { body = body1.body
                            , extId = body1.extId
                            , vX = body1.vX - k1 * equation.vBx
                            , vY = body1.vY - k1 * equation.vBy
                            , vZ = body1.vZ - k1 * equation.vBz
                            , wX = body1.wX + (invI1.m11 * equation.wAx + invI1.m12 * equation.wAy + invI1.m13 * equation.wAz) * solverLambda
                            , wY = body1.wY + (invI1.m21 * equation.wAx + invI1.m22 * equation.wAy + invI1.m23 * equation.wAz) * solverLambda
                            , wZ = body1.wZ + (invI1.m31 * equation.wAx + invI1.m32 * equation.wAy + invI1.m33 * equation.wAz) * solverLambda
                            }

                        else
                            body1

                    newBody2 =
                        if body2.body.kindInt == 2 then
                            let
                                invI2 =
                                    body2.body.invInertiaWorld

                                k2 =
                                    solverLambda * body2.body.invMass
                            in
                            { body = body2.body
                            , extId = body2.extId
                            , vX = body2.vX + k2 * equation.vBx
                            , vY = body2.vY + k2 * equation.vBy
                            , vZ = body2.vZ + k2 * equation.vBz
                            , wX = body2.wX + (invI2.m11 * equation.wBx + invI2.m12 * equation.wBy + invI2.m13 * equation.wBz) * solverLambda
                            , wY = body2.wY + (invI2.m21 * equation.wBx + invI2.m22 * equation.wBy + invI2.m23 * equation.wBz) * solverLambda
                            , wZ = body2.wZ + (invI2.m31 * equation.wBx + invI2.m32 * equation.wBy + invI2.m33 * equation.wBz) * solverLambda
                            }

                        else
                            body2
                in
                applyGroupWarmStart newBody1 newBody2 rest


{-| Single pass over pairGroups: build the EquationsGroups list, apply cached
warm-start impulses, and grow union-find parents — all in one walk. Replaces
three separate walks of equationsGroups in the previous code.

`PairGroup` already carries `body1 : Body` and `body2 : Body`, so the union-find
dynamic-dynamic check reads `kindInt` straight off the PairGroup without an
Array.get. Warm-start impulses commute (just additions), so processing in
pairGroups order vs. reversed-equationsGroups order yields the same array.

-}
buildAndWarmStart :
    Equation.Ctx
    -> SolverBody id
    -> Array (SolverBody id)
    -> Array Int
    -> List (EquationsGroup id)
    -> List PairGroup
    -> ( List (EquationsGroup id), Array (SolverBody id), Array Int )
buildAndWarmStart ctx prevBody1 solverBodies parents groups pairGroups =
    case pairGroups of
        [] ->
            ( groups
            , if prevBody1.body.kindInt == 2 then
                Array.set prevBody1.body.id prevBody1 solverBodies

              else
                solverBodies
            , parents
            )

        pairGroup :: rest ->
            let
                equations =
                    Equation.equationsForPair ctx pairGroup

                bodyId1 =
                    pairGroup.body1.id

                bodyId2 =
                    pairGroup.body2.id

                body1 =
                    if prevBody1.body.id - bodyId1 == 0 then
                        prevBody1

                    else
                        case Array.get bodyId1 solverBodies of
                            Just b ->
                                b

                            Nothing ->
                                prevBody1

                solverBodies1 =
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.kindInt /= 2 then
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies

                body2 =
                    case Array.get bodyId2 solverBodies1 of
                        Just b ->
                            b

                        Nothing ->
                            prevBody1

                ( newBody1, newBody2 ) =
                    applyGroupWarmStart body1 body2 equations

                equationsGroup =
                    { body1 = newBody1
                    , body2 = newBody2
                    , equations = equations
                    }

                solverBodies2 =
                    if newBody2.body.kindInt == 2 then
                        Array.set bodyId2 newBody2 solverBodies1

                    else
                        solverBodies1

                newParents =
                    if pairGroup.body1.kindInt == 2 && pairGroup.body2.kindInt == 2 then
                        union bodyId1 bodyId2 parents

                    else
                        parents
            in
            buildAndWarmStart ctx newBody1 solverBodies2 newParents (equationsGroup :: groups) rest


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

                -- Single fused pass over pairGroups: build equationsGroups,
                -- apply warm-start impulses, and grow union-find parents.
                -- Partitions equationsGroups into connected components
                -- ("islands") of dynamic bodies. Each island converges
                -- independently, so settled regions of the scene exit early
                -- instead of dragging the whole solver to the max iteration count.
                ( equationsGroups, warmStartedBodies, islandParents ) =
                    buildAndWarmStart
                        ctx
                        fillingBody
                        solverBodies
                        (Array.initialize (maxId + 1) identity)
                        []
                        pairGroups

                -- Annotate each group with its island root, then sort so
                -- consecutive entries belong to the same island. Within-island
                -- PGS order doesn't need a secondary key — body1/body2
                -- assignment (already pinned bottom-first by Physics.elm's
                -- body pre-sort) is what governs stack stability, not the
                -- pair-visit order inside the island.
                sortedGroups =
                    List.sortBy Tuple.first
                        (annotateGroupsByRoot islandParents equationsGroups [])

                ( finalSolverBodies, finalEquationsGroups, minRemainingIterations ) =
                    solveIslands iterations fillingBody warmStartedBodies sortedGroups

                iterationsUsed =
                    max 1 (iterations - minRemainingIterations)

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


step : Int -> Float -> List (EquationsGroup id) -> List (EquationsGroup id) -> SolverBody id -> Array (SolverBody id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
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

        currentGroup :: remainingEquationsGroups ->
            let
                bodyId1 =
                    currentGroup.body1.body.id

                bodyId2 =
                    currentGroup.body2.body.id

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
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.kindInt /= 2 then
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
                        currentGroup.equations
            in
            step remainingIterations
                groupContext.deltalambdaTot
                ({ body1 = groupContext.body1
                 , body2 = groupContext.body2
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
                (if groupContext.body2.body.kindInt == 2 then
                    Array.set bodyId2 groupContext.body2 newSolverBodies

                 else
                    -- static and kinematic bodies don’t change
                    newSolverBodies
                )



-- Islands: connected components of dynamic bodies in the contact/constraint
-- graph. Built with union-find inline during `buildAndWarmStart`. Each island
-- runs its own PGS iteration with independent convergence — settled regions
-- stop iterating early.


{-| Find the root of `id` while path-halving: every other node on the path is
relinked to its grandparent. Keeps trees shallow under repeated lookups, which
matters when bodies are unioned in adversarial id order.
-}
findRoot : Int -> Array Int -> ( Array Int, Int )
findRoot id parents =
    case Array.get id parents of
        Just parent ->
            if parent - id == 0 then
                ( parents, id )

            else
                case Array.get parent parents of
                    Just grandparent ->
                        if grandparent - parent == 0 then
                            ( parents, parent )

                        else
                            findRoot grandparent (Array.set id grandparent parents)

                    Nothing ->
                        ( parents, parent )

        Nothing ->
            ( parents, id )


union : Int -> Int -> Array Int -> Array Int
union a b parents =
    let
        ( parents1, rootA ) =
            findRoot a parents

        ( parents2, rootB ) =
            findRoot b parents1
    in
    if rootA - rootB == 0 then
        parents2

    else if rootA - rootB < 0 then
        Array.set rootB rootA parents2

    else
        Array.set rootA rootB parents2


{-| Pair each equationsGroup with its island root. Threads `parents` through
so path-compression updates carry across calls.

What about within-island PGS order? `Physics.elm` already pre-sorts bodies
by gravity projection, which makes `BroadPhase` emit pairs with body1 =
bottom-most body of the pair. PGS convergence on stacks is sensitive to that
body1/body2 assignment (asymmetric `minForce = 0` clamp), but not to the
order in which pairs _within_ an island are processed — confirmed empirically
by the sandbox stack-of-5 stability test. So we only need to sort by root.

-}
annotateGroupsByRoot : Array Int -> List (EquationsGroup id) -> List ( Int, EquationsGroup id ) -> List ( Int, EquationsGroup id )
annotateGroupsByRoot parents equationsGroups acc =
    case equationsGroups of
        [] ->
            acc

        group :: rest ->
            let
                -- Pick the dynamic body's id as the union-find key. If body1
                -- is static and body2 is dynamic, body1's root is itself
                -- (never unioned) so we'd misclassify this group as its own
                -- island.
                pickedId =
                    if group.body1.body.kindInt == 2 then
                        group.body1.body.id

                    else
                        group.body2.body.id

                ( newParents, root ) =
                    findRoot pickedId parents
            in
            annotateGroupsByRoot newParents rest (( root, group ) :: acc)


{-| Walk a list of (root, group) entries sorted by root. Consecutive entries
with the same root form an island; each island is solved as a unit.
-}
solveIslands : Int -> SolverBody id -> Array (SolverBody id) -> List ( Int, EquationsGroup id ) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solveIslands iterations fillingBody arr sorted =
    case sorted of
        [] ->
            ( arr, [], iterations )

        ( firstRoot, firstGroup ) :: rest ->
            collectAndSolveIsland iterations fillingBody firstRoot [ firstGroup ] arr [] iterations rest


collectAndSolveIsland : Int -> SolverBody id -> Int -> List (EquationsGroup id) -> Array (SolverBody id) -> List (EquationsGroup id) -> Int -> List ( Int, EquationsGroup id ) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
collectAndSolveIsland iterations fillingBody currentRoot currentIsland arr accGroups minRem remaining =
    case remaining of
        [] ->
            let
                ( newArr, newAccGroups, remIters ) =
                    solveOneIsland iterations fillingBody currentIsland arr accGroups
            in
            ( newArr, newAccGroups, min minRem remIters )

        ( root, group ) :: rest ->
            if root - currentRoot == 0 then
                collectAndSolveIsland iterations fillingBody currentRoot (group :: currentIsland) arr accGroups minRem rest

            else
                let
                    ( newArr, newAccGroups, remIters ) =
                        solveOneIsland iterations fillingBody currentIsland arr accGroups
                in
                collectAndSolveIsland iterations fillingBody root [ group ] newArr newAccGroups (min minRem remIters) rest


solveOneIsland : Int -> SolverBody id -> List (EquationsGroup id) -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solveOneIsland iterations fillingBody island arr accGroups =
    case island of
        [ singleGroup ] ->
            -- 2-body island: bodies are owned by this group (no sharing with
            -- any other group), so we use the SolverBody refs stashed on the
            -- group directly — no Array.get needed.
            solve2Body iterations singleGroup arr accGroups

        _ ->
            let
                ( newArr, newGroups, remIters ) =
                    step iterations 0 [] island fillingBody arr
            in
            ( newArr, newGroups ++ accGroups, remIters )


{-| Specialized PGS for a 2-body island (single equation group). The two
bodies stay in locals across iterations; only at the end (max iters or
convergence) do we write back to the array.
-}
solve2Body : Int -> EquationsGroup id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solve2Body remainingIterations group arr accGroups =
    let
        result =
            solveEquationsGroup group.body1 group.body2 [] 0 group.equations

        newGroup =
            { body1 = result.body1
            , body2 = result.body2
            , equations = result.equations
            }
    in
    if remainingIterations == 1 then
        ( flushBody result.body2 (flushBody result.body1 arr), newGroup :: accGroups, 0 )

    else if result.deltalambdaTot - Const.precision < 0 then
        ( flushBody result.body2 (flushBody result.body1 arr), newGroup :: accGroups, remainingIterations - 1 )

    else
        solve2Body (remainingIterations - 1) newGroup arr accGroups


flushBody : SolverBody id -> Array (SolverBody id) -> Array (SolverBody id)
flushBody body arr =
    if body.body.kindInt == 2 then
        Array.set body.body.id body arr

    else
        arr


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
                -- G x Wlambda, where W are the body velocities
                gWlambda =
                    -(equation.vBx * body1.vX + equation.vBy * body1.vY + equation.vBz * body1.vZ)
                        + (equation.wAx * body1.wX + equation.wAy * body1.wY + equation.wAz * body1.wZ)
                        + (equation.vBx * body2.vX + equation.vBy * body2.vY + equation.vBz * body2.vZ)
                        + (equation.wBx * body2.wX + equation.wBy * body2.wY + equation.wBz * body2.wZ)

                deltalambdaPrev =
                    equation.solverInvC * (equation.solverB - gWlambda - equation.spookEps * solverLambda)

                deltalambda =
                    if solverLambda + deltalambdaPrev - equation.minForce < 0 then
                        equation.minForce - solverLambda

                    else if solverLambda + deltalambdaPrev - equation.maxForce > 0 then
                        equation.maxForce - solverLambda

                    else
                        deltalambdaPrev

                newBody1 =
                    if body1.body.kindInt == 2 then
                        let
                            invI1 =
                                body1.body.invInertiaWorld

                            k1 =
                                deltalambda * body1.body.invMass
                        in
                        { body = body1.body
                        , extId = body1.extId
                        , vX = body1.vX - k1 * equation.vBx
                        , vY = body1.vY - k1 * equation.vBy
                        , vZ = body1.vZ - k1 * equation.vBz
                        , wX = body1.wX + (invI1.m11 * equation.wAx + invI1.m12 * equation.wAy + invI1.m13 * equation.wAz) * deltalambda
                        , wY = body1.wY + (invI1.m21 * equation.wAx + invI1.m22 * equation.wAy + invI1.m23 * equation.wAz) * deltalambda
                        , wZ = body1.wZ + (invI1.m31 * equation.wAx + invI1.m32 * equation.wAy + invI1.m33 * equation.wAz) * deltalambda
                        }

                    else
                        -- static and kinematic bodies don’t respond to impulses
                        body1

                newBody2 =
                    if body2.body.kindInt == 2 then
                        let
                            invI2 =
                                body2.body.invInertiaWorld

                            k2 =
                                deltalambda * body2.body.invMass
                        in
                        { body = body2.body
                        , extId = body2.extId
                        , vX = body2.vX + k2 * equation.vBx
                        , vY = body2.vY + k2 * equation.vBy
                        , vZ = body2.vZ + k2 * equation.vBz
                        , wX = body2.wX + (invI2.m11 * equation.wBx + invI2.m12 * equation.wBy + invI2.m13 * equation.wBz) * deltalambda
                        , wY = body2.wY + (invI2.m21 * equation.wBx + invI2.m22 * equation.wBy + invI2.m23 * equation.wBz) * deltalambda
                        , wZ = body2.wZ + (invI2.m31 * equation.wBx + invI2.m32 * equation.wBy + invI2.m33 * equation.wBz) * deltalambda
                        }

                    else
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
