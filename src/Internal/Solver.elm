module Internal.Solver exposing (annotateGroupsByRoot, solve)

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Contact exposing (PairGroup)
import Internal.Equation as Equation exposing (EquationsGroup, SolverEquation)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)


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
                            { body1
                                | vX = body1.vX - k1 * equation.vBx
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
                            { body2
                                | vX = body2.vX + k2 * equation.vBx
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
                    , deltalambdaTot = 0
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


type alias SolveResult id =
    { solverBodies : Array (SolverBody id)
    , lambdas : Dict String Float
    , tangents : Dict String Vec3
    , iterations : Int
    }


solve : Float -> Vec3 -> Int -> List PairGroup -> Int -> List ( id, Body ) -> Dict String Float -> Dict String Vec3 -> SolveResult id
solve dt gravity iterations pairGroups maxId bodiesWithIds lambdas tangents =
    case bodiesWithIds of
        [] ->
            { solverBodies = Array.empty
            , lambdas = Dict.empty
            , tangents = Dict.empty
            , iterations = 0
            }

        ( firstExtId, _ ) :: _ ->
            let
                ctx =
                    { dt = dt
                    , gravity = gravity
                    , gravityLength = Vec3.length gravity
                    , lambdas = lambdas
                    , tangents = tangents
                    }

                fillingBody =
                    SolverBody.sentinel firstExtId

                solverBodies =
                    SolverBody.fromBodies maxId bodiesWithIds

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

                ( finalLambdas, finalTangents ) =
                    List.foldl
                        (\{ equations } accs -> collectCaches equations "" False accs)
                        ( Dict.empty, Dict.empty )
                        finalEquationsGroups
            in
            { solverBodies = finalSolverBodies
            , lambdas = finalLambdas
            , tangents = finalTangents
            , iterations = iterationsUsed
            }


{-| Walk a pair group's equations once to populate both warm-start caches:
the lambda dict (keyed by contact-normal id) and the tangent dict (keyed by
the same contact-normal id, storing the first friction equation's t1).
Equation order within `addContactEquations` is [normal, t1, t2], so the
first friction following a normal is t1.
-}
collectCaches : List SolverEquation -> String -> Bool -> ( Dict String Float, Dict String Vec3 ) -> ( Dict String Float, Dict String Vec3 )
collectCaches equations lastNormalId t1Recorded ( lambdaAcc, tangentAcc ) =
    case equations of
        [] ->
            ( lambdaAcc, tangentAcc )

        { equation, solverLambda } :: rest ->
            let
                newLambdaAcc =
                    if equation.id == "" then
                        lambdaAcc

                    else
                        Dict.insert equation.id solverLambda lambdaAcc
            in
            if equation.isContactNormal then
                collectCaches rest equation.id False ( newLambdaAcc, tangentAcc )

            else if equation.frictionCoefficient > 0 && not t1Recorded && lastNormalId /= "" then
                collectCaches rest
                    lastNormalId
                    True
                    ( newLambdaAcc
                    , Dict.insert lastNormalId { x = equation.vBx, y = equation.vBy, z = equation.vBz } tangentAcc
                    )

            else
                collectCaches rest lastNormalId t1Recorded ( newLambdaAcc, tangentAcc )


{-| Solve a multi-body island via split-impulse:

  - Position loop: drives pseudo-velocity to resolve penetration. Output
    advances each body's transform at end-of-step but is discarded as
    velocity. Penetration recovery doesn't inject kinetic energy.
  - Velocity loop: two sweeps per iter — non-friction across the island
    first, then friction sized off the just-finalized normal lambdas.
    Matches Bullet's default split-pass order.

-}
step : Int -> SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
step iterations prevBody1 solverBodies currentEquationsGroups =
    let
        positionResult =
            positionLoop iterations prevBody1 solverBodies currentEquationsGroups
    in
    velocityLoop iterations positionResult.prevBody1 positionResult.solverBodies positionResult.groups


{-| Velocity iteration loop: two sweeps per iteration — non-friction first,
then friction. Friction in sweep 2 sees the just-finalized normal lambdas
across the island, so the Coulomb cone is sized after island-wide GS
propagation. Matches Bullet's default (`SOLVER_INTERLEAVE_CONTACT_AND_FRICTION_CONSTRAINTS`
off).
-}
velocityLoop : Int -> SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
velocityLoop remainingIterations prevBody1 solverBodies currentEquationsGroups =
    let
        pass1 =
            sweep NonFrictionPhase prevBody1 solverBodies [] currentEquationsGroups 0

        pass2 =
            sweep FrictionPhase pass1.prevBody1 pass1.solverBodies [] (List.reverse pass1.groups) 0

        forwardResult =
            List.reverse pass2.groups

        deltaTot =
            pass1.deltalambdaTot + pass2.deltalambdaTot
    in
    if remainingIterations == 1 then
        ( Array.set pass2.prevBody1.body.id pass2.prevBody1 pass2.solverBodies, forwardResult, 0 )

    else if deltaTot - Const.precision < 0 then
        ( Array.set pass2.prevBody1.body.id pass2.prevBody1 pass2.solverBodies, forwardResult, remainingIterations - 1 )

    else
        velocityLoop (remainingIterations - 1) pass2.prevBody1 pass2.solverBodies forwardResult


{-| Position-correction loop: drives the pseudo-velocity field via one
position sweep per iteration. Pseudo accumulates within a step then is
discarded at end-of-step (consumed only by the position integration in
`SolverBody.toBody`).
-}
positionLoop : Int -> SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> SweepResult id
positionLoop remainingIterations prevBody1 solverBodies currentEquationsGroups =
    let
        result =
            positionSweep prevBody1 solverBodies [] currentEquationsGroups 0

        forwardResult =
            List.reverse result.groups
    in
    if remainingIterations == 1 then
        { result | groups = forwardResult }

    else if result.deltalambdaTot - Const.precision < 0 then
        { result | groups = forwardResult }

    else
        positionLoop (remainingIterations - 1) result.prevBody1 result.solverBodies forwardResult


{-| Walk pair groups once, running `solvePositionPass` per pair group.
Same body-threading pattern as `sweep`.
-}
positionSweep : SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> List (EquationsGroup id) -> Float -> SweepResult id
positionSweep prevBody1 solverBodies acc currentEquationsGroups deltalambdaTot =
    case currentEquationsGroups of
        [] ->
            { prevBody1 = prevBody1
            , groups = acc
            , solverBodies = solverBodies
            , deltalambdaTot = deltalambdaTot
            }

        currentGroup :: remainingEquationsGroups ->
            let
                bodyId1 =
                    currentGroup.body1.body.id

                bodyId2 =
                    currentGroup.body2.body.id

                body1 =
                    if prevBody1.body.id - bodyId1 == 0 then
                        prevBody1

                    else
                        case Array.get bodyId1 solverBodies of
                            Just nextBody ->
                                nextBody

                            Nothing ->
                                prevBody1

                newSolverBodies =
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.kindInt /= 2 then
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies

                body2 =
                    case Array.get bodyId2 newSolverBodies of
                        Just nextBody ->
                            nextBody

                        Nothing ->
                            prevBody1

                groupResult =
                    solvePositionPass body1 body2 [] deltalambdaTot currentGroup.equations
            in
            positionSweep
                groupResult.body1
                (if groupResult.body2.body.kindInt == 2 then
                    Array.set bodyId2 groupResult.body2 newSolverBodies

                 else
                    newSolverBodies
                )
                (groupResult :: acc)
                remainingEquationsGroups
                groupResult.deltalambdaTot


type alias SweepResult id =
    { prevBody1 : SolverBody id
    , groups : List (EquationsGroup id)
    , solverBodies : Array (SolverBody id)
    , deltalambdaTot : Float
    }


{-| Walk every pair group in the island once, running `solvePass` per pair
group in the given phase. The body-threading optimisation from the original
solver is preserved: when consecutive pair groups share body1, we avoid the
Array.get/set round-trip. Groups are accumulated in reverse order.
-}
sweep : Phase -> SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> List (EquationsGroup id) -> Float -> SweepResult id
sweep phase prevBody1 solverBodies acc currentEquationsGroups deltalambdaTot =
    case currentEquationsGroups of
        [] ->
            { prevBody1 = prevBody1
            , groups = acc
            , solverBodies = solverBodies
            , deltalambdaTot = deltalambdaTot
            }

        currentGroup :: remainingEquationsGroups ->
            let
                bodyId1 =
                    currentGroup.body1.body.id

                bodyId2 =
                    currentGroup.body2.body.id

                body1 =
                    if prevBody1.body.id - bodyId1 == 0 then
                        prevBody1

                    else
                        case Array.get bodyId1 solverBodies of
                            Just nextBody ->
                                nextBody

                            Nothing ->
                                prevBody1

                newSolverBodies =
                    if prevBody1.body.id - bodyId1 == 0 || prevBody1.body.kindInt /= 2 then
                        solverBodies

                    else
                        Array.set prevBody1.body.id prevBody1 solverBodies

                body2 =
                    case Array.get bodyId2 newSolverBodies of
                        Just nextBody ->
                            nextBody

                        Nothing ->
                            prevBody1

                groupResult =
                    solvePass phase body1 body2 [] deltalambdaTot 0 currentGroup.equations
            in
            sweep
                phase
                groupResult.body1
                (if groupResult.body2.body.kindInt == 2 then
                    Array.set bodyId2 groupResult.body2 newSolverBodies

                 else
                    newSolverBodies
                )
                (groupResult :: acc)
                remainingEquationsGroups
                groupResult.deltalambdaTot



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

No secondary sort key for within-island order: PGS needs pairs visited
bottom-first, and broadphase already emits them that way. Nothing
between here and `step` disturbs the sequence — `List.sortBy` is stable
and intermediate reverses don't matter. `SolverIslandsTest` guards it.

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
                    step iterations fillingBody arr island
            in
            ( newArr, newGroups ++ accGroups, remIters )


{-| Specialized PGS for a 2-body island (single equation group). Position
iterations come first (driving pseudo-velocity), then velocity iterations.
The two bodies stay in locals across iterations; only at the end do we
write back to the array.
-}
solve2Body : Int -> EquationsGroup id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solve2Body iterations group arr accGroups =
    let
        afterPosition =
            solve2BodyPosition iterations group
    in
    solve2BodyVelocity iterations afterPosition arr accGroups


solve2BodyPosition : Int -> EquationsGroup id -> EquationsGroup id
solve2BodyPosition remainingIterations group =
    let
        result =
            solvePositionPass group.body1 group.body2 [] 0 group.equations
    in
    if remainingIterations == 1 then
        result

    else if result.deltalambdaTot - Const.precision < 0 then
        result

    else
        solve2BodyPosition (remainingIterations - 1) result


solve2BodyVelocity : Int -> EquationsGroup id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solve2BodyVelocity remainingIterations group arr accGroups =
    let
        result =
            solveEquationsGroup group.body1 group.body2 0 group.equations
    in
    if remainingIterations == 1 then
        ( flushBody result.body2 (flushBody result.body1 arr), result :: accGroups, 0 )

    else if result.deltalambdaTot - Const.precision < 0 then
        ( flushBody result.body2 (flushBody result.body1 arr), result :: accGroups, remainingIterations - 1 )

    else
        solve2BodyVelocity (remainingIterations - 1) result arr accGroups


flushBody : SolverBody id -> Array (SolverBody id) -> Array (SolverBody id)
flushBody body arr =
    if body.body.kindInt == 2 then
        Array.set body.body.id body arr

    else
        arr


{-| Two-pass PGS over a single pair group: pass 1 solves non-friction
equations (contact normals, joints), pass 2 solves friction equations sized
off the just-updated normal lambdas via the Coulomb-cone clamp `±μ · λ_n`.
Used for 2-body islands.
-}
solveEquationsGroup : SolverBody id -> SolverBody id -> Float -> List SolverEquation -> EquationsGroup id
solveEquationsGroup body1 body2 deltalambdaTot currentEquations =
    let
        normalsDone =
            solvePass NonFrictionPhase body1 body2 [] deltalambdaTot 0 currentEquations

        frictionDone =
            solvePass FrictionPhase normalsDone.body1 normalsDone.body2 [] normalsDone.deltalambdaTot 0 normalsDone.equations
    in
    frictionDone


{-| PGS over a single pair group for the split-impulse position phase.
Walks every non-friction equation (contacts, joints), driving the pseudo-
velocity field using `solverBPosition` as the RHS. Friction equations have
no positional component and are passed through unchanged.
-}
solvePositionPass : SolverBody id -> SolverBody id -> List SolverEquation -> Float -> List SolverEquation -> EquationsGroup id
solvePositionPass body1 body2 acc deltalambdaTot currentEquations =
    case currentEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , equations = List.reverse acc
            , deltalambdaTot = deltalambdaTot
            }

        ({ positionLambda, equation } as solverEq) :: remainingEquations ->
            if equation.frictionCoefficient > 0 then
                solvePositionPass body1 body2 (solverEq :: acc) deltalambdaTot remainingEquations

            else
                let
                    gWlambda =
                        -(equation.vBx * body1.pvX + equation.vBy * body1.pvY + equation.vBz * body1.pvZ)
                            + (equation.wAx * body1.pwX + equation.wAy * body1.pwY + equation.wAz * body1.pwZ)
                            + (equation.vBx * body2.pvX + equation.vBy * body2.pvY + equation.vBz * body2.pvZ)
                            + (equation.wBx * body2.pwX + equation.wBy * body2.pwY + equation.wBz * body2.pwZ)

                    deltalambdaPrev =
                        equation.solverInvC * (equation.solverBPosition - gWlambda - equation.spookEps * positionLambda)

                    deltalambda =
                        if positionLambda + deltalambdaPrev - equation.minForce < 0 then
                            equation.minForce - positionLambda

                        else if positionLambda + deltalambdaPrev - equation.maxForce > 0 then
                            equation.maxForce - positionLambda

                        else
                            deltalambdaPrev

                    newPositionLambda =
                        positionLambda + deltalambda

                    newBody1 =
                        if body1.body.kindInt == 2 then
                            let
                                invI1 =
                                    body1.body.invInertiaWorld

                                k1 =
                                    deltalambda * body1.body.invMass
                            in
                            { body1
                                | pvX = body1.pvX - k1 * equation.vBx
                                , pvY = body1.pvY - k1 * equation.vBy
                                , pvZ = body1.pvZ - k1 * equation.vBz
                                , pwX = body1.pwX + (invI1.m11 * equation.wAx + invI1.m12 * equation.wAy + invI1.m13 * equation.wAz) * deltalambda
                                , pwY = body1.pwY + (invI1.m21 * equation.wAx + invI1.m22 * equation.wAy + invI1.m23 * equation.wAz) * deltalambda
                                , pwZ = body1.pwZ + (invI1.m31 * equation.wAx + invI1.m32 * equation.wAy + invI1.m33 * equation.wAz) * deltalambda
                            }

                        else
                            body1

                    newBody2 =
                        if body2.body.kindInt == 2 then
                            let
                                invI2 =
                                    body2.body.invInertiaWorld

                                k2 =
                                    deltalambda * body2.body.invMass
                            in
                            { body2
                                | pvX = body2.pvX + k2 * equation.vBx
                                , pvY = body2.pvY + k2 * equation.vBy
                                , pvZ = body2.pvZ + k2 * equation.vBz
                                , pwX = body2.pwX + (invI2.m11 * equation.wBx + invI2.m12 * equation.wBy + invI2.m13 * equation.wBz) * deltalambda
                                , pwY = body2.pwY + (invI2.m21 * equation.wBx + invI2.m22 * equation.wBy + invI2.m23 * equation.wBz) * deltalambda
                                , pwZ = body2.pwZ + (invI2.m31 * equation.wBx + invI2.m32 * equation.wBy + invI2.m33 * equation.wBz) * deltalambda
                            }

                        else
                            body2
                in
                solvePositionPass
                    newBody1
                    newBody2
                    ({ solverEq | positionLambda = newPositionLambda } :: acc)
                    (deltalambdaTot + abs deltalambda)
                    remainingEquations


type Phase
    = NonFrictionPhase
    | FrictionPhase


solvePass : Phase -> SolverBody id -> SolverBody id -> List SolverEquation -> Float -> Float -> List SolverEquation -> EquationsGroup id
solvePass phase body1 body2 acc deltalambdaTot lastNormalLambda currentEquations =
    case currentEquations of
        [] ->
            { body1 = body1
            , body2 = body2
            , equations = List.reverse acc
            , deltalambdaTot = deltalambdaTot
            }

        ({ solverLambda, equation } as solverEq) :: remainingEquations ->
            let
                isFriction =
                    equation.frictionCoefficient > 0

                shouldSolve =
                    case phase of
                        NonFrictionPhase ->
                            not isFriction

                        FrictionPhase ->
                            isFriction
            in
            if shouldSolve then
                let
                    -- G x Wlambda, where W are the body velocities
                    gWlambda =
                        -(equation.vBx * body1.vX + equation.vBy * body1.vY + equation.vBz * body1.vZ)
                            + (equation.wAx * body1.wX + equation.wAy * body1.wY + equation.wAz * body1.wZ)
                            + (equation.vBx * body2.vX + equation.vBy * body2.vY + equation.vBz * body2.vZ)
                            + (equation.wBx * body2.wX + equation.wBy * body2.wY + equation.wBz * body2.wZ)

                    deltalambdaPrev =
                        equation.solverInvC * (equation.solverB - gWlambda - equation.spookEps * solverLambda)

                    ( minForce, maxForce ) =
                        if isFriction then
                            let
                                cap =
                                    equation.frictionCoefficient * lastNormalLambda
                            in
                            ( -cap, cap )

                        else
                            ( equation.minForce, equation.maxForce )

                    deltalambda =
                        if solverLambda + deltalambdaPrev - minForce < 0 then
                            minForce - solverLambda

                        else if solverLambda + deltalambdaPrev - maxForce > 0 then
                            maxForce - solverLambda

                        else
                            deltalambdaPrev

                    newSolverLambda =
                        solverLambda + deltalambda

                    newBody1 =
                        if body1.body.kindInt == 2 then
                            let
                                invI1 =
                                    body1.body.invInertiaWorld

                                k1 =
                                    deltalambda * body1.body.invMass
                            in
                            { body1
                                | vX = body1.vX - k1 * equation.vBx
                                , vY = body1.vY - k1 * equation.vBy
                                , vZ = body1.vZ - k1 * equation.vBz
                                , wX = body1.wX + (invI1.m11 * equation.wAx + invI1.m12 * equation.wAy + invI1.m13 * equation.wAz) * deltalambda
                                , wY = body1.wY + (invI1.m21 * equation.wAx + invI1.m22 * equation.wAy + invI1.m23 * equation.wAz) * deltalambda
                                , wZ = body1.wZ + (invI1.m31 * equation.wAx + invI1.m32 * equation.wAy + invI1.m33 * equation.wAz) * deltalambda
                            }

                        else
                            body1

                    newBody2 =
                        if body2.body.kindInt == 2 then
                            let
                                invI2 =
                                    body2.body.invInertiaWorld

                                k2 =
                                    deltalambda * body2.body.invMass
                            in
                            { body2
                                | vX = body2.vX + k2 * equation.vBx
                                , vY = body2.vY + k2 * equation.vBy
                                , vZ = body2.vZ + k2 * equation.vBz
                                , wX = body2.wX + (invI2.m11 * equation.wBx + invI2.m12 * equation.wBy + invI2.m13 * equation.wBz) * deltalambda
                                , wY = body2.wY + (invI2.m21 * equation.wBx + invI2.m22 * equation.wBy + invI2.m23 * equation.wBz) * deltalambda
                                , wZ = body2.wZ + (invI2.m31 * equation.wBx + invI2.m32 * equation.wBy + invI2.m33 * equation.wBz) * deltalambda
                            }

                        else
                            body2

                    newLastNormalLambda =
                        if equation.isContactNormal then
                            newSolverLambda

                        else
                            lastNormalLambda
                in
                solvePass
                    phase
                    newBody1
                    newBody2
                    ({ solverEq | solverLambda = newSolverLambda } :: acc)
                    (deltalambdaTot + abs deltalambda)
                    newLastNormalLambda
                    remainingEquations

            else
                -- Skip this equation in the current phase. Still need to
                -- read its solverLambda so friction equations in pass 2 see
                -- the latest normal lambda.
                let
                    newLastNormalLambda =
                        if equation.isContactNormal then
                            solverLambda

                        else
                            lastNormalLambda
                in
                solvePass
                    phase
                    body1
                    body2
                    (solverEq :: acc)
                    deltalambdaTot
                    newLastNormalLambda
                    remainingEquations
