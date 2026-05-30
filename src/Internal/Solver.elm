module Internal.Solver exposing (annotateGroupsByRoot, solve)

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Contact exposing (PairGroup)
import Internal.Equation as Equation exposing (ContactEquations, Equation, EquationsGroup, SolverEquation)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Integer min/max via subtraction, so the comparison compiles to a direct JS
`<` rather than the shared, polymorphic `_Utils_cmp` that `Basics.min`/`max`
route through.
-}
minInt : Int -> Int -> Int
minInt a b =
    if a - b < 0 then
        a

    else
        b


maxInt : Int -> Int -> Int
maxInt a b =
    if a - b > 0 then
        a

    else
        b


{-| Apply the impulse corresponding to a seeded lambda to both solver bodies.
This pre-loads the body delta-v so the solver starts from a warm state.
-}
applyEquationWarmStart : SolverBody id -> SolverBody id -> SolverEquation -> ( SolverBody id, SolverBody id )
applyEquationWarmStart body1 body2 { solverLambda, equation } =
    if solverLambda == 0 then
        ( body1, body2 )

    else
        ( if body1.body.kindInt == 2 then
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
        , if body2.body.kindInt == 2 then
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
        )


applyConstraintsWarmStart : SolverBody id -> SolverBody id -> List SolverEquation -> ( SolverBody id, SolverBody id )
applyConstraintsWarmStart body1 body2 equations =
    case equations of
        [] ->
            ( body1, body2 )

        equation :: rest ->
            let
                ( newBody1, newBody2 ) =
                    applyEquationWarmStart body1 body2 equation
            in
            applyConstraintsWarmStart newBody1 newBody2 rest


applyContactsWarmStart : SolverBody id -> SolverBody id -> List ContactEquations -> ( SolverBody id, SolverBody id )
applyContactsWarmStart body1 body2 contacts =
    case contacts of
        [] ->
            ( body1, body2 )

        { normal, friction1, friction2 } :: rest ->
            let
                ( b1n, b2n ) =
                    applyEquationWarmStart body1 body2 normal

                ( b1f1, b2f1 ) =
                    applyEquationWarmStart b1n b2n friction1

                ( b1f2, b2f2 ) =
                    applyEquationWarmStart b1f1 b2f1 friction2
            in
            applyContactsWarmStart b1f2 b2f2 rest


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
                built =
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

                ( wb1, wb2 ) =
                    applyConstraintsWarmStart body1 body2 built.constraints

                ( newBody1, newBody2 ) =
                    applyContactsWarmStart wb1 wb2 built.contacts

                equationsGroup =
                    { body1 = newBody1
                    , body2 = newBody2
                    , contacts = built.contacts
                    , constraints = built.constraints
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
                    List.sortWith
                        (\( a, _ ) ( b, _ ) ->
                            if a - b < 0 then
                                LT

                            else if a - b > 0 then
                                GT

                            else
                                EQ
                        )
                        (annotateGroupsByRoot islandParents equationsGroups [])

                ( finalSolverBodies, finalEquationsGroups, minRemainingIterations ) =
                    solveIslands iterations fillingBody warmStartedBodies sortedGroups

                iterationsUsed =
                    maxInt 1 (iterations - minRemainingIterations)

                ( finalLambdas, finalTangents ) =
                    List.foldl
                        (\group accs -> collectCaches group.contacts group.constraints accs)
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
collectCaches : List ContactEquations -> List SolverEquation -> ( Dict String Float, Dict String Vec3 ) -> ( Dict String Float, Dict String Vec3 )
collectCaches contacts constraints accs =
    collectConstraintCaches constraints (collectContactCaches contacts accs)


{-| Each contact caches its normal's lambda and its friction1 (t1) direction,
both keyed by the contact-normal id.
-}
collectContactCaches : List ContactEquations -> ( Dict String Float, Dict String Vec3 ) -> ( Dict String Float, Dict String Vec3 )
collectContactCaches contacts ( lambdaAcc, tangentAcc ) =
    case contacts of
        [] ->
            ( lambdaAcc, tangentAcc )

        { normal, friction1 } :: rest ->
            collectContactCaches rest
                ( Dict.insert normal.equation.id normal.solverLambda lambdaAcc
                , Dict.insert normal.equation.id
                    { x = friction1.equation.vBx, y = friction1.equation.vBy, z = friction1.equation.vBz }
                    tangentAcc
                )


collectConstraintCaches : List SolverEquation -> ( Dict String Float, Dict String Vec3 ) -> ( Dict String Float, Dict String Vec3 )
collectConstraintCaches equations ( lambdaAcc, tangentAcc ) =
    case equations of
        [] ->
            ( lambdaAcc, tangentAcc )

        { equation, solverLambda } :: rest ->
            collectConstraintCaches rest
                ( if equation.id == "" then
                    lambdaAcc

                  else
                    Dict.insert equation.id solverLambda lambdaAcc
                , tangentAcc
                )


{-| Solve a multi-body island: the velocity loop runs two sweeps per iteration
— non-friction (contact normals + joints) across the whole island first, then
friction sized off the just-finalized normal lambdas. Penetration recovery is
folded into the non-friction velocity bias (Baumgarte ERP), so there is no
separate position pass.
-}
step : Int -> SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
step iterations prevBody1 solverBodies currentEquationsGroups =
    velocityLoop iterations prevBody1 solverBodies currentEquationsGroups


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
                    case phase of
                        NonFrictionPhase ->
                            velocityNonFrictionGroup body1 body2 deltalambdaTot currentGroup.contacts currentGroup.constraints

                        FrictionPhase ->
                            velocityFrictionGroup body1 body2 deltalambdaTot currentGroup.contacts currentGroup.constraints
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
            ( newArr, newAccGroups, minInt minRem remIters )

        ( root, group ) :: rest ->
            if root - currentRoot == 0 then
                collectAndSolveIsland iterations fillingBody currentRoot (group :: currentIsland) arr accGroups minRem rest

            else
                let
                    ( newArr, newAccGroups, remIters ) =
                        solveOneIsland iterations fillingBody currentIsland arr accGroups
                in
                collectAndSolveIsland iterations fillingBody root [ group ] newArr newAccGroups (minInt minRem remIters) rest


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


{-| Specialized PGS for a 2-body island (single equation group). The two
bodies stay in locals across iterations; only at the end do we write back to
the array.
-}
solve2Body : Int -> EquationsGroup id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solve2Body iterations group arr accGroups =
    solve2BodyVelocity iterations group arr accGroups


solve2BodyVelocity : Int -> EquationsGroup id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solve2BodyVelocity remainingIterations group arr accGroups =
    let
        result =
            velocityGroup group.body1 group.body2 0 group.contacts group.constraints
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


{-| Apply an impulse of `deltalambda` to body1 (the `vA = -vB` side) along an
equation's Jacobian row, updating linear/angular velocity. Static bodies
(kindInt /= 2) are returned unchanged.
-}
applyVelocityBody1 : Float -> Equation -> SolverBody id -> SolverBody id
applyVelocityBody1 deltalambda equation body =
    if body.body.kindInt == 2 then
        let
            invI =
                body.body.invInertiaWorld

            k =
                deltalambda * body.body.invMass
        in
        { body = body.body
        , extId = body.extId
        , vX = body.vX - k * equation.vBx
        , vY = body.vY - k * equation.vBy
        , vZ = body.vZ - k * equation.vBz
        , wX = body.wX + (invI.m11 * equation.wAx + invI.m12 * equation.wAy + invI.m13 * equation.wAz) * deltalambda
        , wY = body.wY + (invI.m21 * equation.wAx + invI.m22 * equation.wAy + invI.m23 * equation.wAz) * deltalambda
        , wZ = body.wZ + (invI.m31 * equation.wAx + invI.m32 * equation.wAy + invI.m33 * equation.wAz) * deltalambda
        }

    else
        body


applyVelocityBody2 : Float -> Equation -> SolverBody id -> SolverBody id
applyVelocityBody2 deltalambda equation body =
    if body.body.kindInt == 2 then
        let
            invI =
                body.body.invInertiaWorld

            k =
                deltalambda * body.body.invMass
        in
        { body = body.body
        , extId = body.extId
        , vX = body.vX + k * equation.vBx
        , vY = body.vY + k * equation.vBy
        , vZ = body.vZ + k * equation.vBz
        , wX = body.wX + (invI.m11 * equation.wBx + invI.m12 * equation.wBy + invI.m13 * equation.wBz) * deltalambda
        , wY = body.wY + (invI.m21 * equation.wBx + invI.m22 * equation.wBy + invI.m23 * equation.wBz) * deltalambda
        , wZ = body.wZ + (invI.m31 * equation.wBx + invI.m32 * equation.wBy + invI.m33 * equation.wBz) * deltalambda
        }

    else
        body


type alias VelocityListResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , equations : List SolverEquation
    , deltalambdaTot : Float
    }


type alias VelocityContactsResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , contacts : List ContactEquations
    , deltalambdaTot : Float
    }


{-| Solve a flat list of non-friction equations (constraints/joints) with fixed
[minForce, maxForce] bounds. Equations come back in input order.
-}
solveVelocityConstraints : SolverBody id -> SolverBody id -> List SolverEquation -> Float -> List SolverEquation -> VelocityListResult id
solveVelocityConstraints body1 body2 acc deltalambdaTot equations =
    case equations of
        [] ->
            { body1 = body1, body2 = body2, equations = List.reverse acc, deltalambdaTot = deltalambdaTot }

        { solverLambda, equation } :: rest ->
            let
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
            in
            solveVelocityConstraints
                (applyVelocityBody1 deltalambda equation body1)
                (applyVelocityBody2 deltalambda equation body2)
                ({ equation = equation, solverLambda = solverLambda + deltalambda } :: acc)
                (deltalambdaTot + abs deltalambda)
                rest


{-| Pass 1 contact solve: solve each block's normal with its fixed bounds,
leaving the frictions untouched. Same Gauss-Seidel order as the old interleaved
list (frictions were skipped here), so convergence is identical.
-}
solveVelocityNormals : SolverBody id -> SolverBody id -> List ContactEquations -> Float -> List ContactEquations -> VelocityContactsResult id
solveVelocityNormals body1 body2 acc deltalambdaTot contacts =
    case contacts of
        [] ->
            { body1 = body1, body2 = body2, contacts = List.reverse acc, deltalambdaTot = deltalambdaTot }

        { normal, friction1, friction2 } :: rest ->
            let
                equation =
                    normal.equation

                solverLambda =
                    normal.solverLambda

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

                newNormal =
                    { equation = equation, solverLambda = solverLambda + deltalambda }
            in
            solveVelocityNormals
                (applyVelocityBody1 deltalambda equation body1)
                (applyVelocityBody2 deltalambda equation body2)
                ({ normal = newNormal, friction1 = friction1, friction2 = friction2 } :: acc)
                (deltalambdaTot + abs deltalambda)
                rest


{-| Pass 2 contact solve: solve each block's two friction equations, the
Coulomb cone ±μ·λ\_n sized from the block's now-finalized normal lambda.
friction1 then friction2, matching the old interleaved solve order.
-}
solveVelocityFrictions : SolverBody id -> SolverBody id -> List ContactEquations -> Float -> List ContactEquations -> VelocityContactsResult id
solveVelocityFrictions body1 body2 acc deltalambdaTot contacts =
    case contacts of
        [] ->
            { body1 = body1, body2 = body2, contacts = List.reverse acc, deltalambdaTot = deltalambdaTot }

        { normal, friction1, friction2 } :: rest ->
            let
                normalLambda =
                    normal.solverLambda

                eq1 =
                    friction1.equation

                cap1 =
                    eq1.frictionCoefficient * normalLambda

                gW1 =
                    -(eq1.vBx * body1.vX + eq1.vBy * body1.vY + eq1.vBz * body1.vZ)
                        + (eq1.wAx * body1.wX + eq1.wAy * body1.wY + eq1.wAz * body1.wZ)
                        + (eq1.vBx * body2.vX + eq1.vBy * body2.vY + eq1.vBz * body2.vZ)
                        + (eq1.wBx * body2.wX + eq1.wBy * body2.wY + eq1.wBz * body2.wZ)

                dPrev1 =
                    eq1.solverInvC * (eq1.solverB - gW1 - eq1.spookEps * friction1.solverLambda)

                d1 =
                    if friction1.solverLambda + dPrev1 + cap1 < 0 then
                        -cap1 - friction1.solverLambda

                    else if friction1.solverLambda + dPrev1 - cap1 > 0 then
                        cap1 - friction1.solverLambda

                    else
                        dPrev1

                invI1 =
                    body1.body.invInertiaWorld

                invI2 =
                    body2.body.invInertiaWorld

                -- friction1's updated velocities, kept as locals instead of a
                -- throwaway SolverBody so friction2's gWlambda can read them.
                -- invMass / invInertiaWorld are 0 for static bodies, so these
                -- collapse to the body's own velocity without a kindInt guard.
                k1a =
                    d1 * body1.body.invMass

                b1vX =
                    body1.vX - k1a * eq1.vBx

                b1vY =
                    body1.vY - k1a * eq1.vBy

                b1vZ =
                    body1.vZ - k1a * eq1.vBz

                b1wX =
                    body1.wX + (invI1.m11 * eq1.wAx + invI1.m12 * eq1.wAy + invI1.m13 * eq1.wAz) * d1

                b1wY =
                    body1.wY + (invI1.m21 * eq1.wAx + invI1.m22 * eq1.wAy + invI1.m23 * eq1.wAz) * d1

                b1wZ =
                    body1.wZ + (invI1.m31 * eq1.wAx + invI1.m32 * eq1.wAy + invI1.m33 * eq1.wAz) * d1

                k1b =
                    d1 * body2.body.invMass

                b2vX =
                    body2.vX + k1b * eq1.vBx

                b2vY =
                    body2.vY + k1b * eq1.vBy

                b2vZ =
                    body2.vZ + k1b * eq1.vBz

                b2wX =
                    body2.wX + (invI2.m11 * eq1.wBx + invI2.m12 * eq1.wBy + invI2.m13 * eq1.wBz) * d1

                b2wY =
                    body2.wY + (invI2.m21 * eq1.wBx + invI2.m22 * eq1.wBy + invI2.m23 * eq1.wBz) * d1

                b2wZ =
                    body2.wZ + (invI2.m31 * eq1.wBx + invI2.m32 * eq1.wBy + invI2.m33 * eq1.wBz) * d1

                eq2 =
                    friction2.equation

                cap2 =
                    eq2.frictionCoefficient * normalLambda

                gW2 =
                    -(eq2.vBx * b1vX + eq2.vBy * b1vY + eq2.vBz * b1vZ)
                        + (eq2.wAx * b1wX + eq2.wAy * b1wY + eq2.wAz * b1wZ)
                        + (eq2.vBx * b2vX + eq2.vBy * b2vY + eq2.vBz * b2vZ)
                        + (eq2.wBx * b2wX + eq2.wBy * b2wY + eq2.wBz * b2wZ)

                dPrev2 =
                    eq2.solverInvC * (eq2.solverB - gW2 - eq2.spookEps * friction2.solverLambda)

                d2 =
                    if friction2.solverLambda + dPrev2 + cap2 < 0 then
                        -cap2 - friction2.solverLambda

                    else if friction2.solverLambda + dPrev2 - cap2 > 0 then
                        cap2 - friction2.solverLambda

                    else
                        dPrev2

                -- Build each body once, combining the friction1 (already in b1*/b2*)
                -- and friction2 impulses.
                newBody1 =
                    if body1.body.kindInt == 2 then
                        let
                            k2a =
                                d2 * body1.body.invMass
                        in
                        { body = body1.body
                        , extId = body1.extId
                        , vX = b1vX - k2a * eq2.vBx
                        , vY = b1vY - k2a * eq2.vBy
                        , vZ = b1vZ - k2a * eq2.vBz
                        , wX = b1wX + (invI1.m11 * eq2.wAx + invI1.m12 * eq2.wAy + invI1.m13 * eq2.wAz) * d2
                        , wY = b1wY + (invI1.m21 * eq2.wAx + invI1.m22 * eq2.wAy + invI1.m23 * eq2.wAz) * d2
                        , wZ = b1wZ + (invI1.m31 * eq2.wAx + invI1.m32 * eq2.wAy + invI1.m33 * eq2.wAz) * d2
                        }

                    else
                        body1

                newBody2 =
                    if body2.body.kindInt == 2 then
                        let
                            k2b =
                                d2 * body2.body.invMass
                        in
                        { body = body2.body
                        , extId = body2.extId
                        , vX = b2vX + k2b * eq2.vBx
                        , vY = b2vY + k2b * eq2.vBy
                        , vZ = b2vZ + k2b * eq2.vBz
                        , wX = b2wX + (invI2.m11 * eq2.wBx + invI2.m12 * eq2.wBy + invI2.m13 * eq2.wBz) * d2
                        , wY = b2wY + (invI2.m21 * eq2.wBx + invI2.m22 * eq2.wBy + invI2.m23 * eq2.wBz) * d2
                        , wZ = b2wZ + (invI2.m31 * eq2.wBx + invI2.m32 * eq2.wBy + invI2.m33 * eq2.wBz) * d2
                        }

                    else
                        body2

                newFriction1 =
                    { equation = eq1, solverLambda = friction1.solverLambda + d1 }

                newFriction2 =
                    { equation = eq2, solverLambda = friction2.solverLambda + d2 }
            in
            solveVelocityFrictions
                newBody1
                newBody2
                ({ normal = normal, friction1 = newFriction1, friction2 = newFriction2 } :: acc)
                (deltalambdaTot + abs d1 + abs d2)
                rest


{-| Pass 1 over a pair group: constraints first (matching the old list head),
then contact normals. Frictions left for pass 2.
-}
velocityNonFrictionGroup : SolverBody id -> SolverBody id -> Float -> List ContactEquations -> List SolverEquation -> EquationsGroup id
velocityNonFrictionGroup body1 body2 deltalambdaTot contacts constraints =
    let
        afterConstraints =
            solveVelocityConstraints body1 body2 [] deltalambdaTot constraints

        afterNormals =
            solveVelocityNormals afterConstraints.body1 afterConstraints.body2 [] afterConstraints.deltalambdaTot contacts
    in
    { body1 = afterNormals.body1
    , body2 = afterNormals.body2
    , contacts = afterNormals.contacts
    , constraints = afterConstraints.equations
    , deltalambdaTot = afterNormals.deltalambdaTot
    }


{-| Pass 2 over a pair group: contact frictions only.
-}
velocityFrictionGroup : SolverBody id -> SolverBody id -> Float -> List ContactEquations -> List SolverEquation -> EquationsGroup id
velocityFrictionGroup body1 body2 deltalambdaTot contacts constraints =
    let
        afterFrictions =
            solveVelocityFrictions body1 body2 [] deltalambdaTot contacts
    in
    { body1 = afterFrictions.body1
    , body2 = afterFrictions.body2
    , contacts = afterFrictions.contacts
    , constraints = constraints
    , deltalambdaTot = afterFrictions.deltalambdaTot
    }


{-| Both velocity passes over a single pair group (used by 2-body islands).
-}
velocityGroup : SolverBody id -> SolverBody id -> Float -> List ContactEquations -> List SolverEquation -> EquationsGroup id
velocityGroup body1 body2 deltalambdaTot contacts constraints =
    let
        nonFriction =
            velocityNonFrictionGroup body1 body2 deltalambdaTot contacts constraints
    in
    velocityFrictionGroup nonFriction.body1 nonFriction.body2 nonFriction.deltalambdaTot nonFriction.contacts nonFriction.constraints


type Phase
    = NonFrictionPhase
    | FrictionPhase
