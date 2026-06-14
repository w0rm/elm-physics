module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Contact exposing (PairGroup)
import Internal.ContactCache as Cache exposing (ContactCache)
import Internal.ContactId as ContactId
import Internal.Equation as Equation exposing (ConstraintEquation, ContactEquations, EquationsGroup, Jacobian)
import Internal.Islands as Islands exposing (Islands)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Integer min/max via subtraction, compiling to a direct JS `<` instead of
the polymorphic `_Utils_cmp` behind `Basics.min`/`max`.
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


{-| Pre-load both bodies with a seeded lambda's impulse, so the solver starts
from a warm state.
-}
applyEquationWarmStart : Float -> Jacobian -> SolverBody id -> SolverBody id -> ( SolverBody id, SolverBody id )
applyEquationWarmStart solverLambda jacobian body1 body2 =
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
            , vX = body1.vX - k1 * jacobian.vBx
            , vY = body1.vY - k1 * jacobian.vBy
            , vZ = body1.vZ - k1 * jacobian.vBz
            , wX = body1.wX + (invI1.m11 * jacobian.wAx + invI1.m12 * jacobian.wAy + invI1.m13 * jacobian.wAz) * solverLambda
            , wY = body1.wY + (invI1.m21 * jacobian.wAx + invI1.m22 * jacobian.wAy + invI1.m23 * jacobian.wAz) * solverLambda
            , wZ = body1.wZ + (invI1.m31 * jacobian.wAx + invI1.m32 * jacobian.wAy + invI1.m33 * jacobian.wAz) * solverLambda
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
            , vX = body2.vX + k2 * jacobian.vBx
            , vY = body2.vY + k2 * jacobian.vBy
            , vZ = body2.vZ + k2 * jacobian.vBz
            , wX = body2.wX + (invI2.m11 * jacobian.wBx + invI2.m12 * jacobian.wBy + invI2.m13 * jacobian.wBz) * solverLambda
            , wY = body2.wY + (invI2.m21 * jacobian.wBx + invI2.m22 * jacobian.wBy + invI2.m23 * jacobian.wBz) * solverLambda
            , wZ = body2.wZ + (invI2.m31 * jacobian.wBx + invI2.m32 * jacobian.wBy + invI2.m33 * jacobian.wBz) * solverLambda
            }

          else
            body2
        )


applyConstraintsWarmStart : SolverBody id -> SolverBody id -> List ConstraintEquation -> ( SolverBody id, SolverBody id )
applyConstraintsWarmStart body1 body2 equations =
    case equations of
        [] ->
            ( body1, body2 )

        constraint :: rest ->
            let
                ( newBody1, newBody2 ) =
                    applyEquationWarmStart constraint.solverLambda constraint.jacobian body1 body2
            in
            applyConstraintsWarmStart newBody1 newBody2 rest


applyContactsWarmStart : SolverBody id -> SolverBody id -> List ContactEquations -> ( SolverBody id, SolverBody id )
applyContactsWarmStart body1 body2 contacts =
    case contacts of
        [] ->
            ( body1, body2 )

        contact :: rest ->
            let
                ( b1n, b2n ) =
                    applyEquationWarmStart contact.normalLambda contact.data.normal body1 body2

                ( b1f1, b2f1 ) =
                    applyEquationWarmStart contact.friction1Lambda contact.data.friction1 b1n b2n

                ( b1f2, b2f2 ) =
                    applyEquationWarmStart contact.friction2Lambda contact.data.friction2 b1f1 b2f1
            in
            applyContactsWarmStart b1f2 b2f2 rest


buildAndWarmStart :
    Equation.Ctx
    -> SolverBody id
    -> Array (SolverBody id)
    -> Islands
    -> List (EquationsGroup id)
    -> List PairGroup
    -> ( List (EquationsGroup id), Array (SolverBody id), Islands )
buildAndWarmStart ctx prevBody1 solverBodies islands groups pairGroups =
    case pairGroups of
        [] ->
            ( groups
            , if prevBody1.body.kindInt == 2 then
                Array.set prevBody1.body.id prevBody1 solverBodies

              else
                solverBodies
            , islands
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

                newIslands =
                    if pairGroup.body1.kindInt == 2 && pairGroup.body2.kindInt == 2 then
                        Islands.connect bodyId1 bodyId2 islands

                    else
                        islands
            in
            buildAndWarmStart ctx newBody1 solverBodies2 newIslands (equationsGroup :: groups) rest


{-| Post-solve state, wrapped directly as `Types.Contacts`: each body integrated
to its next-frame transform (once here, reused by both the body list and
`contactPoints`), the next-frame warm-start cache, and the frame's `pairGroups`.
-}
type alias SolveResult id =
    { bodies : Array ( id, Body )
    , warmStart : ContactCache Equation.WarmStart
    , iterations : Int
    , pairGroups : List PairGroup
    }


solve : Float -> Vec3 -> Int -> List PairGroup -> Int -> List ( id, Body ) -> ContactCache Equation.WarmStart -> SolveResult id
solve dt gravity iterations pairGroups maxId bodiesWithIds warmStart =
    case bodiesWithIds of
        [] ->
            { bodies = Array.empty
            , warmStart = Cache.empty
            , iterations = 0
            , pairGroups = pairGroups
            }

        ( firstExtId, _ ) :: _ ->
            let
                ctx =
                    { dt = dt
                    , gravity = gravity
                    , gravityLength = Vec3.length gravity
                    , warmStart = warmStart
                    }

                fillingBody =
                    SolverBody.sentinel firstExtId

                solverBodies =
                    SolverBody.fromBodies maxId bodiesWithIds

                -- One fused pass: build equationsGroups, apply warm-start
                -- impulses, and grow the islands. Each island converges
                -- independently, so settled regions exit early.
                ( equationsGroups, warmStartedBodies, islands ) =
                    buildAndWarmStart
                        ctx
                        fillingBody
                        solverBodies
                        (Islands.init maxId)
                        []
                        pairGroups

                -- Solve each island, threading the bodies array; minRem tracks
                -- the fewest iterations any island had left (the iteration report).
                ( finalSolverBodies, finalEquationsGroups, minRemainingIterations ) =
                    Islands.fold
                        (\island ( arr, accGroups, minRem ) ->
                            let
                                ( newArr, newGroups, remIters ) =
                                    solveOneIsland iterations fillingBody island arr accGroups
                            in
                            ( newArr, newGroups, minInt minRem remIters )
                        )
                        ( warmStartedBodies, [], iterations )
                        equationsGroups
                        islands

                iterationsUsed =
                    maxInt 1 (iterations - minRemainingIterations)

                finalWarmStart =
                    collectGroupCaches finalEquationsGroups Cache.empty

                -- Integrate to next-frame transforms once; reused by the output
                -- list and contactPoints.
                integratedBodies =
                    Array.map (SolverBody.solved dt gravity) finalSolverBodies
            in
            { bodies = integratedBodies
            , warmStart = finalWarmStart
            , iterations = iterationsUsed
            , pairGroups = pairGroups
            }


{-| Build next frame's warm-start cache: one insert per contact-bearing pair,
keyed by body-pair key, each entry carrying the normal lambda and friction1
direction. Constraints are never warm-started, so only contacts are collected.
-}
collectGroupCaches : List (EquationsGroup id) -> ContactCache Equation.WarmStart -> ContactCache Equation.WarmStart
collectGroupCaches groups acc =
    case groups of
        [] ->
            acc

        group :: rest ->
            case group.contacts of
                [] ->
                    collectGroupCaches rest acc

                _ :: _ ->
                    let
                        bodyKey =
                            ContactId.bodyKey group.body1.body.id group.body2.body.id
                    in
                    collectGroupCaches rest
                        (Cache.insertGroup bodyKey (warmStartEntries group.contacts []) acc)


{-| A pair's warm-start entries: each contact's solved normal lambda and
friction1 (t1) direction, keyed by the contact id `(shapeKey, featureKey)`.
-}
warmStartEntries : List ContactEquations -> List ( Int, Int, Equation.WarmStart ) -> List ( Int, Int, Equation.WarmStart )
warmStartEntries contacts acc =
    case contacts of
        [] ->
            acc

        { data, normalLambda } :: rest ->
            warmStartEntries rest
                (( data.shapeKey
                 , data.featureKey
                 , { lambda = normalLambda
                   , t1 = { x = data.friction1.vBx, y = data.friction1.vBy, z = data.friction1.vBz }
                   }
                 )
                    :: acc
                )


{-| Solve a multi-body island: two sweeps per iteration — non-friction (normals

  - joints) across the island first, then friction sized off the finalized normal
    lambdas. Penetration recovery folds into the non-friction bias (Baumgarte ERP),
    so there's no separate position pass.

-}
step : Int -> SolverBody id -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
step remainingIterations prevBody1 solverBodies currentEquationsGroups =
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

    else if deltaTot - Const.solverTolerance < 0 then
        ( Array.set pass2.prevBody1.body.id pass2.prevBody1 pass2.solverBodies, forwardResult, remainingIterations - 1 )

    else
        step (remainingIterations - 1) pass2.prevBody1 pass2.solverBodies forwardResult


type alias SweepResult id =
    { prevBody1 : SolverBody id
    , groups : List (EquationsGroup id)
    , solverBodies : Array (SolverBody id)
    , deltalambdaTot : Float
    }


{-| Walk every pair group in the island once in the given phase. Body-threading:
when consecutive groups share body1, skip the Array.get/set round-trip. Groups
accumulate in reverse.
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


{-| Solve one island: a 2-body island skips the array round-trip, anything
larger runs the full island PGS loop. `accGroups` collects the spent groups,
`minRem` tracks the fewest iterations any island had left.
-}
solveOneIsland : Int -> SolverBody id -> List (EquationsGroup id) -> Array (SolverBody id) -> List (EquationsGroup id) -> ( Array (SolverBody id), List (EquationsGroup id), Int )
solveOneIsland iterations fillingBody island arr accGroups =
    case island of
        [ singleGroup ] ->
            -- 2-body island: bodies are owned solely by this group, so use the
            -- SolverBody refs stashed on it directly — no Array.get.
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
solve2Body remainingIterations group arr accGroups =
    let
        nonFriction =
            velocityNonFrictionGroup group.body1 group.body2 0 group.contacts group.constraints

        result =
            velocityFrictionGroup nonFriction.body1 nonFriction.body2 nonFriction.deltalambdaTot nonFriction.contacts nonFriction.constraints
    in
    if remainingIterations == 1 then
        ( flushBody result.body2 (flushBody result.body1 arr), result :: accGroups, 0 )

    else if result.deltalambdaTot - Const.solverTolerance < 0 then
        ( flushBody result.body2 (flushBody result.body1 arr), result :: accGroups, remainingIterations - 1 )

    else
        solve2Body (remainingIterations - 1) result arr accGroups


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
applyVelocityBody1 : Float -> Jacobian -> SolverBody id -> SolverBody id
applyVelocityBody1 deltalambda jacobian body =
    if body.body.kindInt == 2 then
        let
            invI =
                body.body.invInertiaWorld

            k =
                deltalambda * body.body.invMass
        in
        { body = body.body
        , extId = body.extId
        , vX = body.vX - k * jacobian.vBx
        , vY = body.vY - k * jacobian.vBy
        , vZ = body.vZ - k * jacobian.vBz
        , wX = body.wX + (invI.m11 * jacobian.wAx + invI.m12 * jacobian.wAy + invI.m13 * jacobian.wAz) * deltalambda
        , wY = body.wY + (invI.m21 * jacobian.wAx + invI.m22 * jacobian.wAy + invI.m23 * jacobian.wAz) * deltalambda
        , wZ = body.wZ + (invI.m31 * jacobian.wAx + invI.m32 * jacobian.wAy + invI.m33 * jacobian.wAz) * deltalambda
        }

    else
        body


applyVelocityBody2 : Float -> Jacobian -> SolverBody id -> SolverBody id
applyVelocityBody2 deltalambda jacobian body =
    if body.body.kindInt == 2 then
        let
            invI =
                body.body.invInertiaWorld

            k =
                deltalambda * body.body.invMass
        in
        { body = body.body
        , extId = body.extId
        , vX = body.vX + k * jacobian.vBx
        , vY = body.vY + k * jacobian.vBy
        , vZ = body.vZ + k * jacobian.vBz
        , wX = body.wX + (invI.m11 * jacobian.wBx + invI.m12 * jacobian.wBy + invI.m13 * jacobian.wBz) * deltalambda
        , wY = body.wY + (invI.m21 * jacobian.wBx + invI.m22 * jacobian.wBy + invI.m23 * jacobian.wBz) * deltalambda
        , wZ = body.wZ + (invI.m31 * jacobian.wBx + invI.m32 * jacobian.wBy + invI.m33 * jacobian.wBz) * deltalambda
        }

    else
        body


type alias VelocityListResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , equations : List ConstraintEquation
    , deltalambdaTot : Float
    }


type alias VelocityContactsResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , contacts : List ContactEquations
    , deltalambdaTot : Float
    }


{-| Solve a flat list of non-friction equations (constraints/joints) with fixed
[minImpulse, maxImpulse] bounds. Equations come back in input order.
-}
solveVelocityConstraints : SolverBody id -> SolverBody id -> List ConstraintEquation -> Float -> List ConstraintEquation -> VelocityListResult id
solveVelocityConstraints body1 body2 acc deltalambdaTot equations =
    case equations of
        [] ->
            { body1 = body1, body2 = body2, equations = List.reverse acc, deltalambdaTot = deltalambdaTot }

        constraint :: rest ->
            let
                jacobian =
                    constraint.jacobian

                solverLambda =
                    constraint.solverLambda

                gWlambda =
                    -(jacobian.vBx * body1.vX + jacobian.vBy * body1.vY + jacobian.vBz * body1.vZ)
                        + (jacobian.wAx * body1.wX + jacobian.wAy * body1.wY + jacobian.wAz * body1.wZ)
                        + (jacobian.vBx * body2.vX + jacobian.vBy * body2.vY + jacobian.vBz * body2.vZ)
                        + (jacobian.wBx * body2.wX + jacobian.wBy * body2.wY + jacobian.wBz * body2.wZ)

                deltalambdaPrev =
                    constraint.solverInvC * (constraint.solverB - gWlambda - constraint.spookEps * solverLambda)

                deltalambda =
                    if solverLambda + deltalambdaPrev - constraint.minImpulse < 0 then
                        constraint.minImpulse - solverLambda

                    else if solverLambda + deltalambdaPrev - constraint.maxImpulse > 0 then
                        constraint.maxImpulse - solverLambda

                    else
                        deltalambdaPrev
            in
            solveVelocityConstraints
                (applyVelocityBody1 deltalambda jacobian body1)
                (applyVelocityBody2 deltalambda jacobian body2)
                ({ jacobian = jacobian
                 , solverB = constraint.solverB
                 , solverInvC = constraint.solverInvC
                 , spookEps = constraint.spookEps
                 , minImpulse = constraint.minImpulse
                 , maxImpulse = constraint.maxImpulse
                 , solverLambda = solverLambda + deltalambda
                 }
                    :: acc
                )
                (deltalambdaTot + abs deltalambda)
                rest


{-| Pass 1 contact solve: each block's normal with its fixed bounds, frictions
left untouched.
-}
solveVelocityNormals : SolverBody id -> SolverBody id -> List ContactEquations -> Float -> List ContactEquations -> VelocityContactsResult id
solveVelocityNormals body1 body2 acc deltalambdaTot contacts =
    case contacts of
        [] ->
            { body1 = body1, body2 = body2, contacts = List.reverse acc, deltalambdaTot = deltalambdaTot }

        contact :: rest ->
            let
                data =
                    contact.data

                jacobian =
                    data.normal

                solverLambda =
                    contact.normalLambda

                gWlambda =
                    -(jacobian.vBx * body1.vX + jacobian.vBy * body1.vY + jacobian.vBz * body1.vZ)
                        + (jacobian.wAx * body1.wX + jacobian.wAy * body1.wY + jacobian.wAz * body1.wZ)
                        + (jacobian.vBx * body2.vX + jacobian.vBy * body2.vY + jacobian.vBz * body2.vZ)
                        + (jacobian.wBx * body2.wX + jacobian.wBy * body2.wY + jacobian.wBz * body2.wZ)

                deltalambdaPrev =
                    data.normalSolverInvC * (data.normalSolverB - gWlambda - data.spookEps * solverLambda)

                deltalambda =
                    if solverLambda + deltalambdaPrev - data.normalMinImpulse < 0 then
                        data.normalMinImpulse - solverLambda

                    else if solverLambda + deltalambdaPrev - data.normalMaxImpulse > 0 then
                        data.normalMaxImpulse - solverLambda

                    else
                        deltalambdaPrev
            in
            solveVelocityNormals
                (applyVelocityBody1 deltalambda jacobian body1)
                (applyVelocityBody2 deltalambda jacobian body2)
                ({ normalLambda = solverLambda + deltalambda
                 , friction1Lambda = contact.friction1Lambda
                 , friction2Lambda = contact.friction2Lambda
                 , data = data
                 }
                    :: acc
                )
                (deltalambdaTot + abs deltalambda)
                rest


{-| Pass 2 contact solve: each block's two friction equations, the Coulomb cone
±μ·λ\_n sized from the block's finalized normal lambda. friction1 then friction2.
-}
solveVelocityFrictions : SolverBody id -> SolverBody id -> List ContactEquations -> Float -> List ContactEquations -> VelocityContactsResult id
solveVelocityFrictions body1 body2 acc deltalambdaTot contacts =
    case contacts of
        [] ->
            { body1 = body1, body2 = body2, contacts = List.reverse acc, deltalambdaTot = deltalambdaTot }

        contact :: rest ->
            let
                data =
                    contact.data

                normalLambda =
                    contact.normalLambda

                eq1 =
                    data.friction1

                cap1 =
                    data.frictionCoefficient * normalLambda

                gW1 =
                    -(eq1.vBx * body1.vX + eq1.vBy * body1.vY + eq1.vBz * body1.vZ)
                        + (eq1.wAx * body1.wX + eq1.wAy * body1.wY + eq1.wAz * body1.wZ)
                        + (eq1.vBx * body2.vX + eq1.vBy * body2.vY + eq1.vBz * body2.vZ)
                        + (eq1.wBx * body2.wX + eq1.wBy * body2.wY + eq1.wBz * body2.wZ)

                dPrev1 =
                    data.friction1SolverInvC * (data.friction1SolverB - gW1 - data.spookEps * contact.friction1Lambda)

                d1 =
                    if contact.friction1Lambda + dPrev1 + cap1 < 0 then
                        -cap1 - contact.friction1Lambda

                    else if contact.friction1Lambda + dPrev1 - cap1 > 0 then
                        cap1 - contact.friction1Lambda

                    else
                        dPrev1

                invI1 =
                    body1.body.invInertiaWorld

                invI2 =
                    body2.body.invInertiaWorld

                -- friction1's updated velocities as locals (not a throwaway
                -- SolverBody) so friction2's gWlambda can read them. invMass /
                -- invInertiaWorld are 0 for static bodies, so no kindInt guard.
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
                    data.friction2

                cap2 =
                    data.frictionCoefficient * normalLambda

                gW2 =
                    -(eq2.vBx * b1vX + eq2.vBy * b1vY + eq2.vBz * b1vZ)
                        + (eq2.wAx * b1wX + eq2.wAy * b1wY + eq2.wAz * b1wZ)
                        + (eq2.vBx * b2vX + eq2.vBy * b2vY + eq2.vBz * b2vZ)
                        + (eq2.wBx * b2wX + eq2.wBy * b2wY + eq2.wBz * b2wZ)

                dPrev2 =
                    data.friction2SolverInvC * (data.friction2SolverB - gW2 - data.spookEps * contact.friction2Lambda)

                d2 =
                    if contact.friction2Lambda + dPrev2 + cap2 < 0 then
                        -cap2 - contact.friction2Lambda

                    else if contact.friction2Lambda + dPrev2 - cap2 > 0 then
                        cap2 - contact.friction2Lambda

                    else
                        dPrev2

                -- Build each body once, combining friction1 (in b1*/b2*) and
                -- friction2 impulses.
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
            in
            solveVelocityFrictions
                newBody1
                newBody2
                ({ normalLambda = contact.normalLambda
                 , friction1Lambda = contact.friction1Lambda + d1
                 , friction2Lambda = contact.friction2Lambda + d2
                 , data = data
                 }
                    :: acc
                )
                (deltalambdaTot + abs d1 + abs d2)
                rest


{-| Pass 1 over a pair group: constraints first, then contact normals. Frictions
left for pass 2.
-}
velocityNonFrictionGroup : SolverBody id -> SolverBody id -> Float -> List ContactEquations -> List ConstraintEquation -> EquationsGroup id
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
velocityFrictionGroup : SolverBody id -> SolverBody id -> Float -> List ContactEquations -> List ConstraintEquation -> EquationsGroup id
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


type Phase
    = NonFrictionPhase
    | FrictionPhase
