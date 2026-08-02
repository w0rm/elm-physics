module Internal.Solver exposing (solve)

import Array exposing (Array)
import Internal.Body exposing (Body)
import Internal.Const as Const
import Internal.Contact exposing (PairGroup)
import Internal.ContactCache as Cache exposing (ContactCache)
import Internal.ContactId as ContactId
import Internal.Equation as Equation exposing (ConstraintEquation, ContactEquations, EquationsGroup, Jacobian, PointEquation)
import Internal.Islands as Islands exposing (Islands)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 exposing (Vec3)


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


{-| Apply the seeded impulses: each point's normal lambda, and the manifold's
tangent pair and twist. The soft normal rows' impulse bleed keeps the seeds
from pumping, so friction seeds are safe to apply with everything else.
-}
applyContactsWarmStart : SolverBody id -> SolverBody id -> List ContactEquations -> ( SolverBody id, SolverBody id )
applyContactsWarmStart body1 body2 manifolds =
    case manifolds of
        [] ->
            ( body1, body2 )

        manifold :: rest ->
            let
                ( b1n, b2n ) =
                    applyPointsWarmStart body1 body2 manifold.points

                ( b1f, b2f ) =
                    applyEquationWarmStart manifold.friction1Lambda manifold.data.friction1 b1n b2n

                ( b1g, b2g ) =
                    applyEquationWarmStart manifold.friction2Lambda manifold.data.friction2 b1f b2f

                ( b1t, b2t ) =
                    applyEquationWarmStart manifold.twistLambda manifold.data.twist b1g b2g
            in
            applyContactsWarmStart b1t b2t rest


applyPointsWarmStart : SolverBody id -> SolverBody id -> List PointEquation -> ( SolverBody id, SolverBody id )
applyPointsWarmStart body1 body2 points =
    case points of
        [] ->
            ( body1, body2 )

        point :: rest ->
            let
                ( b1n, b2n ) =
                    applyEquationWarmStart point.normalLambda point.data.normal body1 body2
            in
            applyPointsWarmStart b1n b2n rest


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
                    Equation.initCtx dt warmStart

                fillingBody =
                    SolverBody.sentinel firstExtId

                solverBodies =
                    SolverBody.fromBodies dt gravity maxId bodiesWithIds

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

                -- Solve each island, threading the body array;
                -- minRemainingIterations tracks the fewest iterations any
                -- island had left (the iteration report).
                finalState =
                    Islands.fold
                        (solveOneIsland iterations fillingBody)
                        { bodies = warmStartedBodies
                        , groups = []
                        , minRemainingIterations = iterations
                        }
                        equationsGroups
                        islands

                iterationsUsed =
                    maxInt 1 (iterations - finalState.minRemainingIterations)

                finalWarmStart =
                    collectGroupCaches finalState.groups Cache.empty

                -- Integrate to next-frame transforms once; reused by the output
                -- list and contactPoints.
                integratedBodies =
                    Array.map (SolverBody.solved dt) finalState.bodies
            in
            { bodies = integratedBodies
            , warmStart = finalWarmStart
            , iterations = iterationsUsed
            , pairGroups = pairGroups
            }


{-| Build next frame's warm-start cache: one insert per contact-bearing pair,
keyed by body-pair key. Constraints are never warm-started, so only contacts
are collected.
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

                        tangentSign =
                            if group.body1.body.id - group.body2.body.id < 0 then
                                1

                            else
                                -1
                    in
                    collectGroupCaches rest
                        (Cache.insertGroup bodyKey (warmStartEntries tangentSign group.contacts []) acc)


{-| A pair's warm-start entries: each point's solved normal lambda keyed by
`(shapeKey, featureKey)`, plus per manifold the world-space tangent impulse
under `(shapeKey, -1..-3)` (sign-canonicalized to body-id order) and the
twist lambda under `(shapeKey, -4)`.
-}
warmStartEntries : Float -> List ContactEquations -> List ( Int, Int, Equation.WarmStart ) -> List ( Int, Int, Equation.WarmStart )
warmStartEntries tangentSign manifolds acc =
    case manifolds of
        [] ->
            acc

        manifold :: rest ->
            case manifold.points of
                [] ->
                    warmStartEntries tangentSign rest acc

                firstPoint :: _ ->
                    let
                        sk =
                            firstPoint.data.shapeKey

                        t1 =
                            manifold.data.friction1

                        t2 =
                            manifold.data.friction2

                        f1 =
                            manifold.friction1Lambda

                        f2 =
                            manifold.friction2Lambda
                    in
                    warmStartEntries tangentSign
                        rest
                        (pointWarmStartEntries manifold.points
                            (( sk, -1, tangentSign * (f1 * t1.vBx + f2 * t2.vBx) )
                                :: ( sk, -2, tangentSign * (f1 * t1.vBy + f2 * t2.vBy) )
                                :: ( sk, -3, tangentSign * (f1 * t1.vBz + f2 * t2.vBz) )
                                :: ( sk, -4, manifold.twistLambda )
                                :: acc
                            )
                        )


pointWarmStartEntries : List PointEquation -> List ( Int, Int, Equation.WarmStart ) -> List ( Int, Int, Equation.WarmStart )
pointWarmStartEntries points acc =
    case points of
        [] ->
            acc

        { data, normalLambda } :: rest ->
            pointWarmStartEntries rest
                (( data.shapeKey, data.featureKey, normalLambda ) :: acc)


{-| Solve a multi-body island: two sweeps per iteration — non-friction (normals

  - joints) across the island first, then friction sized off the finalized
    normal lambdas. Penetration recovery runs through the soft normal rows'
    bias (no separate position pass); restitution runs once after the
    iterations, from the stored pre-solve approach velocities.

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

                        RestitutionPhase ->
                            restitutionGroup body1 body2 currentGroup.contacts currentGroup.constraints
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


{-| The island fold's accumulator: the body array, the spent groups, and the
fewest iterations any island had left.
-}
type alias SolveAcc id =
    { bodies : Array (SolverBody id)
    , groups : List (EquationsGroup id)
    , minRemainingIterations : Int
    }


{-| Solve one island: a 2-body island skips the array round-trip, anything
larger runs the full island PGS loop; both finish with the restitution pass.
-}
solveOneIsland : Int -> SolverBody id -> List (EquationsGroup id) -> SolveAcc id -> SolveAcc id
solveOneIsland iterations fillingBody island acc =
    if islandWantsSleep island then
        -- Every dynamic body in this connected component wants to sleep, so
        -- skip the velocity solve entirely and stamp the members asleep —
        -- `solved` then holds their poses. A new contact with an awake body
        -- would merge it into this island (union-find), making some member
        -- keep the island awake again, which solves and so wakes it.
        { bodies = markIslandAsleep island acc.bodies
        , groups = island ++ acc.groups
        , minRemainingIterations = acc.minRemainingIterations
        }

    else
        case island of
            [ singleGroup ] ->
                -- 2-body island: bodies are owned solely by this group, so use the
                -- SolverBody refs stashed on it directly — no Array.get.
                solve2Body iterations singleGroup acc

            _ ->
                let
                    ( solvedArr, velocityGroups, remIters ) =
                        step iterations fillingBody acc.bodies island

                    pass =
                        sweep RestitutionPhase fillingBody solvedArr [] velocityGroups 0
                in
                { bodies = Array.set pass.prevBody1.body.id pass.prevBody1 pass.solverBodies
                , groups = List.reverse pass.groups ++ acc.groups
                , minRemainingIterations = minInt acc.minRemainingIterations remIters
                }


{-| An island sleeps only when no group in it forces it awake.
-}
islandWantsSleep : List (EquationsGroup id) -> Bool
islandWantsSleep island =
    case island of
        [] ->
            True

        group :: rest ->
            if groupKeepsIslandAwake group then
                False

            else
                islandWantsSleep rest


{-| A group keeps its island awake if either body does (a settling dynamic or a
moving kinematic), or it carries a user constraint to a non-dynamic body. That
last case covers a static/kinematic anchor repositioned each frame to drive the
dynamic (the mouse-drag demos pin a `static` body to the dragged body with a
`pointToPoint` constraint): the anchor's motion reaches the dynamic only through
the solve, which a sleeping island skips, so it would otherwise sit frozen.
Constraints between two dynamics need no special case — they are unioned into
one island and sleep together once both bodies settle.
-}
groupKeepsIslandAwake : EquationsGroup id -> Bool
groupKeepsIslandAwake group =
    SolverBody.keepsIslandAwake group.body1
        || SolverBody.keepsIslandAwake group.body2
        || (case group.constraints of
                [] ->
                    False

                _ :: _ ->
                    group.body1.body.kindInt /= 2 || group.body2.body.kindInt /= 2
           )


{-| Stamp every dynamic body in a sleeping island with the asleep marker.
-}
markIslandAsleep : List (EquationsGroup id) -> Array (SolverBody id) -> Array (SolverBody id)
markIslandAsleep island arr =
    case island of
        [] ->
            arr

        group :: rest ->
            markIslandAsleep rest (markBodyAsleep group.body2 (markBodyAsleep group.body1 arr))


markBodyAsleep : SolverBody id -> Array (SolverBody id) -> Array (SolverBody id)
markBodyAsleep solverBody arr =
    if solverBody.body.kindInt == 2 then
        Array.set solverBody.body.id (SolverBody.markAsleep solverBody) arr

    else
        arr


{-| Specialized PGS for a 2-body island (single equation group). The two
bodies stay in locals across iterations; only at the end do we write back to
the arrays.
-}
solve2Body : Int -> EquationsGroup id -> SolveAcc id -> SolveAcc id
solve2Body remainingIterations group acc =
    let
        nonFriction =
            velocityNonFrictionGroup group.body1 group.body2 0 group.contacts group.constraints

        result =
            velocityFrictionGroup nonFriction.body1 nonFriction.body2 nonFriction.deltalambdaTot nonFriction.contacts nonFriction.constraints
    in
    if remainingIterations == 1 then
        finish2Body 0 result acc

    else if result.deltalambdaTot - Const.solverTolerance < 0 then
        finish2Body (remainingIterations - 1) result acc

    else
        solve2Body (remainingIterations - 1) result acc


finish2Body : Int -> EquationsGroup id -> SolveAcc id -> SolveAcc id
finish2Body remIters result acc =
    let
        final =
            restitutionGroup result.body1 result.body2 result.contacts result.constraints
    in
    { bodies = flushBody final.body2 (flushBody final.body1 acc.bodies)
    , groups = final :: acc.groups
    , minRemainingIterations = minInt acc.minRemainingIterations remIters
    }


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


{-| Pass 1 contact solve: each manifold's per-point soft normal rows, frictions
left untouched.
-}
solveVelocityNormals : SolverBody id -> SolverBody id -> List ContactEquations -> Float -> List ContactEquations -> VelocityContactsResult id
solveVelocityNormals body1 body2 acc deltalambdaTot manifolds =
    case manifolds of
        [] ->
            { body1 = body1, body2 = body2, contacts = List.reverse acc, deltalambdaTot = deltalambdaTot }

        manifold :: rest ->
            let
                inner =
                    solvePointNormals body1 body2 [] deltalambdaTot manifold.points
            in
            solveVelocityNormals
                inner.body1
                inner.body2
                ({ points = inner.points
                 , friction1Lambda = manifold.friction1Lambda
                 , friction2Lambda = manifold.friction2Lambda
                 , twistLambda = manifold.twistLambda
                 , data = manifold.data
                 }
                    :: acc
                )
                inner.deltalambdaTot
                rest


type alias PointsResult id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , points : List PointEquation
    , deltalambdaTot : Float
    }


{-| Soft normal row update:
`Δλ = -normalMass·(massScale·vn + bias) - impulseScale·λ`, accumulated λ ≥ 0.
-}
solvePointNormals : SolverBody id -> SolverBody id -> List PointEquation -> Float -> List PointEquation -> PointsResult id
solvePointNormals body1 body2 acc deltalambdaTot points =
    case points of
        [] ->
            { body1 = body1, body2 = body2, points = List.reverse acc, deltalambdaTot = deltalambdaTot }

        point :: rest ->
            let
                data =
                    point.data

                jacobian =
                    data.normal

                solverLambda =
                    point.normalLambda

                gWlambda =
                    -(jacobian.vBx * body1.vX + jacobian.vBy * body1.vY + jacobian.vBz * body1.vZ)
                        + (jacobian.wAx * body1.wX + jacobian.wAy * body1.wY + jacobian.wAz * body1.wZ)
                        + (jacobian.vBx * body2.vX + jacobian.vBy * body2.vY + jacobian.vBz * body2.vZ)
                        + (jacobian.wBx * body2.wX + jacobian.wBy * body2.wY + jacobian.wBz * body2.wZ)

                deltalambdaPrev =
                    -data.normalMass * (data.normalMassScale * gWlambda + data.normalBias) - data.normalImpulseScale * solverLambda

                deltalambda =
                    if solverLambda + deltalambdaPrev < 0 then
                        -solverLambda

                    else
                        deltalambdaPrev

                newLambda =
                    solverLambda + deltalambda
            in
            solvePointNormals
                (applyVelocityBody1 deltalambda jacobian body1)
                (applyVelocityBody2 deltalambda jacobian body2)
                ({ normalLambda = newLambda
                 , maxNormalLambda =
                    if newLambda - point.maxNormalLambda > 0 then
                        newLambda

                    else
                        point.maxNormalLambda
                 , data = data
                 }
                    :: acc
                )
                (deltalambdaTot + abs deltalambda)
                rest


{-| Pass 2 contact solve: per manifold, the twist row about the normal, then
the coupled central friction pair. Cones are sized from the manifold's
finalized normal lambdas: the tangent pair clamps to the circle μ·Σλ, the
twist to ±μ·Σ(leverArm·λ) — the torque per-point tangent forces could produce.
-}
solveVelocityFrictions : SolverBody id -> SolverBody id -> List ContactEquations -> Float -> List ContactEquations -> VelocityContactsResult id
solveVelocityFrictions bodyA bodyB acc deltalambdaTot manifolds =
    case manifolds of
        [] ->
            { body1 = bodyA, body2 = bodyB, contacts = List.reverse acc, deltalambdaTot = deltalambdaTot }

        manifold :: rest ->
            let
                data =
                    manifold.data

                caps =
                    coulombCaps manifold.points 0 0

                twist =
                    data.twist

                twistCap =
                    data.frictionCoefficient * caps.lever

                -- the twist row has no linear part
                gWt =
                    (twist.wAx * bodyA.wX + twist.wAy * bodyA.wY + twist.wAz * bodyA.wZ)
                        + (twist.wBx * bodyB.wX + twist.wBy * bodyB.wY + twist.wBz * bodyB.wZ)

                dPrevT =
                    -data.twistMass * gWt

                dT =
                    if manifold.twistLambda + dPrevT + twistCap < 0 then
                        -twistCap - manifold.twistLambda

                    else if manifold.twistLambda + dPrevT - twistCap > 0 then
                        twistCap - manifold.twistLambda

                    else
                        dPrevT

                invI1 =
                    bodyA.body.invInertiaWorld

                invI2 =
                    bodyB.body.invInertiaWorld

                -- twist's updated angular velocities as locals so the tangent
                -- rows read them; invInertiaWorld is 0 for static bodies, so
                -- no kindInt guard.
                b1wX =
                    bodyA.wX + (invI1.m11 * twist.wAx + invI1.m12 * twist.wAy + invI1.m13 * twist.wAz) * dT

                b1wY =
                    bodyA.wY + (invI1.m21 * twist.wAx + invI1.m22 * twist.wAy + invI1.m23 * twist.wAz) * dT

                b1wZ =
                    bodyA.wZ + (invI1.m31 * twist.wAx + invI1.m32 * twist.wAy + invI1.m33 * twist.wAz) * dT

                b2wX =
                    bodyB.wX + (invI2.m11 * twist.wBx + invI2.m12 * twist.wBy + invI2.m13 * twist.wBz) * dT

                b2wY =
                    bodyB.wY + (invI2.m21 * twist.wBx + invI2.m22 * twist.wBy + invI2.m23 * twist.wBz) * dT

                b2wZ =
                    bodyB.wZ + (invI2.m31 * twist.wBx + invI2.m32 * twist.wBy + invI2.m33 * twist.wBz) * dT

                eq1 =
                    data.friction1

                eq2 =
                    data.friction2

                gW1 =
                    -(eq1.vBx * bodyA.vX + eq1.vBy * bodyA.vY + eq1.vBz * bodyA.vZ)
                        + (eq1.wAx * b1wX + eq1.wAy * b1wY + eq1.wAz * b1wZ)
                        + (eq1.vBx * bodyB.vX + eq1.vBy * bodyB.vY + eq1.vBz * bodyB.vZ)
                        + (eq1.wBx * b2wX + eq1.wBy * b2wY + eq1.wBz * b2wZ)

                gW2 =
                    -(eq2.vBx * bodyA.vX + eq2.vBy * bodyA.vY + eq2.vBz * bodyA.vZ)
                        + (eq2.wAx * b1wX + eq2.wAy * b1wY + eq2.wAz * b1wZ)
                        + (eq2.vBx * bodyB.vX + eq2.vBy * bodyB.vY + eq2.vBz * bodyB.vZ)
                        + (eq2.wBx * b2wX + eq2.wBy * b2wY + eq2.wBz * b2wZ)

                -- coupled 2x2 solve, then a circular Coulomb clamp on the
                -- accumulated tangent impulse
                new1 =
                    manifold.friction1Lambda - (data.tangentInv11 * gW1 + data.tangentInv12 * gW2)

                new2 =
                    manifold.friction2Lambda - (data.tangentInv12 * gW1 + data.tangentInv22 * gW2)

                cap =
                    data.frictionCoefficient * caps.total

                lenSq =
                    new1 * new1 + new2 * new2

                scale =
                    if lenSq - cap * cap > 0 then
                        cap / sqrt lenSq

                    else
                        1

                d1 =
                    new1 * scale - manifold.friction1Lambda

                d2 =
                    new2 * scale - manifold.friction2Lambda

                -- both tangent impulses combined into one application per body
                cVx =
                    d1 * eq1.vBx + d2 * eq2.vBx

                cVy =
                    d1 * eq1.vBy + d2 * eq2.vBy

                cVz =
                    d1 * eq1.vBz + d2 * eq2.vBz

                cAx =
                    d1 * eq1.wAx + d2 * eq2.wAx

                cAy =
                    d1 * eq1.wAy + d2 * eq2.wAy

                cAz =
                    d1 * eq1.wAz + d2 * eq2.wAz

                cBx =
                    d1 * eq1.wBx + d2 * eq2.wBx

                cBy =
                    d1 * eq1.wBy + d2 * eq2.wBy

                cBz =
                    d1 * eq1.wBz + d2 * eq2.wBz

                newBody1 =
                    if bodyA.body.kindInt == 2 then
                        { body = bodyA.body
                        , extId = bodyA.extId
                        , vX = bodyA.vX - bodyA.body.invMass * cVx
                        , vY = bodyA.vY - bodyA.body.invMass * cVy
                        , vZ = bodyA.vZ - bodyA.body.invMass * cVz
                        , wX = b1wX + (invI1.m11 * cAx + invI1.m12 * cAy + invI1.m13 * cAz)
                        , wY = b1wY + (invI1.m21 * cAx + invI1.m22 * cAy + invI1.m23 * cAz)
                        , wZ = b1wZ + (invI1.m31 * cAx + invI1.m32 * cAy + invI1.m33 * cAz)
                        }

                    else
                        bodyA

                newBody2 =
                    if bodyB.body.kindInt == 2 then
                        { body = bodyB.body
                        , extId = bodyB.extId
                        , vX = bodyB.vX + bodyB.body.invMass * cVx
                        , vY = bodyB.vY + bodyB.body.invMass * cVy
                        , vZ = bodyB.vZ + bodyB.body.invMass * cVz
                        , wX = b2wX + (invI2.m11 * cBx + invI2.m12 * cBy + invI2.m13 * cBz)
                        , wY = b2wY + (invI2.m21 * cBx + invI2.m22 * cBy + invI2.m23 * cBz)
                        , wZ = b2wZ + (invI2.m31 * cBx + invI2.m32 * cBy + invI2.m33 * cBz)
                        }

                    else
                        bodyB
            in
            solveVelocityFrictions
                newBody1
                newBody2
                ({ points = manifold.points
                 , friction1Lambda = manifold.friction1Lambda + d1
                 , friction2Lambda = manifold.friction2Lambda + d2
                 , twistLambda = manifold.twistLambda + dT
                 , data = data
                 }
                    :: acc
                )
                (deltalambdaTot + abs dT + abs d1 + abs d2)
                rest


{-| A manifold's Coulomb cone inputs: the summed normal lambdas and the
leverArm-weighted sum.
-}
coulombCaps : List PointEquation -> Float -> Float -> { total : Float, lever : Float }
coulombCaps points total lever =
    case points of
        [] ->
            { total = total, lever = lever }

        point :: rest ->
            coulombCaps rest (total + point.normalLambda) (lever + point.normalLambda * point.data.leverArm)


{-| Restitution pass, once per frame after the iterations: for points whose
pre-solve approach speed exceeded the threshold (and that actually collided),
push the normal velocity toward `-e·approach`. Keeping bounce out of the
biased rows makes it exact instead of fighting the penetration bias.
-}
restitutionGroup : SolverBody id -> SolverBody id -> List ContactEquations -> List ConstraintEquation -> EquationsGroup id
restitutionGroup body1 body2 contacts constraints =
    let
        result =
            restitutionManifolds body1 body2 [] contacts
    in
    { body1 = result.body1
    , body2 = result.body2
    , contacts = result.contacts
    , constraints = constraints
    , deltalambdaTot = 0
    }


restitutionManifolds : SolverBody id -> SolverBody id -> List ContactEquations -> List ContactEquations -> VelocityContactsResult id
restitutionManifolds body1 body2 acc manifolds =
    case manifolds of
        [] ->
            { body1 = body1, body2 = body2, contacts = List.reverse acc, deltalambdaTot = 0 }

        manifold :: rest ->
            if manifold.data.bounciness > 0 then
                let
                    inner =
                        restitutionPoints manifold.data.bounciness body1 body2 [] manifold.points
                in
                restitutionManifolds
                    inner.body1
                    inner.body2
                    ({ points = inner.points
                     , friction1Lambda = manifold.friction1Lambda
                     , friction2Lambda = manifold.friction2Lambda
                     , twistLambda = manifold.twistLambda
                     , data = manifold.data
                     }
                        :: acc
                    )
                    rest

            else
                restitutionManifolds body1 body2 (manifold :: acc) rest


restitutionPoints : Float -> SolverBody id -> SolverBody id -> List PointEquation -> List PointEquation -> PointsResult id
restitutionPoints bounciness body1 body2 acc points =
    case points of
        [] ->
            { body1 = body1, body2 = body2, points = List.reverse acc, deltalambdaTot = 0 }

        point :: rest ->
            let
                data =
                    point.data
            in
            if data.relativeVelocity + Equation.restitutionThreshold > 0 || point.maxNormalLambda == 0 then
                restitutionPoints bounciness body1 body2 (point :: acc) rest

            else
                let
                    jacobian =
                        data.normal

                    solverLambda =
                        point.normalLambda

                    gWlambda =
                        -(jacobian.vBx * body1.vX + jacobian.vBy * body1.vY + jacobian.vBz * body1.vZ)
                            + (jacobian.wAx * body1.wX + jacobian.wAy * body1.wY + jacobian.wAz * body1.wZ)
                            + (jacobian.vBx * body2.vX + jacobian.vBy * body2.vY + jacobian.vBz * body2.vZ)
                            + (jacobian.wBx * body2.wX + jacobian.wBy * body2.wY + jacobian.wBz * body2.wZ)

                    deltalambdaPrev =
                        -data.normalMass * (gWlambda + bounciness * data.relativeVelocity)

                    deltalambda =
                        if solverLambda + deltalambdaPrev < 0 then
                            -solverLambda

                        else
                            deltalambdaPrev
                in
                restitutionPoints bounciness
                    (applyVelocityBody1 deltalambda jacobian body1)
                    (applyVelocityBody2 deltalambda jacobian body2)
                    ({ normalLambda = solverLambda + deltalambda
                     , maxNormalLambda = point.maxNormalLambda
                     , data = data
                     }
                        :: acc
                    )
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
    | RestitutionPhase
