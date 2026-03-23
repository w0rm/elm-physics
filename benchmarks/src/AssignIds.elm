module AssignIds exposing (main)

{-| Benchmarks for the ID-assignment pass in Physics.simulate.

Both candidates use the same four-phase structure:

  - Collect: scan bodies once, prepend existing IDs (O(1) each), count id=-1.
  - Sort: List.sort once (native JS sort, O(n log n)).
  - Analyse: single scan of sorted IDs to find duplicate IDs and free IDs.
  - Assign: scan bodies, hand out pre-computed free IDs to id=-1 and duplicates.

twoPassOld — assign phase tracks claimed dups with two lists:
dupIds (fixed) + claimedDups (grows). Two List.member calls in the dup branch.

twoPass — assign phase uses findAndRemove on a single remainingDups list:
one pass checks membership and removes in one go; allDupIds (fixed, tiny)
only consulted for the rare second-occurrence case.

Three scenarios:

  - stable — all bodies already have IDs (the common per-frame case)
  - allNew — all bodies have id = -1 (first frame / full restart)
  - withGaps — new bodies prepended (::) before stable even-ID bodies

-}

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Body as InternalBody
import Internal.Material as Material
import Internal.Matrix3 as Mat3
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


main : BenchmarkProgram
main =
    program <|
        describe "AssignIds"
            [ Benchmark.compare "stable"
                "twoPass"
                (\_ -> assignIdsTwoPass stableInput)
                "twoPassFast"
                (\_ -> assignIdsTwoPassFast stableInput)
            , Benchmark.compare "allNew"
                "twoPass"
                (\_ -> assignIdsTwoPass allNewInput)
                "twoPassFast"
                (\_ -> assignIdsTwoPassFast allNewInput)
            , Benchmark.compare "withGaps"
                "twoPass"
                (\_ -> assignIdsTwoPass withGapsInput)
                "twoPassFast"
                (\_ -> assignIdsTwoPassFast withGapsInput)
            , Benchmark.compare "withDups"
                "twoPass"
                (\_ -> assignIdsTwoPass withDupsInput)
                "twoPassFast"
                (\_ -> assignIdsTwoPassFast withDupsInput)
            , Benchmark.compare "withDupsAndNew"
                "twoPass"
                (\_ -> assignIdsTwoPass withDupsAndNewInput)
                "twoPassFast"
                (\_ -> assignIdsTwoPassFast withDupsAndNewInput)
            ]



-- ── Test data ────────────────────────────────────────────────────────────────


emptyBody : Int -> InternalBody.Body
emptyBody id =
    { id = id
    , material = Material.default
    , transform3d = Transform3d.atOrigin
    , centerOfMassTransform3d = Transform3d.atOrigin
    , velocity = Vec3.zero
    , angularVelocity = Vec3.zero
    , mass = 0
    , volume = 0
    , shapes = []
    , worldShapes = []
    , force = Vec3.zero
    , torque = Vec3.zero
    , boundingSphereRadius = 0
    , linearDamping = 0
    , angularDamping = 0
    , invMass = 0
    , invInertia = Mat3.zero
    , invInertiaWorld = Mat3.zero
    }


{-| 125 bodies, all with IDs 0–124 — the steady-state per-frame case.
-}
stableInput : List ( Int, InternalBody.Protected )
stableInput =
    List.map (\i -> ( i, InternalBody.Protected (emptyBody i) )) (List.range 0 124)


{-| 125 bodies, all with id = -1 — first frame or full restart.
-}
allNewInput : List ( Int, InternalBody.Protected )
allNewInput =
    List.map (\i -> ( i, InternalBody.Protected (emptyBody -1) )) (List.range 0 124)


{-| 62 new bodies (id = -1) prepended to 63 stable bodies with IDs 0, 2, 4, …, 124.
Reflects the realistic usage pattern: new bodies are added with (::) to the front.
-}
withGapsInput : List ( Int, InternalBody.Protected )
withGapsInput =
    List.map (\i -> ( 200 + i, InternalBody.Protected (emptyBody -1) )) (List.range 0 61)
        ++ List.map (\i -> ( 2 * i, InternalBody.Protected (emptyBody (2 * i)) )) (List.range 0 62)


{-| 20 extra bodies with duplicate IDs 0–19 appended to 105 stable bodies with IDs 0–104.
Models a scenario where some bodies are re-added without clearing their old IDs.
-}
withDupsInput : List ( Int, InternalBody.Protected )
withDupsInput =
    List.map (\i -> ( i, InternalBody.Protected (emptyBody i) )) (List.range 0 104)
        ++ List.map (\i -> ( 200 + i, InternalBody.Protected (emptyBody i) )) (List.range 0 19)


{-| 20 duplicates of IDs 0–19, 20 new bodies (id = -1), and 85 stable bodies with IDs 0–84.
Exercises the assign phase with all three body kinds at once.
-}
withDupsAndNewInput : List ( Int, InternalBody.Protected )
withDupsAndNewInput =
    List.map (\i -> ( 300 + i, InternalBody.Protected (emptyBody i) )) (List.range 0 19)
        ++ List.map (\i -> ( 200 + i, InternalBody.Protected (emptyBody -1) )) (List.range 0 19)
        ++ List.map (\i -> ( i, InternalBody.Protected (emptyBody i) )) (List.range 0 84)



-- ── twoPassOld ───────────────────────────────────────────────────────────────
-- Same as twoPass but uses two lists in the assign phase:
-- dupIds (all dup IDs, fixed) + claimedDups (grows as first occurrences are seen).


assignIdsTwoPassOld :
    List ( id, InternalBody.Protected )
    -> ( List ( id, InternalBody.Body ), Int )
assignIdsTwoPassOld bodies =
    let
        ( existingIds, newCount, mx ) =
            tpCollect bodies [] 0 -1

        sorted =
            List.sort existingIds

        ( dupIds, dupCount ) =
            tpFindDups sorted -2 [] 0

        freeIds =
            tpFreeIds 0 sorted (newCount + dupCount) []
    in
    tpAssignOld bodies freeIds dupIds [] mx []


tpAssignOld :
    List ( id, InternalBody.Protected )
    -> List Int
    -> List Int
    -> List Int
    -> Int
    -> List ( id, InternalBody.Body )
    -> ( List ( id, InternalBody.Body ), Int )
tpAssignOld bodies freeIds dupIds claimedDups mx acc =
    case bodies of
        [] ->
            ( acc, mx )

        ( extId, InternalBody.Protected body ) :: rest ->
            if body.id == -1 then
                case freeIds of
                    freshId :: remainingFree ->
                        tpAssignOld rest
                            remainingFree
                            dupIds
                            claimedDups
                            (max mx freshId)
                            (( extId, withId freshId body ) :: acc)

                    [] ->
                        tpAssignOld rest freeIds dupIds claimedDups mx acc

            else if not (List.member body.id dupIds) then
                tpAssignOld rest freeIds dupIds claimedDups mx (( extId, body ) :: acc)

            else if List.member body.id claimedDups then
                case freeIds of
                    freshId :: remainingFree ->
                        tpAssignOld rest
                            remainingFree
                            dupIds
                            claimedDups
                            (max mx freshId)
                            (( extId, withId freshId body ) :: acc)

                    [] ->
                        tpAssignOld rest freeIds dupIds claimedDups mx acc

            else
                tpAssignOld rest
                    freeIds
                    dupIds
                    (body.id :: claimedDups)
                    mx
                    (( extId, body ) :: acc)



-- ── twoPass ──────────────────────────────────────────────────────────────────
--
-- Phase 1 — tpCollect: one scan, O(1) per body.
--   Builds existingIds (unsorted, prepend only) and counts id=-1 bodies.
--
-- Sort — List.sort: native JS sort, O(n log n).
--
-- Phase 2a — tpFindDups: one scan of sorted, O(1) per element.
--   Detects duplicate IDs using a prev/prevAdded flag — no inner lookups.
--
-- Phase 2b — tpFreeIds: one scan of sorted, O(1) per element.
--   Finds the first (newCount + dupCount) gaps by merging counter with sorted.
--
-- Phase 3 — tpAssign: one scan of bodies, O(|dupIds|) per body.
--   dupIds is a multiset (one slot per replacement needed).
--   List.member finds the slot; removeFirst removes it in the same pass.
--   dupIds is empty for stable, so the common case has zero membership tests.


assignIdsTwoPass :
    List ( id, InternalBody.Protected )
    -> ( List ( id, InternalBody.Body ), Int )
assignIdsTwoPass bodies =
    let
        ( existingIds, newCount, mx ) =
            tpCollect bodies [] 0 -1

        sorted =
            List.sort existingIds

        ( dupIds, dupCount ) =
            tpFindDups sorted -2 [] 0

        freeIds =
            tpFreeIds 0 sorted (newCount + dupCount) []
    in
    tpAssign bodies freeIds dupIds -1 mx []


{-| Pass 1: collect existing (non -1) IDs, count new bodies, track max id.
-}
tpCollect :
    List ( id, InternalBody.Protected )
    -> List Int
    -> Int
    -> Int
    -> ( List Int, Int, Int )
tpCollect bodies existingIds newCount mx =
    case bodies of
        [] ->
            ( existingIds, newCount, mx )

        ( _, InternalBody.Protected body ) :: rest ->
            if body.id == -1 then
                tpCollect rest existingIds (newCount + 1) mx

            else
                tpCollect rest (body.id :: existingIds) newCount (max mx body.id)


{-| Scan sorted IDs, adding one entry per extra occurrence of each ID.
Duplicate → one entry. Triplicate → two entries. And so on.
Result is sorted descending — a natural consequence of prepending during an
ascending scan. Called with prev=-2 (impossible value) as initial sentinel.
-}
tpFindDups : List Int -> Int -> List Int -> Int -> ( List Int, Int )
tpFindDups sorted prev acc count =
    case sorted of
        [] ->
            ( acc, count )

        x :: rest ->
            if x - prev == 0 then
                tpFindDups rest x (x :: acc) (count + 1)

            else
                tpFindDups rest x acc count


{-| Collect the first `needed` integers not present in sortedIds, starting from n.
Advances n past each taken slot; skips elements of sortedIds below n.
Returns results in ascending order via reversed accumulator.
-}
tpFreeIds : Int -> List Int -> Int -> List Int -> List Int
tpFreeIds n sorted needed revAcc =
    if needed == 0 then
        List.reverse revAcc

    else
        case sorted of
            [] ->
                tpFillFrom n needed revAcc

            x :: rest ->
                if x > n then
                    -- n is free
                    tpFreeIds (n + 1) sorted (needed - 1) (n :: revAcc)

                else if x == n then
                    -- n is taken
                    tpFreeIds (n + 1) rest needed revAcc

                else
                    -- x < n, stale entry — skip
                    tpFreeIds n rest needed revAcc


tpFillFrom : Int -> Int -> List Int -> List Int
tpFillFrom n needed revAcc =
    if needed == 0 then
        List.reverse revAcc

    else
        tpFillFrom (n + 1) (needed - 1) (n :: revAcc)


{-| Pass 3: assign free IDs to bodies that need them.

dupIds is a sorted multiset (descending initially, flipping on each removal).
memberSorted does an early-exit scan; removeFirstReversing removes the first
occurrence in one TCO pass that naturally reverses the list as a byproduct.
isAscending tracks the current direction so memberSorted exits the right way.
When dupIds is empty (the common stable case) both calls are skipped entirely.

-}
tpAssign :
    List ( id, InternalBody.Protected )
    -> List Int
    -> List Int
    -> Int
    -> Int
    -> List ( id, InternalBody.Body )
    -> ( List ( id, InternalBody.Body ), Int )
tpAssign bodies freeIds dupIds dir mx acc =
    case bodies of
        [] ->
            ( acc, mx )

        ( extId, InternalBody.Protected body ) :: rest ->
            if body.id == -1 then
                case freeIds of
                    freshId :: remainingFree ->
                        tpAssign rest
                            remainingFree
                            dupIds
                            dir
                            (max mx freshId)
                            (( extId, withId freshId body ) :: acc)

                    [] ->
                        tpAssign rest freeIds dupIds dir mx acc

            else
                case dupIds of
                    [] ->
                        tpAssign rest freeIds [] dir mx (( extId, body ) :: acc)

                    _ ->
                        if memberSorted dir body.id dupIds then
                            case freeIds of
                                freshId :: remainingFree ->
                                    tpAssign rest
                                        remainingFree
                                        (removeFirstReversing body.id [] dupIds)
                                        (negate dir)
                                        (max mx freshId)
                                        (( extId, withId freshId body ) :: acc)

                                [] ->
                                    tpAssign rest
                                        freeIds
                                        (removeFirstReversing body.id [] dupIds)
                                        (negate dir)
                                        mx
                                        acc

                        else
                            tpAssign rest freeIds dupIds dir mx (( extId, body ) :: acc)


withId : Int -> InternalBody.Body -> InternalBody.Body
withId freshId body =
    { id = freshId, material = body.material, transform3d = body.transform3d, centerOfMassTransform3d = body.centerOfMassTransform3d, velocity = body.velocity, angularVelocity = body.angularVelocity, mass = body.mass, volume = body.volume, shapes = body.shapes, worldShapes = body.worldShapes, force = body.force, torque = body.torque, boundingSphereRadius = body.boundingSphereRadius, linearDamping = body.linearDamping, angularDamping = body.angularDamping, invMass = body.invMass, invInertia = body.invInertia, invInertiaWorld = body.invInertiaWorld }


memberSorted : Int -> Int -> List Int -> Bool
memberSorted dir x list =
    case list of
        [] ->
            False

        y :: rest ->
            if y - x == 0 then
                True

            else if dir * (y - x) > 0 then
                False

            else
                memberSorted dir x rest


{-| Remove the first occurrence of x and reverse the list in one TCO pass.
When x is found, skip it and accumulate the rest into acc — no prependReversed needed.
-}
removeFirstReversing : Int -> List Int -> List Int -> List Int
removeFirstReversing x acc remaining =
    case remaining of
        [] ->
            acc

        y :: rest ->
            if y == x then
                accumulate acc rest

            else
                removeFirstReversing x (y :: acc) rest


accumulate : List Int -> List Int -> List Int
accumulate acc remaining =
    case remaining of
        [] ->
            acc

        y :: rest ->
            accumulate (y :: acc) rest



-- ── twoPassFast ───────────────────────────────────────────────────────────────
-- Same as twoPass but switches to tpAssignNoDups once dupIds is exhausted,
-- eliminating the case dupIds and dir checks for all remaining bodies.


assignIdsTwoPassFast :
    List ( id, InternalBody.Protected )
    -> ( List ( id, InternalBody.Body ), Int )
assignIdsTwoPassFast bodies =
    let
        ( existingIds, newCount, mx ) =
            tpCollect bodies [] 0 -1

        sorted =
            List.sort existingIds

        ( dupIds, dupCount ) =
            tpFindDups sorted -2 [] 0

        freeIds =
            tpFreeIds 0 sorted (newCount + dupCount) []
    in
    case freeIds of
        [] ->
            tpAssignStable bodies mx []

        _ ->
            tpAssignFast bodies freeIds dupIds -1 mx []


tpAssignFast :
    List ( id, InternalBody.Protected )
    -> List Int
    -> List Int
    -> Int
    -> Int
    -> List ( id, InternalBody.Body )
    -> ( List ( id, InternalBody.Body ), Int )
tpAssignFast bodies freeIds dupIds dir mx acc =
    case freeIds of
        [] ->
            tpAssignStable bodies mx acc

        freshId :: remainingFree ->
            case bodies of
                [] ->
                    ( acc, mx )

                ( extId, InternalBody.Protected body ) :: rest ->
                    if body.id == -1 then
                        tpAssignFast rest
                            remainingFree
                            dupIds
                            dir
                            (max mx freshId)
                            (( extId, withId freshId body ) :: acc)

                    else if memberSorted dir body.id dupIds then
                        case removeFirstReversing body.id [] dupIds of
                            [] ->
                                tpAssignFast rest
                                    remainingFree
                                    []
                                    dir
                                    (max mx freshId)
                                    (( extId, withId freshId body ) :: acc)

                            newDupIds ->
                                tpAssignFast rest
                                    remainingFree
                                    newDupIds
                                    (negate dir)
                                    (max mx freshId)
                                    (( extId, withId freshId body ) :: acc)

                    else
                        tpAssignFast rest freeIds dupIds dir mx (( extId, body ) :: acc)


tpAssignStable :
    List ( id, InternalBody.Protected )
    -> Int
    -> List ( id, InternalBody.Body )
    -> ( List ( id, InternalBody.Body ), Int )
tpAssignStable bodies mx acc =
    case bodies of
        [] ->
            ( acc, mx )

        ( extId, InternalBody.Protected body ) :: rest ->
            tpAssignStable rest mx (( extId, body ) :: acc)
