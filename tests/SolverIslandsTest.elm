module SolverIslandsTest exposing (withinIslandOrder)

{-| Regression test for the within-island processing order.

PGS needs pairs visited bottom-first within each island (floor-contact
first, then up the stack). That order is delivered by a chain of
cons-accumulators (`buildAndWarmStart`, `annotateGroupsByRoot`,
`collectAndSolveIsland`) and a stable `List.sortBy` — no explicit sort
key carries it. If a future refactor adds/drops a `List.reverse` or sort
stability changes, stack convergence will silently regress.

The test re-implements `collectAndSolveIsland`'s island-grouping (a
4-line cons walk) so it can observe what `step` would actually receive,
without running the solver itself.

-}

import Array exposing (Array)
import Expect
import Fuzz exposing (Fuzzer)
import Internal.Equation exposing (EquationsGroup)
import Internal.Solver as Solver
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Set
import Test exposing (Test, describe, fuzz2, test)


type alias Vec3 =
    { x : Float, y : Float, z : Float }


fakeBody : Int -> Int -> SolverBody ()
fakeBody bodyId kindInt =
    let
        b =
            SolverBody.sentinel ()

        body =
            b.body
    in
    { b | body = { body | id = bodyId, kindInt = kindInt } }


dynamicKind : Int
dynamicKind =
    2


staticKind : Int
staticKind =
    1


fakeGroup : ( Int, Int ) -> ( Int, Int ) -> EquationsGroup ()
fakeGroup ( id1, kind1 ) ( id2, kind2 ) =
    { body1 = fakeBody id1 kind1
    , body2 = fakeBody id2 kind2
    , contacts = []
    , constraints = []
    , deltalambdaTot = 0
    }


{-| Gravity projection: lower value = "more bottom" along gravity.
Matches the sign convention in `Physics.elm`.
-}
projection : Vec3 -> Vec3 -> Float
projection gravity p =
    -(p.x * gravity.x + p.y * gravity.y + p.z * gravity.z)


{-| All pairs (i, j) with lo <= i < j < hi, in CSR order. This is the
order `buildAndWarmStart` outputs after broadphase's reverse-cons.
-}
pairsInRange : Int -> Int -> List ( Int, Int )
pairsInRange lo hi =
    List.concatMap
        (\i -> List.map (\j -> ( i, j )) (List.range (i + 1) (hi - 1)))
        (List.range lo (hi - 2))


{-| Mimic `collectAndSolveIsland`: walk a root-sorted list and cons-extend
each contiguous-root run into a sublist, returning islands in step-visit
order.
-}
collectIslands : List ( Int, a ) -> List (List a)
collectIslands sorted =
    case sorted of
        [] ->
            []

        ( firstRoot, firstItem ) :: rest ->
            collectIslandsHelp firstRoot [ firstItem ] [] rest


collectIslandsHelp : Int -> List a -> List (List a) -> List ( Int, a ) -> List (List a)
collectIslandsHelp currentRoot currentIsland acc remaining =
    case remaining of
        [] ->
            List.reverse (currentIsland :: acc)

        ( root, item ) :: rest ->
            if root == currentRoot then
                collectIslandsHelp currentRoot (item :: currentIsland) acc rest

            else
                collectIslandsHelp root [ item ] (currentIsland :: acc) rest


pipelineVisits : Array Int -> List (EquationsGroup ()) -> List (List ( Int, Int ))
pipelineVisits parents groups =
    Solver.annotateGroupsByRoot parents groups []
        |> List.sortBy Tuple.first
        |> collectIslands
        |> List.map (List.map (\g -> ( g.body1.body.id, g.body2.body.id )))


fuzzVec3 : Fuzzer Vec3
fuzzVec3 =
    Fuzz.map3 (\x y z -> { x = x, y = y, z = z })
        (Fuzz.floatRange -5 5)
        (Fuzz.floatRange -5 5)
        (Fuzz.floatRange -5 5)


fuzzGravity : Fuzzer Vec3
fuzzGravity =
    Fuzz.filter
        (\v -> v.x * v.x + v.y * v.y + v.z * v.z > 0.5)
        fuzzVec3


{-| At least 4 so a split into two islands gives each at least 2 bodies
(and therefore at least one pair).
-}
fuzzPositions : Fuzzer (List Vec3)
fuzzPositions =
    Fuzz.intRange 4 8
        |> Fuzz.andThen (\n -> Fuzz.listOfLength n fuzzVec3)


isNonDecreasing : List Float -> Bool
isNonDecreasing list =
    case list of
        [] ->
            True

        first :: rest ->
            List.foldl
                (\x ( ok, prev ) -> ( ok && x >= prev, x ))
                ( True, first )
                rest
                |> Tuple.first


withinIslandOrder : Test
withinIslandOrder =
    describe "Solver within-island ordering"
        [ test "scripted: static floor + 3-box stack + side island" <|
            \_ ->
                let
                    -- buildAndWarmStart output is in CSR order (bottom-first
                    -- by body1 id): floor-b1, b1-b2, b1-b3, b2-b3, then a
                    -- separate b5-b6 island.
                    groups =
                        [ fakeGroup ( 0, staticKind ) ( 1, dynamicKind )
                        , fakeGroup ( 1, dynamicKind ) ( 2, dynamicKind )
                        , fakeGroup ( 1, dynamicKind ) ( 3, dynamicKind )
                        , fakeGroup ( 2, dynamicKind ) ( 3, dynamicKind )
                        , fakeGroup ( 5, dynamicKind ) ( 6, dynamicKind )
                        ]

                    -- Post-union-find: {1,2,3} → root 1; {5,6} → root 5.
                    parents =
                        Array.fromList [ 0, 1, 1, 1, 4, 5, 5 ]
                in
                pipelineVisits parents groups
                    |> Expect.equal
                        [ [ ( 0, 1 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ) ]
                        , [ ( 5, 6 ) ]
                        ]
        , fuzz2 fuzzGravity fuzzPositions "step visits pairs bottom-first by gravity projection within each of two islands" <|
            \gravity positions ->
                let
                    n =
                        List.length positions

                    -- Mimic Physics.elm's gravity sort: sort positions
                    -- by projection, then assign id = sort index. id 0
                    -- is the bottom-most position, id (n-1) the top.
                    positionByNewId =
                        positions
                            |> List.sortBy (projection gravity)
                            |> Array.fromList

                    -- Split ids into two islands: [0, split) and [split, n).
                    -- All-pairs within each island, no inter-island pairs.
                    split =
                        n // 2

                    pairs =
                        pairsInRange 0 split ++ pairsInRange split n

                    groups =
                        List.map
                            (\( a, b ) ->
                                fakeGroup ( a, dynamicKind ) ( b, dynamicKind )
                            )
                            pairs

                    parents =
                        Array.initialize n
                            (\i ->
                                if i < split then
                                    0

                                else
                                    split
                            )

                    visited =
                        pipelineVisits parents groups

                    body1Projection ( id1, _ ) =
                        Array.get id1 positionByNewId
                            |> Maybe.map (projection gravity)
                            |> Maybe.withDefault 0

                    eachIslandBottomFirst =
                        visited
                            |> List.all
                                (\island ->
                                    island
                                        |> List.map body1Projection
                                        |> isNonDecreasing
                                )

                    islandBodyIdSets =
                        visited
                            |> List.map
                                (List.concatMap (\( a, b ) -> [ a, b ])
                                    >> Set.fromList
                                )

                    bodyIdsDisjoint =
                        List.sum (List.map Set.size islandBodyIdSets)
                            == Set.size (List.foldl Set.union Set.empty islandBodyIdSets)

                    totalGroups =
                        visited |> List.map List.length |> List.sum
                in
                Expect.all
                    [ \_ -> visited |> List.length |> Expect.greaterThan 1
                    , \_ -> visited |> List.length |> Expect.lessThan n
                    , \_ -> eachIslandBottomFirst |> Expect.equal True
                    , \_ -> bodyIdsDisjoint |> Expect.equal True
                    , \_ -> totalGroups |> Expect.equal (List.length pairs)
                    ]
                    ()
        ]
