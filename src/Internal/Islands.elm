module Internal.Islands exposing (Islands, connect, fold, init)

{-| Islands: connected components of dynamic bodies in the contact graph.
`connect` merges two bodies' islands as the solver walks pairs; `fold` visits
each island in turn, so settled regions stop iterating independently. Static
bodies never connect, so bodies sharing only a floor stay separate.

A disjoint-set: the value is the parent array (body id → parent), unioned by
`connect` (lower id wins) with path-halving lookups.

-}

import Array exposing (Array)
import Internal.Equation exposing (EquationsGroup)


type alias Islands =
    Array Int


{-| Every body `0..maxId` alone in its own island.
-}
init : Int -> Islands
init maxId =
    Array.initialize (maxId + 1) identity


{-| Merge the islands containing two dynamic bodies (lower id becomes root).
-}
connect : Int -> Int -> Islands -> Islands
connect a b parents =
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


{-| Visit each island once, in body-id order, folding `solveIsland` over `acc`.
-}
fold : (List (EquationsGroup id) -> b -> b) -> b -> List (EquationsGroup id) -> Islands -> b
fold solveIsland acc groups parents =
    case List.sortWith compareRoot (annotate parents groups []) of
        [] ->
            acc

        ( firstRoot, firstGroup ) :: rest ->
            walk solveIsland firstRoot [ firstGroup ] acc rest



-- internals


{-| Tag each group with its island root, threading `parents` for path-halving.
Reverses the input list.
-}
annotate : Array Int -> List (EquationsGroup id) -> List ( Int, EquationsGroup id ) -> List ( Int, EquationsGroup id )
annotate parents groups acc =
    case groups of
        [] ->
            acc

        group :: rest ->
            let
                -- Key off the dynamic body: a static body's root is itself.
                -- Broadphase never pairs two statics, so a static body1 means
                -- body2 is dynamic.
                pickedId =
                    if group.body1.body.kindInt == 2 then
                        group.body1.body.id

                    else
                        group.body2.body.id

                ( newParents, root ) =
                    findRoot pickedId parents
            in
            annotate newParents rest (( root, group ) :: acc)


compareRoot : ( Int, a ) -> ( Int, a ) -> Order
compareRoot ( a, _ ) ( b, _ ) =
    if a - b < 0 then
        LT

    else if a - b > 0 then
        GT

    else
        EQ


{-| Walk the root-sorted groups, growing the current island until the root
changes, then flush it. The cons undoes `annotate`'s reverse, so each island
reaches `solveIsland` in broadphase order.
-}
walk : (List (EquationsGroup id) -> b -> b) -> Int -> List (EquationsGroup id) -> b -> List ( Int, EquationsGroup id ) -> b
walk solveIsland currentRoot currentIsland acc remaining =
    case remaining of
        [] ->
            solveIsland currentIsland acc

        ( root, group ) :: rest ->
            if root - currentRoot == 0 then
                walk solveIsland currentRoot (group :: currentIsland) acc rest

            else
                walk solveIsland root [ group ] (solveIsland currentIsland acc) rest


{-| Root of `id`, path-halving each lookup (relink to grandparent) to keep
trees shallow under adversarial union order.
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
