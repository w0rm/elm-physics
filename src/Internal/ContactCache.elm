module Internal.ContactCache exposing
    ( ContactCache
    , empty
    , getGroup
    , insertGroup
    , lookup
    )

{-| Experimental warm-start cache variant: a red-black tree keyed by the
**body-pair** key (a single `Int`, derived as `shapeKey // 65536`), where each
node holds that pair's contact points as a short
`List ( Int, Int, v )` of `( shapeKey, featureKey, value )`.

The point: a body pair often produces several contact points (a box-box face
clip is ~4), so this stores one tree node per pair instead of one per point —
far fewer nodes to build and balance each frame — and mirrors the solver's
`EquationsGroup` structure (one group = one body pair), so `collectCaches` does
one insert per group and warm-start fetches each group's list once and scans
it per contact.

Storing the full `(shapeKey, featureKey)` in the entry (rather than packing
shape + feature into one int) keeps the existing field budgets intact — no
overflow risk on detailed convex hulls — at the cost of one extra word per
entry. The shape bits in `shapeKey` are shared within a node but comparing the
whole int is still correct, so no shape-bit extraction is needed.

`Empty` carries five `()` fields to match `Node`'s shape (monomorphic `.$`).

-}


type ContactCache v
    = Node NColor Int (List ( Int, Int, v )) (ContactCache v) (ContactCache v)
    | Empty () () () () ()


type NColor
    = Red
    | Black


empty : ContactCache v
empty =
    Empty () () () () ()


{-| Fetch a body pair's contact group (empty if the pair is absent), to be
linear-scanned with `lookup`.
-}
getGroup : Int -> ContactCache v -> List ( Int, Int, v )
getGroup target dict =
    case dict of
        Empty _ _ _ _ _ ->
            []

        Node _ key list left right ->
            let
                d =
                    target - key
            in
            if d < 0 then
                getGroup target left

            else if d > 0 then
                getGroup target right

            else
                list


{-| Linear-scan a pair's list for `( shapeKey, featureKey )`, returning
`default` if absent. Lists are short (one entry per contact point in the pair).
Feature is checked first — within a pair it varies even when shapes don't, so
it short-circuits fastest.
-}
lookup : Int -> Int -> v -> List ( Int, Int, v ) -> v
lookup shapeKey featureKey default list =
    case list of
        [] ->
            default

        ( s, f, v ) :: rest ->
            if (f - featureKey == 0) && (s - shapeKey == 0) then
                v

            else
                lookup shapeKey featureKey default rest


{-| Store a body pair's whole contact list under its body-pair key. One tree
insert per pair.
-}
insertGroup : Int -> List ( Int, Int, v ) -> ContactCache v -> ContactCache v
insertGroup key list dict =
    case insertHelp key list dict of
        Node Red k l left right ->
            Node Black k l left right

        x ->
            x


insertHelp : Int -> List ( Int, Int, v ) -> ContactCache v -> ContactCache v
insertHelp key list dict =
    -- IGNORE TCO — red-black insert rebalances on the way back up
    case dict of
        Empty _ _ _ _ _ ->
            Node Red key list empty empty

        Node nColor nKey nList nLeft nRight ->
            let
                d =
                    key - nKey
            in
            if d < 0 then
                balance nColor nKey nList (insertHelp key list nLeft) nRight

            else if d > 0 then
                balance nColor nKey nList nLeft (insertHelp key list nRight)

            else
                Node nColor nKey list nLeft nRight


balance : NColor -> Int -> List ( Int, Int, v ) -> ContactCache v -> ContactCache v -> ContactCache v
balance color key list left right =
    case right of
        Node Red rK rL rLeft rRight ->
            case left of
                Node Red lK lL lLeft lRight ->
                    Node
                        Red
                        key
                        list
                        (Node Black lK lL lLeft lRight)
                        (Node Black rK rL rLeft rRight)

                _ ->
                    Node color rK rL (Node Red key list left rLeft) rRight

        _ ->
            case left of
                Node Red lK lL (Node Red llK llL llLeft llRight) lRight ->
                    Node
                        Red
                        lK
                        lL
                        (Node Black llK llL llLeft llRight)
                        (Node Black key list lRight right)

                _ ->
                    Node color key list left right
