module Internal.VertexBuffer exposing
    ( VertexBuffer
    , foldl
    , fromList
    , get
    , map
    )

{-| A static, densely-indexed (0..n-1) immutable store of `Vec3` vertices,
balanced once at construction. Two hot-path operations, both allocation-lean:

  - `get` returns the vertex directly (never a `Maybe`), so dereferencing an
    index during placement allocates nothing — unlike `Array.get`, which boxes
    every read in a `Just`.
  - `map` rebuilds the tree applying a function to each vertex (used to place
    all of a convex's unique vertices in one pass), so faces/edges that share a
    vertex place it once instead of once per incidence.

Navigation compares keys by integer subtraction (`target - key`), never the
shared recursive `_Utils_cmp`. `Empty` carries four `()` fields so it shares
`Node`'s object shape (monomorphic `.$`), matching `Internal.ContactCache`.

-}

import Internal.Vector3 as Vec3 exposing (Vec3)


type VertexBuffer
    = Node Int Vec3 VertexBuffer VertexBuffer
    | Empty () () () ()


empty : VertexBuffer
empty =
    Empty () () () ()


{-| Build a balanced tree over keys `0..n-1` in list order. One-time cost at
shape construction (off the hot path); `map` is what runs per frame.
-}
fromList : List Vec3 -> VertexBuffer
fromList vertices =
    let
        ( buffer, _, _ ) =
            fromListHelp (count vertices 0) 0 vertices
    in
    buffer


{-| Consume the first `n` of `vertices` into a balanced tree: the first ⌊n/2⌋
become the left subtree, the next is the root, the rest the right subtree. Keys
are assigned in list order (so `get i` returns the i-th vertex), starting at
`startKey`. Returns the tree, the unconsumed tail, and the next free key.
-}
fromListHelp : Int -> Int -> List Vec3 -> ( VertexBuffer, List Vec3, Int )
fromListHelp n startKey vertices =
    -- IGNORE TCO — builds a balanced tree, both subtrees recurse
    if n <= 0 then
        ( empty, vertices, startKey )

    else
        let
            ( left, afterLeft, rootKey ) =
                fromListHelp (n // 2) startKey vertices
        in
        case afterLeft of
            value :: rest ->
                let
                    ( right, afterRight, nextKey ) =
                        fromListHelp (n - n // 2 - 1) (rootKey + 1) rest
                in
                ( Node rootKey value left right, afterRight, nextKey )

            [] ->
                ( empty, [], rootKey )


count : List a -> Int -> Int
count list acc =
    case list of
        _ :: rest ->
            count rest (acc + 1)

        [] ->
            acc


{-| Fetch the vertex at `index` (`Vec3.zero` if absent — never allocates). The
indices come from the buffer's own construction, so the default is unreachable
in practice; it just keeps `get` total without a `Maybe`.
-}
get : Int -> VertexBuffer -> Vec3
get index buffer =
    case buffer of
        Empty _ _ _ _ ->
            Vec3.zero

        Node key value left right ->
            let
                d =
                    index - key
            in
            if d < 0 then
                get index left

            else if d > 0 then
                get index right

            else
                value


{-| Apply `fn` to every vertex, preserving structure — places all unique
vertices in one traversal.
-}
map : (Vec3 -> Vec3) -> VertexBuffer -> VertexBuffer
map fn buffer =
    -- IGNORE TCO — binary tree, both branches recurse
    case buffer of
        Empty _ _ _ _ ->
            -- Reuse the shared sentinel — leaves carry no data, so re-allocating
            -- one per leaf each frame is pure garbage (the leaves outnumber the
            -- value-bearing nodes in a balanced tree).
            empty

        Node key value left right ->
            Node key (fn value) (map fn left) (map fn right)


{-| Fold over every vertex (in-order, ascending key). Used for order-independent
reductions like min/max projection, so collision needn't materialise a list.
-}
foldl : (Vec3 -> b -> b) -> b -> VertexBuffer -> b
foldl fn acc buffer =
    -- IGNORE TCO — binary tree, both branches recurse
    case buffer of
        Empty _ _ _ _ ->
            acc

        Node _ value left right ->
            foldl fn (fn value (foldl fn acc left)) right
