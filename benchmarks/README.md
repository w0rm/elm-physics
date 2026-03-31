# Elm Performance Rules

When writing or reviewing Elm code in this repo, apply these performance rules to avoid slow JavaScript output, and then check if the tests pass:

1. **Tail-optimized recursion over List.*** — Replace `List.map`, `List.foldl`, `List.filter`, etc. with explicit tail-recursive helper functions. They compile to `while` loops in JS; higher-order List functions do not. Unpack any intermediate records used for recursion state into separate arguments to reduce object allocation. But keep in mind that mutual recursion is not optimised and must be avoided.

2. **Compare to zero instead of comparing two bindings** — Prefer `a - b == 0` over `a == b` when both sides are bindings. Direct equality between two bindings calls the Elm structural equality function; subtracting and comparing to 0 uses JS `===` on a number. Comparing against a numeric literal (e.g. `a == -1`, `idx > -1`) is fine as-is — the compiler already emits a direct JS comparison for literals.

3. **Unroll tuple pattern matches into nested case expressions** — Replace `case (a, b) of` with nested `case a of` / `case b of`. Tuple patterns allocate a temporary object at runtime; nested matches do not.

4. **Use sentinel values instead of Maybe for Int** — Avoid wrapping Int results in `Just`/`Nothing` when a sentinel like `-1` can represent the absent case. This eliminates allocation and unwrapping overhead. A similar technique: use an impossible filler element in a List or Array to avoid `Maybe` at boundaries.

5. **Pattern match Result/Maybe directly instead of using map/andThen** — Replace `Maybe.map f x` and `Result.andThen f x` with explicit `case` expressions. This avoids lambda allocation and indirect calls.

6. **Accumulate in reverse order when safe** — When making multiple recursive passes over a list, intermediate passes may produce a reversed list as long as the final output is correct. Avoid unnecessary `List.reverse` calls between passes. It is OK to use `List.reverse` when you must fix element order without modifying the elements themselves.

7. **Never use record update syntax** — `{ r | field = value }` generates slow JavaScript. Always spell out all record fields explicitly when constructing an updated record, even if most values are unchanged.

8. **Never use List.length** — `List.length` traverses the entire list and is O(n). Track counts explicitly as a separate argument rather than computing them from list lengths.

9. **Prefer `Set.fromList` over repeated `Set.insert`** — When building a `Set` from multiple elements, accumulate them into a list first and call `Set.fromList` at the end. Repeatedly calling `Set.insert` is slower than a single `Set.fromList` call.

10. **Avoid `|>` on the hot path** — Do not use the pipe operator in tight loops or recursive functions that process faces or vertices. It introduces lambda wrapping and indirect calls. `|>` is fine outside the hot path (e.g. at the decoder level or in one-time setup code).

11. **Prefer `Dict.get` + `Dict.insert` over `Dict.update`** — `Dict.update` accepts a lambda, which introduces indirect calls and closure allocation. Use `Dict.get` + a `case` expression + `Dict.insert` instead when conditionally inserting or modifying a value in a `Dict`.

12. **Minimize `Just wrappers`** use `(Just value) as passThroug ->` in  and then return `passThrough` from the case of, instead of returning `(Just value)`.
