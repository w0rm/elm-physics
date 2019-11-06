module ConvexConvex exposing (main)

{- For a useful benchmark,
   copy and rename an older baseline version of Collision/ConvexConvex.elm
   to Collision/OriginalConvexConvex.elm and uncomment the import below,
   then toggle the usage in benchmarks.

   Switching it back to use the (current) ConvexConvex.elm through the
   OriginalConvexConvex alias keeps obsolete or redundant code out of
   the repo while the comparison benchmarks continue to be maintained and
   built and run essentially as absolute non-comparison benchmarks until
   they are needed again in another round of performance work.
-}
{- import Collision.OriginalConvexConvex -}

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.ConvexConvex
import Internal.Convex as Convex exposing (Convex)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


main : BenchmarkProgram
main =
    program <|
        describe "ConvexConvex.getContacts"
            [ colliding
            , separated
            ]


colliding : Benchmark
colliding =
    let
        -- Move the box 0.9 units up and rotate 45 around the y.
        -- only 0.1 units of the box will be overlapping
        -- we expect 4 collision points
        transform3d =
            Transform3d.atPoint { x = 0, y = 0, z = 0.9 }
                |> Transform3d.rotateAroundOwn Vec3.j (pi / 4)
                |> Transform3d.rotateAroundOwn Vec3.i (pi / 20)

        originTransform3d =
            Transform3d.atOrigin
    in
    Benchmark.compare "colliding"
        "baseline"
        (\_ ->
            {- Collision.OriginalConvexConvex.addContacts -}
            Collision.ConvexConvex.addContacts
                transform3d
                box
                originTransform3d
                box
                []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts
                transform3d
                box
                originTransform3d
                box
                []
        )


separated : Benchmark
separated =
    let
        -- Move the box 2.5 units up
        -- so that boxes don’t overlap
        transform3d =
            Transform3d.atPoint { x = 0, y = 0, z = 2.5 }
                |> Transform3d.rotateAroundOwn Vec3.j (pi / 4)
                |> Transform3d.rotateAroundOwn Vec3.i (pi / 20)

        originTransform3d =
            Transform3d.atOrigin
    in
    Benchmark.compare "separated"
        "baseline"
        (\_ ->
            {- Collision.OriginalConvexConvex.addContacts -}
            Collision.ConvexConvex.addContacts
                transform3d
                box
                originTransform3d
                box
                []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts
                transform3d
                box
                originTransform3d
                box
                []
        )


box : Convex
box =
    Convex.fromBlock 1 1 1
