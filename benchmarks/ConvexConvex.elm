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

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.ConvexConvex
import Internal.Convex as Convex exposing (Convex)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


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
        transform =
            { position = vec3 0 0 0.9
            , orientation =
                Quaternion.mul
                    (Quaternion.fromAngleAxis (pi / 4) (vec3 0 1 0))
                    (Quaternion.fromAngleAxis (pi / 20) (vec3 1 0 0))
            }
    in
    Benchmark.compare "colliding"
        "baseline"
        (\_ ->
            {- Collision.OriginalConvexConvex.addContacts -}
            Collision.ConvexConvex.addContacts
                transform
                box
                Transform.identity
                box
                []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts
                transform
                box
                Transform.identity
                box
                []
        )


separated : Benchmark
separated =
    let
        -- Move the box 2.5 units up
        -- so that boxes don’t overlap
        transform =
            { position = vec3 0 0 2.5
            , orientation =
                Quaternion.mul
                    (Quaternion.fromAngleAxis (pi / 4) (vec3 0 1 0))
                    (Quaternion.fromAngleAxis (pi / 20) (vec3 1 0 0))
            }
    in
    Benchmark.compare "separated"
        "baseline"
        (\_ ->
            {- Collision.OriginalConvexConvex.addContacts -}
            Collision.ConvexConvex.addContacts
                transform
                box
                Transform.identity
                box
                []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts
                transform
                box
                Transform.identity
                box
                []
        )


box : Convex
box =
    Convex.fromBox (vec3 1 1 1)
