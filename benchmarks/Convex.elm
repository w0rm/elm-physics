module Convex exposing (main)

{- For a useful benchmark,
   copy and rename an older baseline version of Physics/Convex.elm
   to Physics/OriginalConvex.elm and uncomment the import below,
   then toggle the usage in benchmarks.

   Switching it back to use the (current) Convex.elm through the
   OriginalConvex alias keeps obsolete or redundant code out of
   the repo while the comparison benchmarks continue to be maintained and
   built and run essentially as absolute non-comparison benchmarks until
   they are needed again in another round of performance work.
-}
{- import Physics.OriginalConvex as OriginalConvex -}

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Convex as Convex
import Internal.Quaternion as Quaternion
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


main : BenchmarkProgram
main =
    program <|
        describe "Convex"
            [ foldFaceNormals
            ]


foldFaceNormals : Benchmark
foldFaceNormals =
    let
        sampleHull =
            vec3 1 1 1
                |> Convex.fromBox

        originalSampleHull =
            vec3 1 1 1
                |> {- OriginalConvex.fromBox -} Convex.fromBox

        trivialVisitor : Vec3 -> Vec3 -> Int -> Int
        trivialVisitor _ _ _ =
            0

        -- Move the box 0.45 units up
        -- only 0.05 units of the box will be below plane z=0
        transform =
            { position = vec3 0 0 0.45
            , orientation = Quaternion.identity
            }
    in
    Benchmark.compare "foldFaceNormals"
        "baseline"
        (\_ ->
            {- OriginalConvex.foldFaceNormals -}
            Convex.foldFaceNormals
                -- fold a function with minimal overhead
                trivialVisitor
                0
                originalSampleHull
        )
        "latest code"
        (\_ ->
            Convex.foldFaceNormals
                -- fold a function with minimal overhead
                trivialVisitor
                0
                sampleHull
        )
