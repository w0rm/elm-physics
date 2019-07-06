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

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Convex as Convex
import Internal.Vector3 exposing (Vec3)


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
            Convex.fromBox { x = 1, y = 1, z = 1 }

        originalSampleHull =
            {- OriginalConvex.fromBox -}
            Convex.fromBox { x = 1, y = 1, z = 1 }

        trivialVisitor : Vec3 -> Vec3 -> Int -> Int
        trivialVisitor _ _ _ =
            0
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
