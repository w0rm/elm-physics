module ConvexPolyhedron exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Physics.OriginalConvexPolyhedron as OriginalConvexPolyhedron


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        sampleHull =
            vec3 1 1 1
                |> ConvexPolyhedron.fromBox

        trivialVisitor : Vec3 -> Vec3 -> Int -> Int
        trivialVisitor _ _ _ =
            0
    in
        describe "ConvexPolyhedron"
            [ benchmark "foldFaceNormals" <|
                \_ ->
                    ConvexPolyhedron.foldFaceNormals
                        -- fold a function with minimal overhead
                        trivialVisitor
                        0
                        sampleHull

            -- compare the results of two benchmarks
            , Benchmark.compare "foldFaceNormals"
                "original parallel face arrays"
                (\_ ->
                    ConvexPolyhedron.foldFaceNormals
                        -- fold a function with minimal overhead
                        trivialVisitor
                        0
                        sampleHull
                )
                "unified face array"
                (\_ ->
                    OriginalConvexPolyhedron.foldFaceNormals
                        -- fold a function with minimal overhead
                        trivialVisitor
                        0
                        sampleHull
                )
            ]
