module Convex exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Convex as Convex
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3


main : BenchmarkProgram
main =
    program <|
        describe "Convex"
            [ placeIn
            ]



placeIn : Benchmark
placeIn =
    let
        sampleHull =
            Convex.fromBlock 1 1 1

        transform =
            Transform3d.atPoint { x = 0, y = 0, z = 2.5 }
                |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 4)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 20)
    in
    Benchmark.compare "placeIn"
        "baseline"
        (\_ ->
            {- Convex.placeInOld -}
            Convex.placeIn transform sampleHull
        )
        "latest code"
        (\_ ->
            Convex.placeIn transform sampleHull
        )
