module ConvexConvex exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.ConvexConvex
import Shapes.Convex as Convex exposing (Convex)
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
                |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 4)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 20)

        firstConvex =
            Convex.placeIn transform3d box

        secondConvex =
            Convex.placeIn Transform3d.atOrigin box
    in
    Benchmark.compare "colliding"
        "baseline"
        (\_ ->
            {- Collision.ConvexConvex.oldAddContacts -}
            Collision.ConvexConvex.addContacts firstConvex secondConvex []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts firstConvex secondConvex []
        )


separated : Benchmark
separated =
    let
        -- Move the box 2.5 units up
        -- so that boxes don’t overlap
        transform3d =
            Transform3d.atPoint { x = 0, y = 0, z = 2.5 }
                |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 4)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 20)

        firstConvex =
            Convex.placeIn transform3d box

        secondConvex =
            Convex.placeIn Transform3d.atOrigin box
    in
    Benchmark.compare "separated"
        "baseline"
        (\_ ->
            {- Collision.ConvexConvex.oldAddContacts -}
            Collision.ConvexConvex.addContacts firstConvex secondConvex []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts firstConvex secondConvex []
        )


box : Convex
box =
    Convex.fromBlock 1 1 1
