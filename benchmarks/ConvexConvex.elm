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

import Angle
import Axis3d
import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.ConvexConvex
import Frame3d
import Internal.Convex as Convex exposing (Convex)
import Point3d


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
        frame3d =
            Frame3d.atPoint Point3d.origin
                |> Frame3d.rotateAround Axis3d.y (Angle.radians (pi / 4))
                |> Frame3d.rotateAround Axis3d.x (Angle.radians (pi / 20))
                |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 0.9 })

        originFrame3d =
            Frame3d.atPoint Point3d.origin
    in
    Benchmark.compare "colliding"
        "baseline"
        (\_ ->
            {- Collision.OriginalConvexConvex.addContacts -}
            Collision.ConvexConvex.addContacts
                frame3d
                box
                originFrame3d
                box
                []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts
                frame3d
                box
                originFrame3d
                box
                []
        )


separated : Benchmark
separated =
    let
        -- Move the box 2.5 units up
        -- so that boxes don’t overlap
        frame3d =
            Frame3d.atPoint Point3d.origin
                |> Frame3d.rotateAround Axis3d.y (Angle.radians (pi / 4))
                |> Frame3d.rotateAround Axis3d.x (Angle.radians (pi / 20))
                |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 2.5 })

        originFrame3d =
            Frame3d.atPoint Point3d.origin
    in
    Benchmark.compare "separated"
        "baseline"
        (\_ ->
            {- Collision.OriginalConvexConvex.addContacts -}
            Collision.ConvexConvex.addContacts
                frame3d
                box
                originFrame3d
                box
                []
        )
        "latest code"
        (\_ ->
            Collision.ConvexConvex.addContacts
                frame3d
                box
                originFrame3d
                box
                []
        )


box : Convex
box =
    Convex.fromBlock 1 1 1
