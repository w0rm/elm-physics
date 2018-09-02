module ConvexPolyhedron exposing (main)

{- For a useful benchmark,
   copy and rename an older baseline version of Physics/ConvexPolyhedron.elm
   to Physics/OriginalConvexPolyhedron.elm and uncomment the import below,
   then toggle the usage in benchmarks.

   Switching it back to use the (current) ConvexPolyhedron.elm through the
   OriginalConvexPolyhedron alias keeps obsolete or redundant code out of
   the repo while the comparison benchmarks continue to be maintained and
   built and run essentially as absolute non-comparison benchmarks until
   they are needed again in another round of performance work.
-}
{- import Physics.OriginalConvexPolyhedron as OriginalConvexPolyhedron -}

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.ConvexPolyhedron as ConvexPolyhedron
import Physics.Quaternion as Quaternion


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        sampleHull =
            vec3 1 1 1
                |> ConvexPolyhedron.fromBox

        originalSampleHull =
            vec3 1 1 1
                |> {- OriginalConvexPolyhedron.fromBox -} ConvexPolyhedron.fromBox

        trivialVisitor : Vec3 -> Vec3 -> Int -> Int
        trivialVisitor _ _ _ =
            0

        sepNormal =
            vec3 0 0 1

        -- Move the box 0.45 units up
        -- only 0.05 units of the box will be below plane z=0
        transform =
            { position = vec3 0 0 0.45
            , quaternion = Quaternion.identity
            }

        -- points in the plane z
        worldVertsB =
            [ vec3 -1.0 -1.0 0
            , vec3 -1.0 1.0 0
            , vec3 1.0 1.0 0
            , vec3 1.0 -1.0 0
            ]

        boxHull halfExtent =
            ConvexPolyhedron.fromBox
                (vec3 halfExtent halfExtent halfExtent)

        originalBoxHull halfExtent =
            {- OriginalConvexPolyhedron.fromBox -}
            ConvexPolyhedron.fromBox
                (vec3 halfExtent halfExtent halfExtent)
    in
    describe "ConvexPolyhedron"
        [ Benchmark.compare "foldFaceNormals"
            "baseline"
            (\_ ->
                {- OriginalConvexPolyhedron.foldFaceNormals -}
                ConvexPolyhedron.foldFaceNormals
                    -- fold a function with minimal overhead
                    trivialVisitor
                    0
                    originalSampleHull
            )
            "latest code"
            (\_ ->
                ConvexPolyhedron.foldFaceNormals
                    -- fold a function with minimal overhead
                    trivialVisitor
                    0
                    sampleHull
            )

        -- We will now clip a face in hullA that is closest to the
        -- sepNormal against the points in worldVertsB.
        -- We can expect to get back the 4 corners of the box hullA
        -- penetrated 0.05 units into the plane worldVertsB we
        -- constructed.
        , Benchmark.compare "clipFaceAgainstHull"
            "baseline"
            (\_ ->
                {- OriginalConvexPolyhedron.clipFaceAgainstHull -}
                ConvexPolyhedron.clipFaceAgainstHull
                    transform
                    (originalBoxHull 0.5)
                    sepNormal
                    worldVertsB
                    -100
                    100
            )
            "latest code"
            (\_ ->
                ConvexPolyhedron.clipFaceAgainstHull
                    transform
                    (boxHull 0.5)
                    sepNormal
                    worldVertsB
                    -100
                    100
            )
        ]
