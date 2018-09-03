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

import AltMath.Vector3 as AltVec3
import AltPhysics.ConvexPolyhedron as AltConvexPolyhedron
import AltPhysics.Quaternion as AltQuaternion
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Math.Vector3 as Vec3
import Physics.ConvexPolyhedron as ConvexPolyhedron
import Physics.Quaternion as Quaternion


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        sampleHull =
            Vec3.vec3 1 1 1
                |> ConvexPolyhedron.fromBox

        altSampleHull =
            AltVec3.vec3 1 1 1
                |> AltConvexPolyhedron.fromBox

        trivialVisitor : a -> a -> Int -> Int
        trivialVisitor _ _ _ =
            0

        sepNormal =
            Vec3.vec3 0 0 1

        altSepNormal =
            AltVec3.vec3 0 0 1

        -- Move the box 0.45 units up
        -- only 0.05 units of the box will be below plane z=0
        transform =
            { position = Vec3.vec3 0 0 0.45
            , quaternion = Quaternion.identity
            }

        altTransform =
            { position = AltVec3.vec3 0 0 0.45
            , quaternion = AltQuaternion.identity
            }

        -- points in the plane z
        worldVertsB =
            [ Vec3.vec3 -1.0 -1.0 0
            , Vec3.vec3 -1.0 1.0 0
            , Vec3.vec3 1.0 1.0 0
            , Vec3.vec3 1.0 -1.0 0
            ]

        altWorldVertsB =
            [ AltVec3.vec3 -1.0 -1.0 0
            , AltVec3.vec3 -1.0 1.0 0
            , AltVec3.vec3 1.0 1.0 0
            , AltVec3.vec3 1.0 -1.0 0
            ]

        boxHull halfExtent =
            ConvexPolyhedron.fromBox
                (Vec3.vec3 halfExtent halfExtent halfExtent)

        altBoxHull halfExtent =
            AltConvexPolyhedron.fromBox
                (AltVec3.vec3 halfExtent halfExtent halfExtent)
    in
    describe "ConvexPolyhedron"
        [ Benchmark.compare "foldFaceNormals"
            "baseline"
            (\_ ->
                ConvexPolyhedron.foldFaceNormals
                    -- fold a function with minimal overhead
                    trivialVisitor
                    0
                    sampleHull
            )
            "latest code"
            (\_ ->
                AltConvexPolyhedron.foldFaceNormals
                    -- fold a function with minimal overhead
                    trivialVisitor
                    0
                    altSampleHull
            )

        -- We will now clip a face in hullA that is closest to the
        -- sepNormal against the points in worldVertsB.
        -- We can expect to get back the 4 corners of the box hullA
        -- penetrated 0.05 units into the plane worldVertsB we
        -- constructed.
        , Benchmark.compare "clipFaceAgainstHull"
            "baseline"
            (\_ ->
                ConvexPolyhedron.clipFaceAgainstHull
                    transform
                    (boxHull 0.5)
                    sepNormal
                    worldVertsB
                    -100
                    100
            )
            "latest code"
            (\_ ->
                AltConvexPolyhedron.clipFaceAgainstHull
                    altTransform
                    (altBoxHull 0.5)
                    altSepNormal
                    altWorldVertsB
                    -100
                    100
            )
        ]
