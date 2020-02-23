module SphereConvex exposing (main)

{- For a useful benchmark,
   copy and rename an older baseline version of Collision/SphereConvex.elm
   to Collision/OriginalSphereConvex.elm and uncomment the import below,
   then toggle the usage in benchmarks.

   Switching it back to use the (current) SphereConvex.elm through the
   OriginalSphereConvex alias keeps obsolete or redundant code out of
   the repo while the comparison benchmarks continue to be maintained and
   built and run essentially as absolute non-comparison benchmarks until
   they are needed again in another round of performance work.
-}
{- import Collision.OriginalSphereConvex -}

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.SphereConvex
import Fixtures.Convex
import Fixtures.NarrowPhase
import Internal.Convex as Convex
import Internal.Transform3d as Transform3d


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        center =
            { x = 0, y = 0, z = 7 }

        radius =
            5

        boxHalfExtent =
            1

        originTransform3d =
            Transform3d.atOrigin

        boxHull =
            Convex.fromBlock boxHalfExtent boxHalfExtent boxHalfExtent

        originalBoxHull =
            Convex.fromBlock boxHalfExtent boxHalfExtent boxHalfExtent

        boxPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions center radius boxHalfExtent
                |> List.map Tuple.first

        boxFarPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions center (radius * 2) boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            3

        octoHull =
            Fixtures.Convex.octoHull octoHalfExtent

        originalOctoHull =
            Fixtures.Convex.originalOctoHull octoHalfExtent

        octoPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions center radius octoHalfExtent
                |> List.map Tuple.first

        octoFarPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions center (radius * 2) octoHalfExtent
                |> List.map Tuple.first
    in
    describe "SphereConvex"
        [ Benchmark.compare "box colliding"
            "baseline"
            (\_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                originTransform3d
                                radius
                                (Transform3d.atPoint position)
                                originalBoxHull
                                []
                        )
            )
            "latest code"
            (\_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                boxHull
                                []
                        )
            )
        , Benchmark.compare "box separated"
            "baseline"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                originalBoxHull
                                []
                        )
            )
            "latest code"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                boxHull
                                []
                        )
            )
        , Benchmark.compare "octahedron colliding"
            "baseline"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                originalOctoHull
                                []
                        )
            )
            "latest code"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                octoHull
                                []
                        )
            )
        , Benchmark.compare "octahedron failing"
            "baseline"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                originalOctoHull
                                []
                        )
            )
            "latest code"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Transform3d.atPoint center)
                                radius
                                (Transform3d.atPoint position)
                                octoHull
                                []
                        )
            )
        ]
