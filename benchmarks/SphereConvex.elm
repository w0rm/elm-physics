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

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.SphereConvex
import Fixtures.ConvexPolyhedron as HullFixtures
import Fixtures.NarrowPhase
import Internal.Body as Body exposing (Body)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        center =
            vec3 0 0 7

        radius =
            5

        boxHalfExtent =
            1

        boxHull =
            HullFixtures.boxHull boxHalfExtent

        originalBoxHull =
            HullFixtures.originalBoxHull boxHalfExtent

        boxPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions center radius boxHalfExtent
                |> List.map Tuple.first

        boxFarPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions center (radius * 2) boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            3

        octoHull =
            HullFixtures.octoHull octoHalfExtent

        originalOctoHull =
            HullFixtures.originalOctoHull octoHalfExtent

        octoPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions center radius octoHalfExtent
                |> List.map Tuple.first

        octoFarPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions center (radius * 2) octoHalfExtent
                |> List.map Tuple.first
    in
    describe "NarrowPhase"
        [ Benchmark.compare "addSphereConvexContacts box"
            "baseline"
            (\_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                Transform.identity
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
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
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                boxHull
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts box fail"
            "baseline"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
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
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                boxHull
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts octohedron"
            "baseline"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
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
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                octoHull
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts oct fail"
            "baseline"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            {- Collision.OriginalSphereConvex.addContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
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
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                octoHull
                                []
                        )
            )
        ]
