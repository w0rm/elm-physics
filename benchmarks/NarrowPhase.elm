module NarrowPhase exposing (main)

{- For a useful benchmark,
   copy and rename an older baseline version of Physics/NarrowPhase.elm
   to Physics/OriginalNarrowPhase.elm and uncomment the import below,
   then toggle the usage in benchmarks.

   Switching it back to use the (current) NarrowPhase.elm through the
   OriginalNarrowPhase alias keeps obsolete or redundant code out of
   the repo while the comparison benchmarks continue to be maintained and
   built and run essentially as absolute non-comparison benchmarks until
   they are needed again in another round of performance work.
-}
{- import Physics.OriginalNarrowPhase as OriginalNarrowPhase -}

import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Fixtures.ConvexPolyhedron as HullFixtures
import Fixtures.NarrowPhase
import Internal.Body as Body exposing (Body)
import Internal.NarrowPhase as NarrowPhase
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform


emptyBody : Body ()
emptyBody =
    Body.compound [] ()


body1 : Body ()
body1 =
    { emptyBody | id = 0 }


body2 : Body ()
body2 =
    { emptyBody | id = 1 }


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
                            {- OriginalNarrowPhase.addSphereConvexContacts -}
                            NarrowPhase.addSphereConvexContacts
                                Transform.identity
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                originalBoxHull
                                body2
                                []
                        )
            )
            "latest code"
            (\_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                boxHull
                                body2
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts box fail"
            "baseline"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            {- OriginalNarrowPhase.addSphereConvexContacts -}
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                originalBoxHull
                                body2
                                []
                        )
            )
            "latest code"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                boxHull
                                body2
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts octohedron"
            "baseline"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            {- OriginalNarrowPhase.addSphereConvexContacts -}
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                originalOctoHull
                                body2
                                []
                        )
            )
            "latest code"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                octoHull
                                body2
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts oct fail"
            "baseline"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            {- OriginalNarrowPhase.addSphereConvexContacts -}
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                originalOctoHull
                                body2
                                []
                        )
            )
            "latest code"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                octoHull
                                body2
                                []
                        )
            )
        ]
