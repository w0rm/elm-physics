module SphereConvex exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.SphereConvex
import Fixtures.Convex
import Fixtures.NarrowPhase
import Shapes.Convex as Convex
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

        boxHull =
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
                            {- Collision.SphereConvex.oldAddContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = { x = 0, y = 0, z = 0 }, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) boxHull)
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
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) boxHull)
                                []
                        )
            )
        , Benchmark.compare "box separated"
            "baseline"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            {- Collision.SphereConvex.oldAddContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) boxHull)
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
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) boxHull)
                                []
                        )
            )
        , Benchmark.compare "octahedron colliding"
            "baseline"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            {- Collision.SphereConvex.oldAddContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) octoHull)
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
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) octoHull)
                                []
                        )
            )
        , Benchmark.compare "octahedron failing"
            "baseline"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            {- Collision.SphereConvex.oldAddContacts -}
                            Collision.SphereConvex.addContacts
                                identity
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) octoHull)
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
                                { position = center, radius = radius }
                                (Convex.placeIn (Transform3d.atPoint position) octoHull)
                                []
                        )
            )
        ]
