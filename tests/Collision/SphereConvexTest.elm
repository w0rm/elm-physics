module Collision.SphereConvexTest exposing (addContacts)

import Collision.SphereConvex
import Expect
import Extra.Expect as Expect
import Fixtures.Convex
import Fixtures.NarrowPhase
import Internal.Transform3d as Transform3d
import Shapes.Convex as Convex
import Shapes.Sphere as Sphere
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    let
        center =
            { x = 0, y = 0, z = 7 }

        sphereRadius =
            5

        boxSize =
            6

        octoHalfExtent =
            1

        placeInWithCorrectWinding position convex =
            Convex.placeIn Transform3d.atOrigin (Convex.placeIn (Transform3d.atPoint position) convex)
    in
    describe "Collision.SphereConvex.addContacts"
        [ test "for a box"
            (Fixtures.NarrowPhase.sphereContactBoxPositions center sphereRadius boxSize
                |> List.map
                    (\( position, expectedContacts ) ->
                        \_ ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin sphereRadius))
                                (Convex.placeIn (Transform3d.atPoint position) (Convex.fromBlock boxSize boxSize boxSize))
                                []
                                |> Expect.contacts expectedContacts
                    )
                |> Expect.all
            )
        , test "fail for a far box" <|
            \_ ->
                Fixtures.NarrowPhase.sphereContactBoxPositions center (sphereRadius * 2) boxSize
                    |> List.concatMap
                        (\( position, _ ) ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin sphereRadius))
                                (placeInWithCorrectWinding position (Convex.fromBlock boxSize boxSize boxSize))
                                []
                        )
                    |> Expect.equal []
        , test "for an octohedron"
            (Fixtures.NarrowPhase.sphereContactOctohedronPositions center sphereRadius octoHalfExtent
                |> List.map
                    (\( position, expectedContacts ) ->
                        \_ ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin sphereRadius))
                                (placeInWithCorrectWinding position (Fixtures.Convex.octoHull octoHalfExtent))
                                []
                                |> Expect.contacts expectedContacts
                    )
                |> Expect.all
            )
        , test "fail for a far octohedron" <|
            \_ ->
                Fixtures.NarrowPhase.sphereContactOctohedronPositions center (sphereRadius * 2) octoHalfExtent
                    |> List.concatMap
                        (\( position, _ ) ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin sphereRadius))
                                (placeInWithCorrectWinding position (Fixtures.Convex.octoHull octoHalfExtent))
                                []
                        )
                    |> Expect.equal []
        ]
