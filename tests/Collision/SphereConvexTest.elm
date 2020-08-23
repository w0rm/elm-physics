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

        radius =
            5

        boxHalfExtent =
            3

        boxHull =
            Convex.fromBlock
                (boxHalfExtent * 2)
                (boxHalfExtent * 2)
                (boxHalfExtent * 2)

        boxFarPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions
                center
                (radius * 2)
                boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            1

        octoHull =
            Fixtures.Convex.octoHull octoHalfExtent

        octoFarPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions
                center
                (radius * 2)
                octoHalfExtent
                |> List.map Tuple.first
    in
    describe "Collision.SphereConvex.addContacts"
        [ test "for a box"
            (Fixtures.NarrowPhase.sphereContactBoxPositions center radius boxHalfExtent
                |> List.map
                    (\( position, expectedContacts ) ->
                        \_ ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin radius))
                                (Convex.placeIn (Transform3d.atPoint position) boxHull)
                                []
                                |> Expect.contacts expectedContacts
                    )
                |> Expect.all
            )
        , test "fail for a far box" <|
            \_ ->
                boxFarPositions
                    |> List.concatMap
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin radius))
                                (Convex.placeIn (Transform3d.atPoint position) boxHull)
                                []
                        )
                    |> Expect.equal []
        , test "for an octohedron"
            (Fixtures.NarrowPhase.sphereContactOctohedronPositions center radius octoHalfExtent
                |> List.map
                    (\( position, expectedContacts ) ->
                        \_ ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin radius))
                                (Convex.placeIn (Transform3d.atPoint position) octoHull)
                                []
                                |> Expect.contacts expectedContacts
                    )
                |> Expect.all
            )
        , test "fail for a far octohedron" <|
            \_ ->
                octoFarPositions
                    |> List.concatMap
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin radius))
                                (Convex.placeIn (Transform3d.atPoint position) octoHull)
                                []
                        )
                    |> Expect.equal []
        ]
