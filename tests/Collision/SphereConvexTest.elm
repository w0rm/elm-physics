module Collision.SphereConvexTest exposing (addContacts)

import Collision.SphereConvex
import Expect
import Extra.Expect as Expect
import Fixtures.Convex
import Fixtures.NarrowPhase
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
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
                            Collision.SphereConvex.addContacts ""
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
                            Collision.SphereConvex.addContacts ""
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
                            Collision.SphereConvex.addContacts ""
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
                            Collision.SphereConvex.addContacts ""
                                identity
                                (Sphere.placeIn (Transform3d.atPoint center) (Sphere.atOrigin sphereRadius))
                                (placeInWithCorrectWinding position (Fixtures.Convex.octoHull octoHalfExtent))
                                []
                        )
                    |> Expect.equal []
        , test "orderContact flip: ni negated and pi/pj swapped when convex is body1" <|
            \_ ->
                let
                    -- Sphere-vs-box face contact along +x. Without the flip:
                    -- ni = +x, pi on sphere (-x side), pj on box (+x side).
                    -- With the flip: ni = -x, pi/pj swapped.
                    boxPos =
                        { x = sphereRadius + boxSize / 2 - 0.1, y = 0, z = 0 }

                    sphere =
                        Sphere.placeIn (Transform3d.atPoint { x = 0, y = 0, z = 0 })
                            (Sphere.atOrigin sphereRadius)

                    box =
                        Convex.placeIn (Transform3d.atPoint boxPos) (Convex.fromBlock boxSize boxSize boxSize)

                    flipper c =
                        { c | ni = Vec3.negate c.ni, pi = c.pj, pj = c.pi }

                    direct =
                        Collision.SphereConvex.addContacts "" identity sphere box []

                    flipped =
                        Collision.SphereConvex.addContacts "" flipper sphere box []
                in
                Expect.contacts (List.map flipper direct) flipped
        ]
