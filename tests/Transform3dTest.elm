module Transform3dTest exposing (directionRelativeTo, inverse, pointRelativeTo, relativeTo)

import Expect
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Test exposing (Test, describe, test)


pointRelativeTo : Test
pointRelativeTo =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.k (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.i (pi / 5)

        point =
            { x = 0.4, y = 0.6, z = 0.8 }
    in
    describe "Transform3d.pointRelativeTo"
        [ test "transforms the point back to its original value" <|
            \_ ->
                point
                    |> Transform3d.pointPlaceIn transform3d
                    |> Transform3d.pointRelativeTo transform3d
                    |> Expect.all
                        [ .x >> Expect.within (Expect.Absolute 0.00001) point.x
                        , .y >> Expect.within (Expect.Absolute 0.00001) point.y
                        , .z >> Expect.within (Expect.Absolute 0.00001) point.z
                        ]
        ]


directionRelativeTo : Test
directionRelativeTo =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.k (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.i (pi / 5)

        direction =
            { x = 0.4, y = 0.6, z = 0.8 }
    in
    describe "Transform3d.directionRelativeTo"
        [ test "transforms the direction back to its original value" <|
            \_ ->
                direction
                    |> Transform3d.directionPlaceIn transform3d
                    |> Transform3d.directionRelativeTo transform3d
                    |> Expect.all
                        [ .x >> Expect.within (Expect.Absolute 0.00001) direction.x
                        , .y >> Expect.within (Expect.Absolute 0.00001) direction.y
                        , .z >> Expect.within (Expect.Absolute 0.00001) direction.z
                        ]
        ]


relativeTo : Test
relativeTo =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.k (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.i (pi / 5)

        transform3dInverse =
            Transform3d.relativeTo transform3d Transform3d.atOrigin

        direction =
            { x = 0.4, y = 0.6, z = 0.8 }

        point =
            { x = 0.3, y = 0.5, z = 0.7 }
    in
    describe "Transform3d.relativeTo"
        [ test "transforms the direction back to its original value" <|
            \_ ->
                direction
                    |> Transform3d.directionPlaceIn transform3d
                    |> Transform3d.directionPlaceIn transform3dInverse
                    |> Expect.all
                        [ .x >> Expect.within (Expect.Absolute 0.00001) direction.x
                        , .y >> Expect.within (Expect.Absolute 0.00001) direction.y
                        , .z >> Expect.within (Expect.Absolute 0.00001) direction.z
                        ]
        , test "transforms the point back to its original value" <|
            \_ ->
                point
                    |> Transform3d.pointPlaceIn transform3d
                    |> Transform3d.pointPlaceIn transform3dInverse
                    |> Expect.all
                        [ .x >> Expect.within (Expect.Absolute 0.00001) point.x
                        , .y >> Expect.within (Expect.Absolute 0.00001) point.y
                        , .z >> Expect.within (Expect.Absolute 0.00001) point.z
                        ]
        ]


inverse : Test
inverse =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.k (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.i (pi / 5)

        transform3dInverse =
            Transform3d.inverse transform3d

        direction =
            { x = 0.4, y = 0.6, z = 0.8 }

        point =
            { x = 0.3, y = 0.5, z = 0.7 }
    in
    describe "Transform3d.inverse"
        [ test "transforms the direction back to its original value" <|
            \_ ->
                direction
                    |> Transform3d.directionPlaceIn transform3d
                    |> Transform3d.directionPlaceIn transform3dInverse
                    |> Expect.all
                        [ .x >> Expect.within (Expect.Absolute 0.00001) direction.x
                        , .y >> Expect.within (Expect.Absolute 0.00001) direction.y
                        , .z >> Expect.within (Expect.Absolute 0.00001) direction.z
                        ]
        , test "transforms the point back to its original value" <|
            \_ ->
                point
                    |> Transform3d.pointPlaceIn transform3d
                    |> Transform3d.pointPlaceIn transform3dInverse
                    |> Expect.all
                        [ .x >> Expect.within (Expect.Absolute 0.00001) point.x
                        , .y >> Expect.within (Expect.Absolute 0.00001) point.y
                        , .z >> Expect.within (Expect.Absolute 0.00001) point.z
                        ]
        ]
