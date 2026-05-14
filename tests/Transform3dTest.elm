module Transform3dTest exposing (directionRelativeTo, inverse, lerp, pointRelativeTo, relativeTo)

import Expect
import Extra.Expect as Expect
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Test exposing (Test, describe, test)


pointRelativeTo : Test
pointRelativeTo =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 5)

        point =
            { x = 0.4, y = 0.6, z = 0.8 }
    in
    describe "Transform3d.pointRelativeTo"
        [ test "transforms the point back to its original value" <|
            \_ ->
                point
                    |> Transform3d.pointPlaceIn transform3d
                    |> Transform3d.pointRelativeTo transform3d
                    |> Expect.vec3 point
        ]


directionRelativeTo : Test
directionRelativeTo =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 5)

        direction =
            { x = 0.4, y = 0.6, z = 0.8 }
    in
    describe "Transform3d.directionRelativeTo"
        [ test "transforms the direction back to its original value" <|
            \_ ->
                direction
                    |> Transform3d.directionPlaceIn transform3d
                    |> Transform3d.directionRelativeTo transform3d
                    |> Expect.vec3 direction
        ]


relativeTo : Test
relativeTo =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 5)

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
                    |> Expect.vec3 direction
        , test "transforms the point back to its original value" <|
            \_ ->
                point
                    |> Transform3d.pointPlaceIn transform3d
                    |> Transform3d.pointPlaceIn transform3dInverse
                    |> Expect.vec3 point
        ]


lerp : Test
lerp =
    let
        t1 =
            Transform3d.atPoint { x = 0, y = 0, z = 0 }

        t2 =
            Transform3d.atPoint { x = 2, y = 4, z = 6 }
                |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 2)

        -- A point on the X axis of body coords, placed in world.
        probe =
            { x = 1, y = 0, z = 0 }
    in
    describe "Transform3d.lerp"
        [ test "t = 0 returns the first transform's origin" <|
            \_ ->
                Transform3d.originPoint (Transform3d.lerp 0 t1 t2)
                    |> Expect.vec3 { x = 0, y = 0, z = 0 }
        , test "t = 1 returns the second transform's origin" <|
            \_ ->
                Transform3d.originPoint (Transform3d.lerp 1 t1 t2)
                    |> Expect.vec3 { x = 2, y = 4, z = 6 }
        , test "t = 0.5 origin is the midpoint" <|
            \_ ->
                Transform3d.originPoint (Transform3d.lerp 0.5 t1 t2)
                    |> Expect.vec3 { x = 1, y = 2, z = 3 }
        , test "t = 0 leaves orientation unchanged" <|
            \_ ->
                -- placing a body-X point at t=0 should match placing it via t1
                Transform3d.pointPlaceIn (Transform3d.lerp 0 t1 t2) probe
                    |> Expect.vec3 (Transform3d.pointPlaceIn t1 probe)
        , test "t = 1 reaches t2's orientation (probe ends at t2's X direction)" <|
            \_ ->
                Transform3d.pointPlaceIn (Transform3d.lerp 1 t1 t2) probe
                    |> Expect.vec3 (Transform3d.pointPlaceIn t2 probe)
        , test "interpolation stays normalized (round-trip through inverse)" <|
            \_ ->
                let
                    lerped =
                        Transform3d.lerp 0.5 t1 t2
                in
                probe
                    |> Transform3d.pointPlaceIn lerped
                    |> Transform3d.pointRelativeTo lerped
                    |> Expect.vec3 probe
        , test "short-path correction: lerping toward 3π/2 rotates via -π/4, not +3π/4" <|
            \_ ->
                -- 3π/2 around Z represents the same orientation as -π/2.
                -- Short-path interpolation should take us via -π/4. Without
                -- the double-cover correction we'd go the long way through
                -- +3π/4 and end up at the antipodal point.
                let
                    longRotation =
                        Transform3d.atOrigin
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (3 * pi / 2)

                    negativeQuarter =
                        Transform3d.atOrigin
                            |> Transform3d.rotateAroundOwn Vec3.zAxis -(pi / 4)
                in
                Transform3d.pointPlaceIn (Transform3d.lerp 0.5 Transform3d.atOrigin longRotation) probe
                    |> Expect.vec3 (Transform3d.pointPlaceIn negativeQuarter probe)
        ]


inverse : Test
inverse =
    let
        transform3d =
            Transform3d.atPoint { x = 0.5, y = 0.6, z = 0.7 }
                |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 5)
                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 5)

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
                    |> Expect.vec3 direction
        , test "transforms the point back to its original value" <|
            \_ ->
                point
                    |> Transform3d.pointPlaceIn transform3d
                    |> Transform3d.pointPlaceIn transform3dInverse
                    |> Expect.vec3 point
        ]
