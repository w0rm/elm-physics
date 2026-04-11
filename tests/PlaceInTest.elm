module PlaceInTest exposing (placeInTests)

import Angle
import Axis3d
import Block3d
import Extra.Expect as Expect
import Frame3d
import Physics
import Physics.Material as Material
import Point3d
import Test exposing (Test, describe, test)


unitBlock : Physics.Body
unitBlock =
    Physics.block
        (Block3d.from (Point3d.meters -0.5 -0.5 -0.5) (Point3d.meters 0.5 0.5 0.5))
        Material.wood


placeInTests : Test
placeInTests =
    describe "Physics.placeIn"
        [ test "placeIn atOrigin is identity" <|
            \_ ->
                unitBlock
                    |> Physics.placeIn Frame3d.atOrigin
                    |> Physics.frame
                    |> Expect.frame3d Frame3d.atOrigin
        , test "placeIn sets position" <|
            \_ ->
                let
                    target =
                        Frame3d.atPoint (Point3d.meters 1 2 3)
                in
                unitBlock
                    |> Physics.placeIn target
                    |> Physics.frame
                    |> Expect.frame3d target
        , test "placeIn sets orientation" <|
            \_ ->
                let
                    target =
                        Frame3d.atOrigin
                            |> Frame3d.rotateAround Axis3d.z (Angle.degrees 90)
                in
                unitBlock
                    |> Physics.placeIn target
                    |> Physics.frame
                    |> Expect.frame3d target
        , test "placeIn sets position and orientation" <|
            \_ ->
                let
                    target =
                        Frame3d.atPoint (Point3d.meters 3 4 5)
                            |> Frame3d.rotateAround Axis3d.x (Angle.degrees 45)
                in
                unitBlock
                    |> Physics.placeIn target
                    |> Physics.frame
                    |> Expect.frame3d target
        , test "placeIn overwrites previous moveTo" <|
            \_ ->
                let
                    target =
                        Frame3d.atPoint (Point3d.meters 10 0 0)
                in
                unitBlock
                    |> Physics.moveTo (Point3d.meters 1 2 3)
                    |> Physics.placeIn target
                    |> Physics.frame
                    |> Expect.frame3d target
        , test "placeIn overwrites previous rotateAround" <|
            \_ ->
                let
                    target =
                        Frame3d.atOrigin
                            |> Frame3d.rotateAround Axis3d.y (Angle.degrees 30)
                in
                unitBlock
                    |> Physics.rotateAround Axis3d.z (Angle.degrees 90)
                    |> Physics.placeIn target
                    |> Physics.frame
                    |> Expect.frame3d target
        , test "placeIn roundtrips with frame" <|
            \_ ->
                let
                    moved =
                        unitBlock
                            |> Physics.moveTo (Point3d.meters 1 2 3)
                            |> Physics.rotateAround Axis3d.z (Angle.degrees 45)

                    bodyFrame =
                        Physics.frame moved
                in
                unitBlock
                    |> Physics.placeIn bodyFrame
                    |> Physics.frame
                    |> Expect.frame3d bodyFrame
        , test "placeIn with mirrored frame ignores mirroring" <|
            \_ ->
                let
                    -- reverseX makes a left-handed frame
                    mirrored =
                        Frame3d.atPoint (Point3d.meters 1 2 3)
                            |> Frame3d.rotateAround Axis3d.z (Angle.degrees 45)
                            |> Frame3d.reverseX

                    -- x and y are preserved, z is recomputed as x cross y
                    expected =
                        Frame3d.atPoint (Point3d.meters 1 2 3)
                            |> Frame3d.rotateAround Axis3d.z (Angle.degrees 45)
                            |> Frame3d.reverseX
                            |> Frame3d.reverseZ
                in
                unitBlock
                    |> Physics.placeIn mirrored
                    |> Physics.frame
                    |> Expect.frame3d expected
        ]
