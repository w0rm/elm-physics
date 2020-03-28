module Collision.ConvexConvexTest exposing
    ( addContacts
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Collision.ConvexConvex
import Expect
import Internal.Const as Const
import Internal.Convex as Convex
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    describe "Collision.ConvexConvex.addContacts"
        [ test "should return 4 results" <|
            \_ ->
                let
                    convex =
                        Convex.fromBlock 1 1 1

                    t1 =
                        -- going slightly into another box
                        Transform3d.atPoint { x = 0, y = 0, z = 2.1 }
                            |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)

                    t2 =
                        Transform3d.atPoint { x = 0, y = 0, z = 4 }
                            |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)
                in
                Collision.ConvexConvex.addContacts (Convex.placeIn t1 convex) (Convex.placeIn t2 convex) []
                    |> List.length
                    |> Expect.equal 4
        , test "should return 2 results" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 0.6 0.6 0.6

                    convex2 =
                        Convex.fromBlock 0.5 0.5 0.5

                    transform3d1 =
                        Transform3d.atPoint { x = -0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 2)

                    transform3d2 =
                        Transform3d.atPoint { x = 0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                in
                Collision.ConvexConvex.addContacts
                    (Convex.placeIn transform3d1 convex1)
                    (Convex.placeIn transform3d2 convex2)
                    []
                    |> List.length
                    |> Expect.equal 2
        ]


testSeparatingAxis : Test
testSeparatingAxis =
    describe "Collision.ConvexConvex.testSeparatingAxis"
        [ test "returns Just depth" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis convex1 convex2 Vec3.xAxis)
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = -5.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis convex1 convex2 Vec3.xAxis)
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    let
                        convex1 =
                            Convex.fromBlock 0.5 0.5 0.5
                                |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })

                        convex2 =
                            Convex.fromBlock 0.5 0.5 0.5
                                |> Convex.placeIn
                                    (Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                                        |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                                    )
                    in
                    Collision.ConvexConvex.testSeparatingAxis convex1 convex2 Vec3.xAxis
                of
                    Nothing ->
                        Expect.fail "expected depth"

                    Just value ->
                        Expect.within (Expect.Absolute 0.00001) 0.4071067 value
        ]


findSeparatingAxis : Test
findSeparatingAxis =
    describe "Collision.ConvexConvex.findSeparatingAxis"
        [ test "works for offset" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis convex1 convex2)
                    (Just { x = -1, y = 0, z = 0 })
        , test "works for rotation" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn
                                (Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                                    |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                                )
                in
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis convex1 convex2)
                    (Just { x = -1, y = 0, z = 0 })
        ]


project : Test
project =
    describe "Collision.ConvexConvex.project"
        [ test "works for the positive x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.xAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        { x = -1, y = 0, z = 0 }
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 0.5 0.5 0.5
                            |> Convex.placeIn (Transform3d.atPoint { x = 0, y = 1, z = 0 })
                        ).vertices
                    )
                    { min = 0.5, max = 1.5 }
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    Vec3.yAxis
                    Const.maxNumber
                    -Const.maxNumber
                    (Convex.fromBlock 0.5 0.5 0.5
                        |> Convex.placeIn
                            (Transform3d.atPoint { x = 0, y = 1, z = 0 }
                                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 2)
                            )
                    ).vertices
                    |> Expect.all
                        [ .min >> Expect.within (Expect.Absolute 0.00001) 0.5
                        , .max >> Expect.within (Expect.Absolute 0.00001) 1.5
                        ]
        ]
