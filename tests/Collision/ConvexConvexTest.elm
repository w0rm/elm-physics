module Collision.ConvexConvexTest exposing
    ( addContacts
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Collision.ConvexConvex
import Expect
import Internal.Const as Const
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    let
        -- Captured from the Collisions sandbox: a 1.5-cube whose local
        -- frame is ~47° off identity plus a ~0.3° tilt. Only the
        -- origin differs between the two scenarios below.
        tiltedBox origin =
            Convex.fromBlock 1.5 1.5 1.5
                |> Convex.placeIn
                    (Transform3d.fromOriginAndBasis
                        origin
                        { x = -0.001580161067349219, y = 0.9999882020975505, z = -0.004593338296621986 }
                        { x = -0.6852663896092609, y = -0.004428115539111141, z = -0.7282790447793068 }
                        { x = -0.7282907924468673, y = 0.0019968621580540736, z = 0.6852653020390244 }
                    )

        targetBox =
            Convex.fromBlock 2 2 2
                |> Convex.placeIn Transform3d.atOrigin
    in
    describe "Collision.ConvexConvex.addContacts"
        [ test "should return 4 results" <|
            \_ ->
                let
                    convex =
                        Convex.fromBlock 2 2 2

                    t1 =
                        -- going slightly into another box
                        Transform3d.atPoint { x = 0, y = 0, z = 2.1 }
                            |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)

                    t2 =
                        Transform3d.atPoint { x = 0, y = 0, z = 4 }
                            |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)
                in
                Collision.ConvexConvex.addContacts "" (Convex.placeIn t1 convex) (Convex.placeIn t2 convex) []
                    |> List.length
                    |> Expect.equal 4
        , test "should return 2 results" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1.2 1.2 1.2

                    convex2 =
                        Convex.fromBlock 1 1 1

                    transform3d1 =
                        Transform3d.atPoint { x = -0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 2)

                    transform3d2 =
                        Transform3d.atPoint { x = 0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                in
                Collision.ConvexConvex.addContacts ""
                    (Convex.placeIn transform3d1 convex1)
                    (Convex.placeIn transform3d2 convex2)
                    []
                    |> List.length
                    |> Expect.equal 2
        , test "produces a single edge-edge contact when neither body's face normal is the SAT min" <|
            \_ ->
                let
                    box1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.atOrigin
                                    |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 6)
                                )

                    box2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.atPoint { x = 0.6, y = 0.6, z = 0.6 }
                                    |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 6)
                                )
                in
                Collision.ConvexConvex.addContacts "" box1 box2 []
                    |> List.length
                    |> Expect.equal 1
        , test "axis-aligned corner-on-corner stays on the face-clip path" <|
            \_ ->
                let
                    box1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn Transform3d.atOrigin

                    box2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.7, y = 0.7, z = 0.7 })
                in
                Collision.ConvexConvex.addContacts "" box1 box2 []
                    |> List.all (\c -> not (String.startsWith "-e" c.id))
                    |> Expect.equal True
        , test "tilted box with edge crossing target top face emits two face-face contacts" <|
            \_ ->
                Collision.ConvexConvex.addContacts ""
                    (tiltedBox { x = -0.11608751888227789, y = 0.5683352706207844, z = 1.9053833288923054 })
                    targetBox
                    []
                    |> List.map .id
                    |> Expect.equal [ "-f3-f5-v2", "-f3-f5-v1" ]
        , test "same tilt at shifted y-offset emits two face-face contacts" <|
            \_ ->
                Collision.ConvexConvex.addContacts ""
                    (tiltedBox { x = -0.06790182997043828, y = -0.6276944695546394, z = 1.9053833288923054 })
                    targetBox
                    []
                    |> List.map .id
                    |> Expect.equal [ "-f3-f5-v3", "-f3-f5-v2" ]
        ]


testSeparatingAxis : Test
testSeparatingAxis =
    describe "Collision.ConvexConvex.testSeparatingAxis"
        [ test "returns Just depth" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis convex1 convex2 Vec3.xAxis)
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -5.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
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
                            Convex.fromBlock 1 1 1
                                |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })

                        convex2 =
                            Convex.fromBlock 1 1 1
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
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis convex1 convex2)
                    (Just { x = -1, y = 0, z = 0 })
        , test "works for rotation" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
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
                        (Convex.fromBlock 1 1 1).vertices
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        { x = -1, y = 0, z = 0 }
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 1 1 1).vertices
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 1 1 1).vertices
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 1 1 1
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
                    (Convex.fromBlock 1 1 1
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
