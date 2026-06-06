module Collision.ConvexConvexTest exposing
    ( addContacts
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Collision.ConvexConvex
import Expect
import Internal.Const as Const
import Internal.ContactId as ContactId
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
                Collision.ConvexConvex.addContacts 0 (Convex.placeIn t1 convex) (Convex.placeIn t2 convex) []
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
                Collision.ConvexConvex.addContacts 0
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
                Collision.ConvexConvex.addContacts 0 box1 box2 []
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
                Collision.ConvexConvex.addContacts 0 box1 box2 []
                    |> List.all (\c -> not (String.startsWith "-e" (ContactId.featureString c.featureKey)))
                    |> Expect.equal True
        , test "settled near-coplanar stack interface stays at four contacts (no degenerate over-count)" <|
            -- Real settling poses where the clip emitted 6 points (4 corners + 2 on
            -- the shared edges); ~1e-4 drift triggers it, exact coplanarity doesn't.
            \_ ->
                let
                    box2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.fromOriginAndBasis
                                    { x = 0.0002598859790008731, y = 0.00013646414197537694, z = 1.4996999282620576 }
                                    { x = 0.999999999331571, y = 0.0000032595970446131582, z = -0.00003641748174313937 }
                                    { x = -0.0000032582908802255252, y = 0.999999999351492, z = 0.00003586641330410744 }
                                    { x = 0.00003641759862957714, y = -0.000035866294621384654, z = 0.9999999986936836 }
                                )

                    box3 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.fromOriginAndBasis
                                    { x = 0.0001967863351834402, y = 0.000008252933652183521, z = 2.499616662498541 }
                                    { x = 0.9999999861179005, y = 0.000002032073717667724, z = 0.00016661353315350634 }
                                    { x = -0.0000020688652840958605, y = 0.9999999756171408, z = 0.00022081992116338665 }
                                    { x = -0.00016661308036863387, y = -0.00022082026279889716, z = 0.9999999617392458 }
                                )
                in
                Collision.ConvexConvex.addContacts 0 box2 box3 []
                    |> List.length
                    |> Expect.equal 4
        , test "tilted box with edge crossing target top face emits two face-face contacts" <|
            \_ ->
                Collision.ConvexConvex.addContacts 0
                    (tiltedBox { x = -0.11608751888227789, y = 0.5683352706207844, z = 1.9053833288923054 })
                    targetBox
                    []
                    |> List.map (\c -> ContactId.featureString c.featureKey)
                    |> List.sort
                    |> Expect.equal [ "-f3-f5-v5", "-f3-f5-v6" ]
        , test "same tilt at shifted y-offset emits two face-face contacts" <|
            \_ ->
                Collision.ConvexConvex.addContacts 0
                    (tiltedBox { x = -0.06790182997043828, y = -0.6276944695546394, z = 1.9053833288923054 })
                    targetBox
                    []
                    |> List.map (\c -> ContactId.featureString c.featureKey)
                    |> List.sort
                    |> Expect.equal [ "-f3-f5-v5", "-f3-f5-v6" ]
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
                        (Convex.convexVertices (Convex.fromBlock 1 1 1))
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        { x = -1, y = 0, z = 0 }
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.convexVertices (Convex.fromBlock 1 1 1))
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.convexVertices (Convex.fromBlock 1 1 1))
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
                            |> Convex.convexVertices
                        )
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
                        |> Convex.convexVertices
                    )
                    |> Expect.all
                        [ .min >> Expect.within (Expect.Absolute 0.00001) 0.5
                        , .max >> Expect.within (Expect.Absolute 0.00001) 1.5
                        ]
        ]
