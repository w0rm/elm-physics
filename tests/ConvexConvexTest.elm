module ConvexConvexTest exposing
    ( findSeparatingAxis
    , getContacts
    , project
    , testSeparatingAxis
    )

import Collision.ConvexConvex
import Expect
import Internal.Convex as Convex
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Test exposing (Test, describe, test)


getContacts : Test
getContacts =
    describe "Collision.ConvexConvex.addContacts"
        [ test "should return 4 results" <|
            \_ ->
                let
                    convex =
                        Convex.fromBlock 1 1 1

                    t1 =
                        -- going slightly into another box
                        Transform3d.atPoint { x = 0, y = 0, z = 2.1 }
                            |> Transform3d.rotateAroundOwn Vec3.j (pi / 2)

                    t2 =
                        Transform3d.atPoint { x = 0, y = 0, z = 4 }
                            |> Transform3d.rotateAroundOwn Vec3.j (pi / 2)
                in
                Collision.ConvexConvex.addContacts t1 convex t2 convex []
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
                            |> Transform3d.rotateAroundOwn Vec3.k (pi / 2)

                    transform3d2 =
                        Transform3d.atPoint { x = 0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.k (pi / 4)
                in
                Collision.ConvexConvex.addContacts transform3d1 convex1 transform3d2 convex2 []
                    |> List.length
                    |> Expect.equal 2
        ]


testSeparatingAxis : Test
testSeparatingAxis =
    describe "Collision.ConvexConvex.testSeparatingAxis"
        [ test "returns Just depth" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis
                        { transform3d1 = Transform3d.atPoint { x = -0.2, y = 0, z = 0 }
                        , convex1 = Convex.fromBlock 0.5 0.5 0.5
                        , transform3d2 = Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                        , convex2 = Convex.fromBlock 0.5 0.5 0.5
                        }
                        Vec3.i
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis
                        { transform3d1 = Transform3d.atPoint { x = -5.2, y = 0, z = 0 }
                        , convex1 = Convex.fromBlock 0.5 0.5 0.5
                        , transform3d2 = Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                        , convex2 = Convex.fromBlock 0.5 0.5 0.5
                        }
                        Vec3.i
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    Collision.ConvexConvex.testSeparatingAxis
                        { transform3d1 = Transform3d.atPoint { x = 1, y = 0, z = 0 }
                        , convex1 = Convex.fromBlock 0.5 0.5 0.5
                        , transform3d2 =
                            Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                                |> Transform3d.rotateAroundOwn Vec3.k (pi / 4)
                        , convex2 = Convex.fromBlock 0.5 0.5 0.5
                        }
                        Vec3.i
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
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis
                        (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })
                        (Convex.fromBlock 0.5 0.5 0.5)
                        (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                        (Convex.fromBlock 0.5 0.5 0.5)
                    )
                    (Just { x = -1, y = 0, z = 0 })
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis
                        (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })
                        (Convex.fromBlock 0.5 0.5 0.5)
                        (Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.k (pi / 4)
                        )
                        (Convex.fromBlock 0.5 0.5 0.5)
                    )
                    (Just { x = -1, y = 0, z = 0 })
        ]


project : Test
project =
    describe "Collision.ConvexConvex.project"
        [ test "works for the positive x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform3d.atOrigin
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        Vec3.i
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform3d.atOrigin
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        { x = -1, y = 0, z = 0 }
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform3d.atOrigin
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        Vec3.j
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        (Transform3d.atPoint { x = 0, y = 1, z = 0 })
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        Vec3.j
                    )
                    { min = 0.5, max = 1.5 }
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    (Transform3d.atPoint { x = 0, y = 1, z = 0 }
                        |> Transform3d.rotateAroundOwn Vec3.i (pi / 2)
                    )
                    (Convex.fromBlock 0.5 0.5 0.5).vertices
                    Vec3.j
                    |> Expect.all
                        [ .min >> Expect.within (Expect.Absolute 0.00001) 0.5
                        , .max >> Expect.within (Expect.Absolute 0.00001) 1.5
                        ]
        ]
