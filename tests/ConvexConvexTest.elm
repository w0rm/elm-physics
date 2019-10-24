module ConvexConvexTest exposing
    ( findSeparatingAxis
    , getContacts
    , project
    , testSeparatingAxis
    )

import Angle
import Axis3d
import Collision.ConvexConvex
import Expect
import Frame3d
import Internal.Convex as Convex
import Internal.Vector3 as Vec3
import Point3d
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
                        Frame3d.atPoint Point3d.origin
                            |> Frame3d.rotateAround Axis3d.y (Angle.radians (pi / 2))
                            |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 2.1 })

                    t2 =
                        Frame3d.atPoint Point3d.origin
                            |> Frame3d.rotateAround Axis3d.y (Angle.radians (pi / 2))
                            |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 4 })
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

                    frame3d1 =
                        Frame3d.atPoint Point3d.origin
                            |> Frame3d.rotateAround Axis3d.z (Angle.radians (pi / 2))
                            |> Frame3d.moveTo (Point3d.fromMeters { x = -0.5, y = 0, z = 0 })

                    frame3d2 =
                        Frame3d.atPoint Point3d.origin
                            |> Frame3d.rotateAround Axis3d.z (Angle.radians (pi / 4))
                            |> Frame3d.moveTo (Point3d.fromMeters { x = 0.5, y = 0, z = 0 })
                in
                Collision.ConvexConvex.addContacts frame3d1 convex1 frame3d2 convex2 []
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
                        { frame3d1 = Frame3d.atPoint (Point3d.fromMeters { x = -0.2, y = 0, z = 0 })
                        , convex1 = Convex.fromBlock 0.5 0.5 0.5
                        , frame3d2 = Frame3d.atPoint (Point3d.fromMeters { x = 0.2, y = 0, z = 0 })
                        , convex2 = Convex.fromBlock 0.5 0.5 0.5
                        }
                        Vec3.i
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis
                        { frame3d1 = Frame3d.atPoint (Point3d.fromMeters { x = -5.2, y = 0, z = 0 })
                        , convex1 = Convex.fromBlock 0.5 0.5 0.5
                        , frame3d2 = Frame3d.atPoint (Point3d.fromMeters { x = 0.2, y = 0, z = 0 })
                        , convex2 = Convex.fromBlock 0.5 0.5 0.5
                        }
                        Vec3.i
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    Collision.ConvexConvex.testSeparatingAxis
                        { frame3d1 = Frame3d.atPoint (Point3d.fromMeters { x = 1, y = 0, z = 0 })
                        , convex1 = Convex.fromBlock 0.5 0.5 0.5
                        , frame3d2 =
                            Frame3d.atPoint Point3d.origin
                                |> Frame3d.rotateAround Axis3d.z (Angle.radians (pi / 4))
                                |> Frame3d.moveTo (Point3d.fromMeters { x = 0.2, y = 0, z = 0 })
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
                        (Frame3d.atPoint (Point3d.fromMeters { x = -0.2, y = 0, z = 0 }))
                        (Convex.fromBlock 0.5 0.5 0.5)
                        (Frame3d.atPoint (Point3d.fromMeters { x = 0.2, y = 0, z = 0 }))
                        (Convex.fromBlock 0.5 0.5 0.5)
                    )
                    (Just { x = -1, y = 0, z = 0 })
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis
                        (Frame3d.atPoint (Point3d.fromMeters { x = -0.2, y = 0, z = 0 }))
                        (Convex.fromBlock 0.5 0.5 0.5)
                        (Frame3d.atPoint Point3d.origin
                            |> Frame3d.rotateAround Axis3d.z (Angle.radians (pi / 4))
                            |> Frame3d.moveTo (Point3d.fromMeters { x = 0.2, y = 0, z = 0 })
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
                        (Frame3d.atPoint Point3d.origin)
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        Vec3.i
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        (Frame3d.atPoint Point3d.origin)
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        { x = -1, y = 0, z = 0 }
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        (Frame3d.atPoint Point3d.origin)
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        Vec3.j
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        (Frame3d.atPoint (Point3d.fromMeters { x = 0, y = 1, z = 0 }))
                        (Convex.fromBlock 0.5 0.5 0.5).vertices
                        Vec3.j
                    )
                    { min = 0.5, max = 1.5 }
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    (Frame3d.atPoint Point3d.origin
                        |> Frame3d.rotateAround Axis3d.x (Angle.radians (pi / 2))
                        |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 1, z = 0 })
                    )
                    (Convex.fromBlock 0.5 0.5 0.5).vertices
                    Vec3.j
                    |> Expect.all
                        [ .min >> Expect.within (Expect.Absolute 0.00001) 0.5
                        , .max >> Expect.within (Expect.Absolute 0.00001) 1.5
                        ]
        ]
