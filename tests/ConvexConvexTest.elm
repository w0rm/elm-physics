module ConvexConvexTest exposing
    ( findSeparatingAxis
    , getContacts
    , project
    , testSeparatingAxis
    )

import Collision.ConvexConvex
import Expect exposing (Expectation)
import Internal.Convex as Convex exposing (Convex)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform
import Internal.Vector3 as Vec3 exposing (Vec3)
import Test exposing (..)


getContacts : Test
getContacts =
    describe "Collision.ConvexConvex.addContacts"
        [ test "should return 4 results" <|
            \_ ->
                let
                    convex =
                        Convex.fromBox { x = 1, y = 1, z = 1 }

                    t1 =
                        { position = { x = 0, y = 0, z = 2.1 } -- going slightly into another box
                        , orientation = Quaternion.fromAngleAxis (pi / 2) Vec3.j
                        }

                    t2 =
                        { position = { x = 0, y = 0, z = 4 }
                        , orientation = Quaternion.fromAngleAxis (pi / 2) Vec3.j
                        }
                in
                Collision.ConvexConvex.addContacts t1 convex t2 convex []
                    |> List.length
                    |> Expect.equal 4
        , test "should return 2 results" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBox { x = 0.6, y = 0.6, z = 0.6 }

                    convex2 =
                        Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }

                    transform1 =
                        { position = { x = -0.5, y = 0, z = 0 }
                        , orientation = Quaternion.fromAngleAxis (pi / 2) Vec3.k
                        }

                    transform2 =
                        { position = { x = 0.5, y = 0, z = 0 }
                        , orientation = Quaternion.fromAngleAxis (pi / 4) Vec3.k
                        }
                in
                Collision.ConvexConvex.addContacts transform1 convex1 transform2 convex2 []
                    |> List.length
                    |> Expect.equal 2
        , test "should work for the case from the debugger" <|
            \_ ->
                let
                    convex =
                        Convex.fromBox { x = 1, y = 1, z = 1 }

                    transform1 =
                        { position = { x = -2.9496035986031215, y = -0.059705884468658266, z = 0.05803282809897854 }
                        , orientation = { x = -0.022809298766761247, y = 0.006783793446053796, z = 0.002763745916207627, w = 0.9997129976872166 }
                        }

                    transform2 =
                        { position = { x = -1.7732501140437167, y = -0.23893989356833145, z = 1.9746722038817583 }
                        , orientation = { x = -0.14987379072976215, y = 0.5294480629310288, z = 0.19937553795533458, w = -0.8108464653532712 }
                        }
                in
                Collision.ConvexConvex.addContacts transform1 convex transform2 convex []
                    |> List.length
                    |> Expect.equal 1
        ]


testSeparatingAxis : Test
testSeparatingAxis =
    describe "Collision.ConvexConvex.testSeparatingAxis"
        [ test "returns Just depth" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis
                        { transform1 =
                            { position = { x = -0.2, y = 0, z = 0 }
                            , orientation = Quaternion.identity
                            }
                        , convex1 = Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }
                        , transform2 =
                            { position = { x = 0.2, y = 0, z = 0 }
                            , orientation = Quaternion.identity
                            }
                        , convex2 = Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }
                        }
                        Vec3.i
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis
                        { transform1 =
                            { position = { x = -5.2, y = 0, z = 0 }
                            , orientation = Quaternion.identity
                            }
                        , convex1 = Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }
                        , transform2 =
                            { position = { x = 0.2, y = 0, z = 0 }
                            , orientation = Quaternion.identity
                            }
                        , convex2 = Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }
                        }
                        Vec3.i
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    Collision.ConvexConvex.testSeparatingAxis
                        { transform1 =
                            { position = Vec3.i
                            , orientation = Quaternion.identity
                            }
                        , convex1 = Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }
                        , transform2 =
                            { position = { x = 0.2, y = 0, z = 0 }
                            , orientation = Quaternion.fromAngleAxis (pi / 4) Vec3.k
                            }
                        , convex2 = Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }
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
                        { position = { x = -0.2, y = 0, z = 0 }
                        , orientation = Quaternion.identity
                        }
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 })
                        { position = { x = 0.2, y = 0, z = 0 }
                        , orientation = Quaternion.identity
                        }
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 })
                    )
                    (Just { x = -1, y = 0, z = 0 })
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis
                        { position = { x = -0.2, y = 0, z = 0 }
                        , orientation = Quaternion.identity
                        }
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 })
                        { position = { x = 0.2, y = 0, z = 0 }
                        , orientation = Quaternion.fromAngleAxis (pi / 4) Vec3.k
                        }
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 })
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
                        Transform.identity
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }).vertices
                        Vec3.i
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }).vertices
                        { x = -1, y = 0, z = 0 }
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }).vertices
                        Vec3.j
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        { orientation = Quaternion.identity
                        , position = Vec3.j
                        }
                        (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }).vertices
                        Vec3.j
                    )
                    { min = 0.5, max = 1.5 }
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    { orientation = Quaternion.fromAngleAxis (pi / 2) Vec3.i
                    , position = Vec3.j
                    }
                    (Convex.fromBox { x = 0.5, y = 0.5, z = 0.5 }).vertices
                    Vec3.j
                    |> Expect.all
                        [ .min >> Expect.within (Expect.Absolute 0.00001) 0.5
                        , .max >> Expect.within (Expect.Absolute 0.00001) 1.5
                        ]
        ]
