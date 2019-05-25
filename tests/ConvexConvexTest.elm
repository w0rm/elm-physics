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
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (..)


getContacts : Test
getContacts =
    describe "Collision.ConvexConvex.addContacts"
        [ test "should return 4 results" <|
            \_ ->
                let
                    convex =
                        Convex.fromBox (vec3 1 1 1)

                    t1 =
                        { position = vec3 0 0 2.1 -- going slightly into another box
                        , orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }

                    t2 =
                        { position = vec3 0 0 4
                        , orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }
                in
                Collision.ConvexConvex.addContacts t1 convex t2 convex []
                    |> List.length
                    |> Expect.equal 4
        , test "should return 2 results" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBox (vec3 0.6 0.6 0.6)

                    convex2 =
                        Convex.fromBox (vec3 0.5 0.5 0.5)

                    transform1 =
                        { position = vec3 -0.5 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 0 0 1)
                        }

                    transform2 =
                        { position = vec3 0.5 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                in
                Collision.ConvexConvex.addContacts transform1 convex1 transform2 convex2 []
                    |> List.length
                    |> Expect.equal 2
        , test "should work for the case from the debugger" <|
            \_ ->
                let
                    convex =
                        Convex.fromBox (vec3 1 1 1)

                    transform1 =
                        { position = vec3 -2.9496035986031215 -0.059705884468658266 0.05803282809897854
                        , orientation = { x = -0.022809298766761247, y = 0.006783793446053796, z = 0.002763745916207627, w = 0.9997129976872166 }
                        }

                    transform2 =
                        { position = vec3 -1.7732501140437167 -0.23893989356833145 1.9746722038817583
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
                            { position = vec3 -0.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , convex1 = Convex.fromBox (vec3 0.5 0.5 0.5)
                        , transform2 =
                            { position = vec3 0.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , convex2 = Convex.fromBox (vec3 0.5 0.5 0.5)
                        }
                        (vec3 1 0 0)
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSeparatingAxis
                        { transform1 =
                            { position = vec3 -5.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , convex1 = Convex.fromBox (vec3 0.5 0.5 0.5)
                        , transform2 =
                            { position = vec3 0.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , convex2 = Convex.fromBox (vec3 0.5 0.5 0.5)
                        }
                        (vec3 1 0 0)
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    Collision.ConvexConvex.testSeparatingAxis
                        { transform1 =
                            { position = vec3 1 0 0
                            , orientation = Quaternion.identity
                            }
                        , convex1 = Convex.fromBox (vec3 0.5 0.5 0.5)
                        , transform2 =
                            { position = vec3 0.2 0 0
                            , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                            }
                        , convex2 = Convex.fromBox (vec3 0.5 0.5 0.5)
                        }
                        (vec3 1 0 0)
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
                        { position = vec3 -0.2 0 0
                        , orientation = Quaternion.identity
                        }
                        (Convex.fromBox (vec3 0.5 0.5 0.5))
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.identity
                        }
                        (Convex.fromBox (vec3 0.5 0.5 0.5))
                    )
                    (Just (vec3 -1 0 0))
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis
                        { position = vec3 -0.2 0 0
                        , orientation = Quaternion.identity
                        }
                        (Convex.fromBox (vec3 0.5 0.5 0.5))
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (Convex.fromBox (vec3 0.5 0.5 0.5))
                    )
                    (Just (vec3 -1 0 0))
        ]


project : Test
project =
    describe "Collision.ConvexConvex.project"
        [ test "works for the positive x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (Convex.fromBox (vec3 0.5 0.5 0.5)).vertices
                        (vec3 1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (Convex.fromBox (vec3 0.5 0.5 0.5)).vertices
                        (vec3 -1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (Convex.fromBox (vec3 0.5 0.5 0.5)).vertices
                        (vec3 0 1 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        { orientation = Quaternion.identity
                        , position = vec3 0 1 0
                        }
                        (Convex.fromBox (vec3 0.5 0.5 0.5)).vertices
                        (vec3 0 1 0)
                    )
                    ( 1.5, 0.5 )
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    { orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 1 0 0)
                    , position = vec3 0 1 0
                    }
                    (Convex.fromBox (vec3 0.5 0.5 0.5)).vertices
                    (vec3 0 1 0)
                    |> Expect.all
                        [ Tuple.first >> Expect.within (Expect.Absolute 0.00001) 1.5
                        , Tuple.second >> Expect.within (Expect.Absolute 0.00001) 0.5
                        ]
        ]
