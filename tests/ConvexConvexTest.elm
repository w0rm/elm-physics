module ConvexConvexTest exposing
    ( clipAgainstHull
    , clipFaceAgainstHull
    , clipFaceAgainstPlane
    , findSeparatingAxis
    , project
    , testSepAxis
    )

import Array exposing (Array)
import Collision.ConvexConvex
import Expect exposing (Expectation)
import Fixtures.ConvexPolyhedron exposing (boxHull)
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (..)


clipFaceAgainstPlane : Test
clipFaceAgainstPlane =
    describe "Collision.ConvexConvex.clipFaceAgainstPlane"
        [ test "should return 4 results" <|
            -- Four points 1 unit below the plane z=0
            -- we assume to get back 4
            \_ ->
                Collision.ConvexConvex.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    0
                    [ vec3 -0.2 -0.2 -1
                    , vec3 -0.2 0.2 -1
                    , vec3 0.2 0.2 -1
                    , vec3 0.2 -0.2 -1
                    ]
                    |> Expect.equal
                        [ vec3 -0.2 -0.2 -1
                        , vec3 0.2 -0.2 -1
                        , vec3 0.2 0.2 -1
                        , vec3 -0.2 0.2 -1
                        ]
        , test "should return no results" <|
            -- Lower the plane to z=-2
            -- we assume no points back
            \_ ->
                Collision.ConvexConvex.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    2
                    [ vec3 -0.2 -0.2 -1
                    , vec3 -0.2 0.2 -1
                    , vec3 0.2 0.2 -1
                    , vec3 0.2 -0.2 -1
                    ]
                    |> Expect.equal []
        , test "should return 4 results for two points below, two over" <|
            -- two points below, two over. We get four points back,
            -- though 2 of them are clipped to the back of the plane
            \_ ->
                Collision.ConvexConvex.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    0
                    [ vec3 -2 -2 1
                    , vec3 -2 2 1
                    , vec3 2 2 -1
                    , vec3 2 -2 -1
                    ]
                    |> Expect.equal
                        [ vec3 0 -2 0
                        , vec3 2 -2 -1
                        , vec3 2 2 -1
                        , vec3 0 2 0
                        ]
        ]


clipFaceAgainstHull : Test
clipFaceAgainstHull =
    describe "Collision.ConvexConvex.clipFaceAgainstHull"
        [ test "should return 4 results" <|
            \_ ->
                let
                    sepNormal =
                        vec3 0 0 1

                    -- Move the box 0.45 units up
                    -- only 0.05 units of the box will be below plane z=0
                    transform =
                        { position = vec3 0 0 0.45
                        , orientation = Quaternion.identity
                        }

                    -- points in the plane z
                    worldVertsB =
                        [ vec3 -1.0 -1.0 0
                        , vec3 -1.0 1.0 0
                        , vec3 1.0 1.0 0
                        , vec3 1.0 -1.0 0
                        ]

                    -- We will now clip a face in hullA that is closest to the sepNormal
                    -- against the points in worldVertsB.
                    -- We can expect to get back the 4 corners of the box hullA penetrated 0.05 units
                    -- into the plane worldVertsB we constructed
                in
                {-
                   [ { point: Vec3 { x: 0.5, y: -0.5, z: 0 },
                         normal: Vec3 { x: 0, y: 0, z: -1 },
                         depth: -0.04999999999999999 },
                     { point: Vec3 { x: -0.5, y: -0.5, z: 0 },
                         normal: Vec3 { x: 0, y: 0, z: -1 },
                         depth: -0.04999999999999999 },
                     { point: Vec3 { x: -0.5, y: 0.5, z: 0 },
                         normal: Vec3 { x: 0, y: 0, z: -1 },
                         depth: -0.04999999999999999 },
                     { point: Vec3 { x: 0.5, y: 0.5, z: 0 },
                         normal: Vec3 { x: 0, y: 0, z: -1 },
                         depth: -0.04999999999999999 } ]
                -}
                Collision.ConvexConvex.clipFaceAgainstHull
                    transform
                    (boxHull 0.5)
                    sepNormal
                    worldVertsB
                    -100
                    100
                    |> List.length
                    |> Expect.equal 4
        ]


clipAgainstHull : Test
clipAgainstHull =
    describe "Collision.ConvexConvex.clipAgainstHull"
        [ test "should return 4 results" <|
            \_ ->
                let
                    hull1 =
                        boxHull 1

                    hull2 =
                        boxHull 1

                    t1 =
                        { position = vec3 0 0 2.1 -- going slightly into another box
                        , orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }

                    t2 =
                        { position = vec3 0 0 4
                        , orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }
                in
                {-
                   [ { point: Vec3 { x: 0.9999999999999997, y: 1, z: 3.0000000000000004 },
                       normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                       depth: -0.09999999999999964 },
                     { point: Vec3 { x: 0.9999999999999998, y: -1, z: 3.0000000000000004 },
                       normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                       depth: -0.09999999999999964 },
                     { point: Vec3 { x: -0.9999999999999997, y: -1, z: 3 },
                       normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                       depth: -0.10000000000000009 },
                     { point: Vec3 { x: -0.9999999999999997, y: 1, z: 3 },
                       normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                       depth: -0.10000000000000009 } ]
                -}
                case Collision.ConvexConvex.findSeparatingAxis t1 hull1 t2 hull2 of
                    Just separatingAxis ->
                        Collision.ConvexConvex.clipAgainstHull t1 hull1 t2 hull2 separatingAxis -100 100
                            |> List.length
                            |> Expect.equal 4

                    Nothing ->
                        Expect.fail "Couldn't find separate axis"
        , test "should return 2 results" <|
            \_ ->
                let
                    hull1 =
                        boxHull 0.6

                    hull2 =
                        boxHull 0.5

                    t1 =
                        { position = vec3 -0.5 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 0 0 1)
                        }

                    t2 =
                        { position = vec3 0.5 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                in
                {-
                   [ { point: Vec3 { x: -0.20710678118654746, y: -5.551115123125783e-17, z: -0.5 },
                       normal: Vec3 { x: 0.9999999999999999, y: 0, z: 0 },
                       depth: -0.30710678118654733 },
                     { point: Vec3 { x: -0.20710678118654746, y: -5.551115123125783e-17, z: 0.5 },
                       normal: Vec3 { x: 0.9999999999999999, y: 0, z: 0 },
                       depth: -0.30710678118654733 } ]
                -}
                case Collision.ConvexConvex.findSeparatingAxis t1 hull1 t2 hull2 of
                    Just separatingAxis ->
                        Collision.ConvexConvex.clipAgainstHull t1 hull1 t2 hull2 separatingAxis -100 100
                            |> List.length
                            |> Expect.equal 2

                    Nothing ->
                        Expect.fail "Couldn't find separate axis"
        , test "should work for the case from the debugger" <|
            \_ ->
                let
                    hull =
                        boxHull 1

                    t1 =
                        { position = vec3 -2.9496035986031215 -0.059705884468658266 0.05803282809897854
                        , orientation = { x = -0.022809298766761247, y = 0.006783793446053796, z = 0.002763745916207627, w = 0.9997129976872166 }
                        }

                    t2 =
                        { position = vec3 -1.7732501140437167 -0.23893989356833145 1.9746722038817583
                        , orientation = { x = -0.14987379072976215, y = 0.5294480629310288, z = 0.19937553795533458, w = -0.8108464653532712 }
                        }

                    maybeSeparatingAxis =
                        Collision.ConvexConvex.findSeparatingAxis t1 hull t2 hull
                in
                case maybeSeparatingAxis of
                    Just separatingAxis ->
                        Collision.ConvexConvex.clipAgainstHull t1 hull t2 hull separatingAxis -100 100
                            |> List.length
                            |> Expect.equal 1

                    {-
                       [ { point: Vec3 { x: -1.9395931897893413, y: -0.620034911301545, z: 0.567836561523491 },
                           normal: Vec3 { x: 0.013437614750654274, y: 0.04564300225339029, z: 0.9988674320724998 },
                           depth: -0.502776622199867 } ]
                    -}
                    Nothing ->
                        Expect.fail "Couldn't find separate axis"
        ]


testSepAxis : Test
testSepAxis =
    describe "Collision.ConvexConvex.testSepAxis"
        [ test "returns Just depth" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSepAxis
                        { t1 =
                            { position = vec3 -0.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , hull1 = boxHull 0.5
                        , t2 =
                            { position = vec3 0.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , hull2 = boxHull 0.5
                        }
                        (vec3 1 0 0)
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.testSepAxis
                        { t1 =
                            { position = vec3 -5.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , hull1 = boxHull 0.5
                        , t2 =
                            { position = vec3 0.2 0 0
                            , orientation = Quaternion.identity
                            }
                        , hull2 = boxHull 0.5
                        }
                        (vec3 1 0 0)
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    Collision.ConvexConvex.testSepAxis
                        { t1 =
                            { position = vec3 1 0 0
                            , orientation = Quaternion.identity
                            }
                        , hull1 = boxHull 0.5
                        , t2 =
                            { position = vec3 0.2 0 0
                            , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                            }
                        , hull2 = boxHull 0.5
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
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.identity
                        }
                        (boxHull 0.5)
                    )
                    (Just (vec3 -1 0 0))
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis
                        { position = vec3 -0.2 0 0
                        , orientation = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (boxHull 0.5)
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
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 -1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Transform.identity
                        (boxHull 0.5)
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
                        (boxHull 0.5)
                        (vec3 0 1 0)
                    )
                    ( 1.5, 0.5 )
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    { orientation = Quaternion.fromAngleAxis (pi / 2) (vec3 1 0 0)
                    , position = vec3 0 1 0
                    }
                    (boxHull 0.5)
                    (vec3 0 1 0)
                    |> Expect.all
                        [ Tuple.first >> Expect.within (Expect.Absolute 0.00001) 1.5
                        , Tuple.second >> Expect.within (Expect.Absolute 0.00001) 0.5
                        ]
        ]
