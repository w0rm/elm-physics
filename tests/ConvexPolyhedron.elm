module ConvexPolyhedron exposing (..)

import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Expect exposing (Expectation)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)
import Test exposing (..)
import Array.Hamt as Array


clipFaceAgainstPlane : Test
clipFaceAgainstPlane =
    describe "ConvexPolyhedron.clipFaceAgainstPlane"
        [ test "should return 4 results" <|
            -- Four points 1 unit below the plane z=0
            -- we assume to get back 4
            \_ ->
                ConvexPolyhedron.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    0
                    [ vec3 -0.2 -0.2 -1
                    , vec3 -0.2 0.2 -1
                    , vec3 0.2 0.2 -1
                    , vec3 0.2 -0.2 -1
                    ]
                    |> Expect.equal
                        [ vec3 -0.2 -0.2 -1
                        , vec3 -0.2 0.2 -1
                        , vec3 0.2 0.2 -1
                        , vec3 0.2 -0.2 -1
                        ]
        , test "should return no results" <|
            -- Lower the plane to z=-2
            -- we assume no points back
            \_ ->
                ConvexPolyhedron.clipFaceAgainstPlane
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
                ConvexPolyhedron.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    0
                    [ vec3 -2 -2 1
                    , vec3 -2 2 1
                    , vec3 2 2 -1
                    , vec3 2 -2 -1
                    ]
                    |> Expect.equal
                        [ vec3 0 -2 0
                        , vec3 0 2 0
                        , vec3 2 2 -1
                        , vec3 2 -2 -1
                        ]
        ]


clipFaceAgainstHull : Test
clipFaceAgainstHull =
    describe "ConvexPolyhedron.clipFaceAgainstHull"
        [ test "should return 4 results" <|
            \_ ->
                let
                    sepNormal =
                        vec3 0 0 1

                    -- Move the box 0.45 units up
                    -- only 0.05 units of the box will be below plane z=0
                    transform =
                        { position = vec3 0 0 0.45
                        , quaternion = Quaternion.identity
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
                    ConvexPolyhedron.clipFaceAgainstHull
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
    describe "ConvexPolyhedron.clipAgainstHull"
        [ test "should return 4 results" <|
            \_ ->
                let
                    hull1 =
                        boxHull 1

                    hull2 =
                        boxHull 1

                    t1 =
                        { position = vec3 0 0 2.1 -- going slightly into another box
                        , quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }

                    t2 =
                        { position = vec3 0 0 4
                        , quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
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
                    case ConvexPolyhedron.findSeparatingAxis t1 hull1 t2 hull2 of
                        Just separatingAxis ->
                            ConvexPolyhedron.clipAgainstHull t1 hull1 t2 hull2 separatingAxis -100 100
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
                        , quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 0 0 1)
                        }

                    t2 =
                        { position = vec3 0.5 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
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
                    case ConvexPolyhedron.findSeparatingAxis t1 hull1 t2 hull2 of
                        Just separatingAxis ->
                            ConvexPolyhedron.clipAgainstHull t1 hull1 t2 hull2 separatingAxis -100 100
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
                        , quaternion = vec4 -0.022809298766761247 0.006783793446053796 0.002763745916207627 0.9997129976872166
                        }

                    t2 =
                        { position = vec3 -1.7732501140437167 -0.23893989356833145 1.9746722038817583
                        , quaternion = vec4 -0.14987379072976215 0.5294480629310288 0.19937553795533458 -0.8108464653532712
                        }

                    maybeSeparatingAxis =
                        ConvexPolyhedron.findSeparatingAxis t1 hull t2 hull
                in
                    case maybeSeparatingAxis of
                        Just separatingAxis ->
                            ConvexPolyhedron.clipAgainstHull t1 hull t2 hull separatingAxis -100 100
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
    describe "ConvexPolyhedron.testSepAxis"
        [ test "returns Just depth" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.testSepAxis
                        { position = vec3 -0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.testSepAxis
                        { position = vec3 -5.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    (ConvexPolyhedron.testSepAxis
                        { position = vec3 1 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                of
                    Nothing ->
                        Expect.fail "expected depth"

                    Just value ->
                        Expect.within (Expect.Absolute 0.00001) 0.4071067 value
        ]


findSeparatingAxis : Test
findSeparatingAxis =
    describe "ConvexPolyhedron.findSeparatingAxis"
        [ test "works for offset" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.findSeparatingAxis
                        { position = vec3 -0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                    )
                    (Just (vec3 -1 0 0))
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.findSeparatingAxis
                        { position = vec3 -0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (boxHull 0.5)
                    )
                    (Just (vec3 -1 0 0))
        ]


project : Test
project =
    describe "ConvexPolyhedron.project"
        [ test "works for the positive x axis" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 -1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 0 1 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        { quaternion = Quaternion.identity
                        , position = vec3 0 1 0
                        }
                        (boxHull 0.5)
                        (vec3 0 1 0)
                    )
                    ( 1.5, 0.5 )
        , test "works for the rotation and offset" <|
            \_ ->
                (ConvexPolyhedron.project
                    { quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 1 0 0)
                    , position = vec3 0 1 0
                    }
                    (boxHull 0.5)
                    (vec3 0 1 0)
                )
                    |> Expect.all
                        [ Tuple.first >> Expect.within (Expect.Absolute 0.00001) 1.5
                        , Tuple.second >> Expect.within (Expect.Absolute 0.00001) 0.5
                        ]
        ]


faceNormals : Test
faceNormals =
    describe "ConvexPolyhedron.uniqueEdges"
        [ test "works for the box" <|
            \_ ->
                boxHull 1
                    |> .normals
                    |> Array.toList
                    |> Expect.equal
                        [ vec3 0 0 -1
                        , vec3 0 0 1
                        , vec3 0 -1 0
                        , vec3 0 1 0
                        , vec3 -1 0 0
                        , vec3 1 0 0
                        ]
        ]


uniqueEdges : Test
uniqueEdges =
    describe "ConvexPolyhedron.faceNormals"
        [ test "works for the box" <|
            \_ ->
                boxHull 1
                    |> .edges
                    |> Expect.equal
                        [ vec3 -1 0 0
                        , vec3 0 -1 0
                        , vec3 0 0 -1
                        , vec3 0 0 1
                        , vec3 0 1 0
                        , vec3 1 0 0
                        ]
        ]


boxHull : Float -> ConvexPolyhedron
boxHull size =
    ConvexPolyhedron.fromBox (vec3 size size size)
