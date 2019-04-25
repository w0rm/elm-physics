module ConvexPolyhedronTest exposing
    ( addFaceEdges
    , boxUniqueEdges
    , clipAgainstHull
    , clipFaceAgainstHull
    , clipFaceAgainstPlane
    , faceAdjacency
    , findSeparatingAxis
    , initFaceNormal
    , initUniqueEdges
    , project
    , testSepAxis
    )

import Array exposing (Array)
import Expect exposing (Expectation)
import Fixtures.ConvexPolyhedron
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (..)


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
                        , vec3 0.2 -0.2 -1
                        , vec3 0.2 0.2 -1
                        , vec3 -0.2 0.2 -1
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
                        , vec3 2 -2 -1
                        , vec3 2 2 -1
                        , vec3 0 2 0
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
                        , orientation = { x = -0.022809298766761247, y = 0.006783793446053796, z = 0.002763745916207627, w = 0.9997129976872166 }
                        }

                    t2 =
                        { position = vec3 -1.7732501140437167 -0.23893989356833145 1.9746722038817583
                        , orientation = { x = -0.14987379072976215, y = 0.5294480629310288, z = 0.19937553795533458, w = -0.8108464653532712 }
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
                        , orientation = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.identity
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
                        , orientation = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.identity
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    ConvexPolyhedron.testSepAxis
                        { position = vec3 1 0 0
                        , orientation = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , orientation = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
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
                    (ConvexPolyhedron.findSeparatingAxis
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
                        { orientation = Quaternion.identity
                        , position = vec3 0 1 0
                        }
                        (boxHull 0.5)
                        (vec3 0 1 0)
                    )
                    ( 1.5, 0.5 )
        , test "works for the rotation and offset" <|
            \_ ->
                ConvexPolyhedron.project
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


initFaceNormal : Test
initFaceNormal =
    let
        boxNormals =
            [ vec3 0 0 -1
            , vec3 0 0 1
            , vec3 0 -1 0
            , vec3 0 1 0
            , vec3 -1 0 0
            , vec3 1 0 0
            ]

        -- The direction of each triangle's first angle.
        -- These differ by a 45 degree angle, so that
        -- each entry's normal can be found two entries
        -- down the list and each entry's complement
        -- can be found four entries down --
        -- looping back around as needed.
        xRotationRingSequence =
            [ vec3 0 1 0
            , vec3 0 1 1 |> Vec3.normalize
            , vec3 0 0 1
            , vec3 0 -1 1 |> Vec3.normalize
            , vec3 0 -1 0
            , vec3 0 -1 -1 |> Vec3.normalize
            , vec3 0 0 -1
            , vec3 0 1 -1 |> Vec3.normalize
            ]

        xNormalRingSequence =
            listRingRotate 2 xRotationRingSequence

        xAntiNormalRingSequence =
            listRingRotate 4 xNormalRingSequence

        xyRightTriangle rightAngleTurn =
            [ vec3 0 0 0
            , vec3 1 0 0
            , vec3 1 0 0 |> Vec3.add rightAngleTurn
            ]

        -- Variations around the y axis.
        --
        -- TODO: These y and z variants could be calculated here using
        -- transforms that represent 120 degree rotations around the axis
        -- (Vec3.normalize (vec3 1 1 1)).
        -- For now, they are hard-coded.
        yRotationRingSequence =
            [ vec3 0 0 1
            , vec3 1 0 1 |> Vec3.normalize
            , vec3 1 0 0
            , vec3 1 0 -1 |> Vec3.normalize
            , vec3 0 0 -1
            , vec3 -1 0 -1 |> Vec3.normalize
            , vec3 -1 0 0
            , vec3 -1 0 1 |> Vec3.normalize
            ]

        yNormalRingSequence =
            listRingRotate 2 yRotationRingSequence

        yAntiNormalRingSequence =
            listRingRotate 4 yNormalRingSequence

        yzRightTriangle rightAngleTurn =
            [ vec3 0 0 0
            , vec3 0 1 0
            , vec3 0 1 0 |> Vec3.add rightAngleTurn
            ]

        -- Variations around the z axis.
        zRotationRingSequence =
            [ vec3 1 0 0
            , vec3 1 1 0 |> Vec3.normalize
            , vec3 0 1 0
            , vec3 -1 1 0 |> Vec3.normalize
            , vec3 -1 0 0
            , vec3 -1 -1 0 |> Vec3.normalize
            , vec3 0 -1 0
            , vec3 1 -1 0 |> Vec3.normalize
            ]

        zNormalRingSequence =
            listRingRotate 2 zRotationRingSequence

        zAntiNormalRingSequence =
            listRingRotate 4 zNormalRingSequence

        zxRightTriangle rightAngleTurn =
            [ vec3 0 0 0
            , vec3 0 0 1
            , vec3 0 0 1 |> Vec3.add rightAngleTurn
            ]

        faceIndices =
            [ 0, 1, 2 ]

        backFaceIndices =
            [ 2, 1, 0 ]

        toRightTriangles rightTriangle =
            List.map
                (\rightAngleTurn ->
                    rightAngleTurn
                        |> rightTriangle
                        |> Array.fromList
                )

        -- TODO: test the public api of ConvexPolyhedron.init instead
        legacyInitFaceNormal : List Int -> Array Vec3 -> Vec3
        legacyInitFaceNormal indices vertices =
            ConvexPolyhedron.init [ indices ] vertices
                |> .faces
                |> List.head
                |> Maybe.map .normal
                |> Maybe.withDefault Const.zero3
    in
    describe "ConvexPolyhedron.initFaceNormal"
        [ test "works for the box" <|
            \_ ->
                boxHull 1
                    |> (\{ faces } ->
                            List.map
                                (\{ vertices } ->
                                    legacyInitFaceNormal
                                        (List.range 0 (List.length vertices - 1))
                                        (Array.fromList vertices)
                                )
                                faces
                       )
                    |> Expect.equal boxNormals
        , test "box-specific bypass optimization works identically" <|
            \_ ->
                boxHull 1
                    |> .faces
                    |> List.map .normal
                    |> Expect.equal boxNormals
        , test "works for a right-handed triangle flipped around the x axis" <|
            \_ ->
                xRotationRingSequence
                    |> toRightTriangles xyRightTriangle
                    |> List.map
                        (legacyInitFaceNormal faceIndices)
                    |> expectListVec3WithinPrecision
                        xNormalRingSequence
        , test "works for a left-handed triangle flipped around the x axis" <|
            \_ ->
                xRotationRingSequence
                    |> toRightTriangles xyRightTriangle
                    |> List.map
                        (legacyInitFaceNormal backFaceIndices)
                    |> expectListVec3WithinPrecision
                        xAntiNormalRingSequence
        , test "works for a right-handed triangle flipped around the y axis" <|
            \_ ->
                yRotationRingSequence
                    |> toRightTriangles yzRightTriangle
                    |> List.map
                        (legacyInitFaceNormal faceIndices)
                    |> expectListVec3WithinPrecision
                        yNormalRingSequence
        , test "works for a left-handed triangle flipped around the y axis" <|
            \_ ->
                yRotationRingSequence
                    |> toRightTriangles yzRightTriangle
                    |> List.map
                        (legacyInitFaceNormal backFaceIndices)
                    |> expectListVec3WithinPrecision
                        yAntiNormalRingSequence
        , test "works for a right-handed triangle flipped around the z axis" <|
            \_ ->
                zRotationRingSequence
                    |> toRightTriangles zxRightTriangle
                    |> List.map
                        (legacyInitFaceNormal faceIndices)
                    |> expectListVec3WithinPrecision
                        zNormalRingSequence
        , test "works for a left-handed triangle flipped around the z axis" <|
            \_ ->
                zRotationRingSequence
                    |> toRightTriangles zxRightTriangle
                    |> List.map
                        (legacyInitFaceNormal backFaceIndices)
                    |> expectListVec3WithinPrecision
                        zAntiNormalRingSequence
        ]


{-| Force point equality for a point within a radius epsilon
== sqrt (Const.precision) of its expected position.
-}
normalizeVec3Towards : Vec3 -> Vec3 -> Vec3
normalizeVec3Towards approximation canonical =
    if
        (approximation == canonical)
            || ((Vec3.sub approximation canonical
                    |> Vec3.lengthSquared
                )
                    < Const.precision
               )
    then
        canonical

    else
        approximation


{-|

    Substitute a less precise test for Equals without sacrificing
    the detailed reporting on failure.

-}
expectListVec3WithinPrecision : List Vec3 -> List Vec3 -> Expectation
expectListVec3WithinPrecision actualList expectedList =
    List.map2
        normalizeVec3Towards
        actualList
        expectedList
        |> Expect.equal expectedList


listRingRotate : Int -> List a -> List a
listRingRotate offset ring =
    -- This brute force implementation doubles the list and uses
    -- modulus indexing to ensure enough elements to carve out the
    -- desired slice at any non-negative offset.
    let
        resultLength =
            List.length ring
    in
    ring
        ++ ring
        |> List.drop (modBy resultLength offset)
        |> List.take resultLength


initUniqueEdges : Test
initUniqueEdges =
    describe "ConvexPolyhedron.initUniqueEdges"
        -- There are several valid representations of the same convex
        -- polyhedron, differing in the listed order of vertices and/or faces
        -- or in insignificant rounding errors in vertex values.
        -- So, the implementation of initUniqueEdges should be given some
        -- lattitude in its resulting list of edges.
        -- ConvexPolyhedron.addFaceEdges does most of the work of
        -- ConvexPolyhedron.initUniqueEdges, and it can be tested with seed
        -- values to get more deterministic results from ConvexPolyhedrons
        -- even with varying equivalent representations.
        [ test "gives the correct number of edges for a box" <|
            \_ ->
                boxHull 1
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 3

        -- The square pyramid shape has fewer parallel edges than a box.
        -- The extent of parallel edges in a box was masking a bug discovered
        -- in code review of addFaceEdges/initUniqueEdges that would miss
        -- some edges.
        , test "works for a square pyramid" <|
            \_ ->
                Fixtures.ConvexPolyhedron.squarePyramid
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                Fixtures.ConvexPolyhedron.askewSquarePyramid
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                Fixtures.ConvexPolyhedron.nonSquareQuadPyramid
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    -- all edges unique, none parallel
                    |> Expect.equal 8
        ]


addFaceEdges : Test
addFaceEdges =
    describe "ConvexPolyhedron.addFaceEdges"
        -- Testing addFaceEdges avoids over-testing for exact results from
        -- ConvexPolyhedron.initUniqueEdges.
        -- There are several valid representations of the same convex
        -- polyhedron, differing in the listed order of vertices and/or faces
        -- or in insignificant rounding errors in vertex values.
        -- So, the implementation of initUniqueEdges should be given some
        -- lattitude in its resulting list of edges.
        -- ConvexPolyhedron.addFaceEdges does most of the work of
        -- ConvexPolyhedron.initUniqueEdges, and it can be tested with seed
        -- values to get more deterministic results from ConvexPolyhedrons
        -- even with varying equivalent representations.
        [ test "works for the box with positive seeds" <|
            \_ ->
                let
                    -- Pre-calculated seeds are one way to get an exact
                    -- normalized result. Members of the seed set are
                    -- acceptable members of the result set.
                    -- So long as the result-building process is
                    -- non-destructive, the seeds should act as magnets for
                    -- other valid results and should mask them in the final
                    -- result.
                    fullSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        , vec3 0 0 1
                        ]
                in
                boxHull 1
                    |> addEdgesOfConvexPolyhedron fullSeedSet
                    |> Expect.equal fullSeedSet
        , test "works for the box with negatively directed seeds" <|
            \_ ->
                let
                    fullSeedSet =
                        [ vec3 -1 0 0
                        , vec3 0 -1 0
                        , vec3 0 0 -1
                        ]
                in
                boxHull 1
                    |> addEdgesOfConvexPolyhedron fullSeedSet
                    |> Expect.equal fullSeedSet
        , test "works for the box with partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the
                    -- addition of complementary edges. This tests that the
                    -- de-duping is not wildly over- or under- aggressive.
                    partialSeedSet =
                        [ vec3 -1 0 0
                        , vec3 0 0 1
                        ]
                in
                boxHull 1
                    |> countEdgesOfConvexPolyhedron partialSeedSet
                    |> Expect.equal 3
        , test "works for the box with different partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the
                    -- addition of complementary edges. This tests that the
                    -- de-duping is not wildly over- or under- aggressive.
                    partialSeedSet =
                        [ vec3 0 0 1 ]
                in
                boxHull 1
                    |> countEdgesOfConvexPolyhedron partialSeedSet
                    |> Expect.equal 3
        , test "works for the box with other different partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the
                    -- addition of complementary edges. This tests that the
                    -- de-duping is not wildly over- or under- aggressive.
                    partialSeedSet =
                        [ vec3 0 1 0 ]
                in
                boxHull 1
                    |> countEdgesOfConvexPolyhedron partialSeedSet
                    |> Expect.equal 3
        , test "works for the box with approximate seeds" <|
            \_ ->
                let
                    -- These approximate seeds should mask as effectively
                    -- as their exact integer equivalents would, ASSUMING
                    -- we have avoided the worst case scenario.
                    -- That would be when the vertices under test are
                    -- near the specific precision boundaries that would
                    -- cause insignificant error terms to compound
                    -- instead of canceling in the edge calculations.
                    validSeedSet =
                        [ vec3 (1 - Const.precision / 3.0) 0 0
                        , vec3 0 (1 + Const.precision / 3.0) 0
                        , vec3 0 0 (-1 - Const.precision / 3.0)
                        ]
                in
                boxHull 1
                    |> addEdgesOfConvexPolyhedron validSeedSet
                    |> Expect.equal validSeedSet
        , test "works for the box with invalid seeds" <|
            \_ ->
                let
                    -- Each invalid seed should simply linger in the result
                    -- with no effect on how (many) valid elements are added
                    -- as complementary edges. This tests that de-duping is
                    -- not overly aggressive in its matching.
                    -- Note: Some seeds use (Const.precision * 3.0) offsets to
                    -- force values that are purposely not quite precise
                    -- enough to match "exact" vertex values.
                    -- These tests should work as well with non-exact vertices
                    -- except in a worst case scenario: we are ASSUMING that
                    -- any insignificant error terms in the vertex values are
                    -- not cases that will be compounded by the edge
                    -- calculations in the same specific dimension as these
                    -- test offsets.
                    invalidSeedSet =
                        [ vec3 1 1 0
                        , vec3 (1 - Const.precision * 3.0) 0 0
                        , vec3 0 0 (-1 - Const.precision * 3.0)
                        , vec3 0 0 0
                        ]
                in
                boxHull 1
                    |> countEdgesOfConvexPolyhedron invalidSeedSet
                    |> Expect.equal (List.length invalidSeedSet + 3)

        -- The square pyramid shape has fewer parallel edges than a box.
        -- The extent of parallel edges in a box was masking a bug discovered
        -- in code review of addFaceEdges/initUniqueEdges that would miss
        -- some edges.
        , test "works for a square pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                Fixtures.ConvexPolyhedron.squarePyramid
                    |> countEdgesOfConvexPolyhedron partialSeedSet
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                Fixtures.ConvexPolyhedron.askewSquarePyramid
                    |> countEdgesOfConvexPolyhedron partialSeedSet
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                Fixtures.ConvexPolyhedron.nonSquareQuadPyramid
                    |> countEdgesOfConvexPolyhedron partialSeedSet
                    -- all edges unique, none parallel
                    |> Expect.equal 8
        ]


boxUniqueEdges : Test
boxUniqueEdges =
    describe "ConvexPolyhedron.boxUniqueEdges"
        [ test "works for the box" <|
            \_ ->
                boxHull 1
                    |> .edges
                    |> Expect.equal
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        , vec3 0 0 1
                        ]
        ]


faceAdjacency : Test
faceAdjacency =
    describe "ConvexPolyhedron.faceAdjacency"
        [ test "works for the box" <|
            \_ ->
                Fixtures.ConvexPolyhedron.boxVertexIndices
                    |> ConvexPolyhedron.faceAdjacency
                    |> List.map List.sort
                    |> Expect.equal
                        [ [ 2, 3, 4, 5 ]
                        , [ 2, 3, 4, 5 ]
                        , [ 0, 1, 4, 5 ]
                        , [ 0, 1, 4, 5 ]
                        , [ 0, 1, 2, 3 ]
                        , [ 0, 1, 2, 3 ]
                        ]
        , test "works for the octohedron" <|
            \_ ->
                Fixtures.ConvexPolyhedron.octoVertexIndices
                    |> ConvexPolyhedron.faceAdjacency
                    |> List.map List.sort
                    |> Expect.equal
                        [ [ 1, 2, 3, 4, 5, 6 ]
                        , [ 0, 2, 3, 4, 6, 7 ]
                        , [ 0, 1, 3, 4, 5, 7 ]
                        , [ 0, 1, 2, 5, 6, 7 ]
                        , [ 0, 1, 2, 5, 6, 7 ]
                        , [ 0, 2, 3, 4, 6, 7 ]
                        , [ 0, 1, 3, 4, 5, 7 ]
                        , [ 1, 2, 3, 4, 5, 6 ]
                        ]
        ]



-- Test helper functions


{-| Provide convenient test access to initUniqueEdges based on the faces and
vertices of an existing ConvexPolyhedron. There is no need for this in
production, where initUniqueEdges is called once at most per ConvexPolyhedron
BEFORE that ConvexPolyhedron is fully initialized, because its result gets
cached in ConvexPolyhedron.edges.
-}
uniqueEdgesOfConvexPolyhedron : ConvexPolyhedron -> List Vec3
uniqueEdgesOfConvexPolyhedron { faces } =
    ConvexPolyhedron.initUniqueEdges faces


{-| This test helper function is intended as a more flexible variant of
ConvexPolyhedron.initUniqueEdges. Its differences from initUniqueEdges are
that it can be fed an initial list of "seed" edges and it operates on a
pre-existing ConvexPolyhedron's vertices and faces.
See the comment on uniqueEdgesOfConvexPolyhedron.
These differences have no application in production.
Keep this code in sync with any changes to ConvexPolyhedron.initUniqueEdges.
-}
addEdgesOfConvexPolyhedron : List Vec3 -> ConvexPolyhedron -> List Vec3
addEdgesOfConvexPolyhedron seedEdges { faces } =
    List.foldl ConvexPolyhedron.addFaceEdges seedEdges faces


{-| Useful variant of addEdgesOfConvexPolyhedron that abstracts out the count
for a less-detailed result.
-}
countEdgesOfConvexPolyhedron : List Vec3 -> ConvexPolyhedron -> Int
countEdgesOfConvexPolyhedron seedEdges hull =
    List.length <| addEdgesOfConvexPolyhedron seedEdges hull



-- Test data generators


{-| A ConvexPolyhedron for a cube with the given half-extent, constructed
using optimized box-specific initializers.
-}
boxHull : Float -> ConvexPolyhedron
boxHull halfExtent =
    Fixtures.ConvexPolyhedron.boxHull halfExtent
