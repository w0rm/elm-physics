module ConvexTest exposing
    ( addFaceEdges
    , boxUniqueEdges
    , faceAdjacency
    , initFaceNormal
    , initUniqueEdges
    )

import Array exposing (Array)
import Expect exposing (Expectation)
import Fixtures.Convex
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (..)


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

        -- TODO: test the public api of Convex.init instead
        legacyInitFaceNormal : List Int -> Array Vec3 -> Vec3
        legacyInitFaceNormal indices vertices =
            Convex.init [ indices ] vertices
                |> .faces
                |> List.head
                |> Maybe.map .normal
                |> Maybe.withDefault Vec3.zero
    in
    describe "Convex.initFaceNormal"
        [ test "works for the box" <|
            \_ ->
                Fixtures.Convex.boxHull 1
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
                Fixtures.Convex.boxHull 1
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
                    - Const.precision
                    < 0
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
    describe "Convex.initUniqueEdges"
        -- There are several valid representations of the same convex
        -- polyhedron, differing in the listed order of vertices and/or faces
        -- or in insignificant rounding errors in vertex values.
        -- So, the implementation of initUniqueEdges should be given some
        -- lattitude in its resulting list of edges.
        -- Convex.addFaceEdges does most of the work of
        -- Convex.initUniqueEdges, and it can be tested with seed
        -- values to get more deterministic results from Convexs
        -- even with varying equivalent representations.
        [ test "gives the correct number of edges for a box" <|
            \_ ->
                Fixtures.Convex.boxHull 1
                    |> uniqueEdgesOfConvex
                    |> List.length
                    |> Expect.equal 3

        -- The square pyramid shape has fewer parallel edges than a box.
        -- The extent of parallel edges in a box was masking a bug discovered
        -- in code review of addFaceEdges/initUniqueEdges that would miss
        -- some edges.
        , test "works for a square pyramid" <|
            \_ ->
                Fixtures.Convex.squarePyramid
                    |> uniqueEdgesOfConvex
                    |> List.length
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                Fixtures.Convex.askewSquarePyramid
                    |> uniqueEdgesOfConvex
                    |> List.length
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                Fixtures.Convex.nonSquareQuadPyramid
                    |> uniqueEdgesOfConvex
                    |> List.length
                    -- all edges unique, none parallel
                    |> Expect.equal 8
        ]


addFaceEdges : Test
addFaceEdges =
    describe "Convex.addFaceEdges"
        -- Testing addFaceEdges avoids over-testing for exact results from
        -- Convex.initUniqueEdges.
        -- There are several valid representations of the same convex
        -- polyhedron, differing in the listed order of vertices and/or faces
        -- or in insignificant rounding errors in vertex values.
        -- So, the implementation of initUniqueEdges should be given some
        -- lattitude in its resulting list of edges.
        -- Convex.addFaceEdges does most of the work of
        -- Convex.initUniqueEdges, and it can be tested with seed
        -- values to get more deterministic results from Convexs
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
                Fixtures.Convex.boxHull 1
                    |> addEdgesOfConvex fullSeedSet
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
                Fixtures.Convex.boxHull 1
                    |> addEdgesOfConvex fullSeedSet
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
                Fixtures.Convex.boxHull 1
                    |> countEdgesOfConvex partialSeedSet
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
                Fixtures.Convex.boxHull 1
                    |> countEdgesOfConvex partialSeedSet
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
                Fixtures.Convex.boxHull 1
                    |> countEdgesOfConvex partialSeedSet
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
                Fixtures.Convex.boxHull 1
                    |> addEdgesOfConvex validSeedSet
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
                        , vec3 1 (Const.precision * 3.0) 0
                        , vec3 1 0 (Const.precision * 3.0)
                        ]
                in
                Fixtures.Convex.boxHull 1
                    |> countEdgesOfConvex invalidSeedSet
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
                Fixtures.Convex.squarePyramid
                    |> countEdgesOfConvex partialSeedSet
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                Fixtures.Convex.askewSquarePyramid
                    |> countEdgesOfConvex partialSeedSet
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                Fixtures.Convex.nonSquareQuadPyramid
                    |> countEdgesOfConvex partialSeedSet
                    -- all edges unique, none parallel
                    |> Expect.equal 8
        ]


boxUniqueEdges : Test
boxUniqueEdges =
    describe "Convex.boxUniqueEdges"
        [ test "works for the box" <|
            \_ ->
                Expect.equal
                    (Fixtures.Convex.boxHull 1).uniqueEdges
                    [ vec3 1 0 0
                    , vec3 0 1 0
                    , vec3 0 0 1
                    ]
        ]


faceAdjacency : Test
faceAdjacency =
    describe "Convex.faceAdjacency"
        [ test "works for the box" <|
            \_ ->
                Fixtures.Convex.boxVertexIndices
                    |> Convex.faceAdjacency
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
                Fixtures.Convex.octoVertexIndices
                    |> Convex.faceAdjacency
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
vertices of an existing Convex. There is no need for this in
production, where initUniqueEdges is called once at most per Convex
BEFORE that Convex is fully initialized, because its result gets
cached in Convex.edges.
-}
uniqueEdgesOfConvex : Convex -> List Vec3
uniqueEdgesOfConvex { faces } =
    Convex.initUniqueEdges faces


{-| This test helper function is intended as a more flexible variant of
Convex.initUniqueEdges. Its differences from initUniqueEdges are
that it can be fed an initial list of "seed" edges and it operates on a
pre-existing Convex's vertices and faces.
See the comment on uniqueEdgesOfConvex.
These differences have no application in production.
Keep this code in sync with any changes to Convex.initUniqueEdges.
-}
addEdgesOfConvex : List Vec3 -> Convex -> List Vec3
addEdgesOfConvex seedEdges { faces } =
    List.foldl Convex.addFaceEdges seedEdges faces


{-| Useful variant of addEdgesOfConvex that abstracts out the count
for a less-detailed result.
-}
countEdgesOfConvex : List Vec3 -> Convex -> Int
countEdgesOfConvex seedEdges hull =
    List.length <| addEdgesOfConvex seedEdges hull
