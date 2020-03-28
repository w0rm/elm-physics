module ConvexTest exposing
    ( addFaceEdges
    , boxUniqueEdges
    , initFaceNormal
    , initUniqueEdges
    )

import Array exposing (Array)
import Expect
import Extra.Expect as Expect
import Fixtures.Convex
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Test exposing (Test, describe, test)


initFaceNormal : Test
initFaceNormal =
    let
        boxNormals =
            [ Vec3.zNegative
            , Vec3.zAxis
            , Vec3.yNegative
            , Vec3.yAxis
            , Vec3.xNegative
            , Vec3.xAxis
            ]

        -- The direction of each triangle's first angle.
        -- These differ by a 45 degree angle, so that
        -- each entry's normal can be found two entries
        -- down the list and each entry's complement
        -- can be found four entries down --
        -- looping back around as needed.
        xRotationRingSequence =
            [ Vec3.yAxis
            , { x = 0, y = 1, z = 1 } |> Vec3.normalize
            , Vec3.zAxis
            , { x = 0, y = -1, z = 1 } |> Vec3.normalize
            , Vec3.yNegative
            , { x = 0, y = -1, z = -1 } |> Vec3.normalize
            , Vec3.zNegative
            , { x = 0, y = 1, z = -1 } |> Vec3.normalize
            ]

        xNormalRingSequence =
            listRingRotate 2 xRotationRingSequence

        xAntiNormalRingSequence =
            listRingRotate 4 xNormalRingSequence

        xyRightTriangle rightAngleTurn =
            [ Vec3.zero
            , Vec3.xAxis
            , Vec3.xAxis |> Vec3.add rightAngleTurn
            ]

        -- Variations around the y axis.
        --
        -- TODO: These y and z variants could be calculated here using
        -- transforms that represent 120 degree rotations around the axis
        -- (Vec3.normalize (vec3 1 1 1)).
        -- For now, they are hard-coded.
        yRotationRingSequence =
            [ Vec3.zAxis
            , { x = 1, y = 0, z = 1 } |> Vec3.normalize
            , Vec3.xAxis
            , { x = 1, y = 0, z = -1 } |> Vec3.normalize
            , Vec3.zNegative
            , { x = -1, y = 0, z = -1 } |> Vec3.normalize
            , Vec3.xNegative
            , { x = -1, y = 0, z = 1 } |> Vec3.normalize
            ]

        yNormalRingSequence =
            listRingRotate 2 yRotationRingSequence

        yAntiNormalRingSequence =
            listRingRotate 4 yNormalRingSequence

        yzRightTriangle rightAngleTurn =
            [ Vec3.zero
            , Vec3.yAxis
            , Vec3.yAxis |> Vec3.add rightAngleTurn
            ]

        -- Variations around the z axis.
        zRotationRingSequence =
            [ Vec3.xAxis
            , { x = 1, y = 1, z = 0 } |> Vec3.normalize
            , Vec3.yAxis
            , { x = -1, y = 1, z = 0 } |> Vec3.normalize
            , Vec3.xNegative
            , { x = -1, y = -1, z = 0 } |> Vec3.normalize
            , Vec3.yNegative
            , { x = 1, y = -1, z = 0 } |> Vec3.normalize
            ]

        zNormalRingSequence =
            listRingRotate 2 zRotationRingSequence

        zAntiNormalRingSequence =
            listRingRotate 4 zNormalRingSequence

        zxRightTriangle rightAngleTurn =
            [ Vec3.zero
            , Vec3.zAxis
            , Vec3.zAxis |> Vec3.add rightAngleTurn
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
                Convex.fromBlock 1 1 1
                    |> (\{ faces } ->
                            List.map
                                (\{ vertices } ->
                                    legacyInitFaceNormal
                                        (List.range 0 (List.length vertices - 1))
                                        (Array.fromList (List.reverse vertices))
                                )
                                faces
                       )
                    |> Expect.equal boxNormals
        , test "box-specific bypass optimization works identically" <|
            \_ ->
                Convex.fromBlock 1 1 1
                    |> .faces
                    |> List.map .normal
                    |> Expect.equal boxNormals
        , test "works for a right-handed triangle flipped around the x axis" <|
            \_ ->
                xRotationRingSequence
                    |> toRightTriangles xyRightTriangle
                    |> List.map (legacyInitFaceNormal faceIndices)
                    |> Expect.vec3s xNormalRingSequence
        , test "works for a left-handed triangle flipped around the x axis" <|
            \_ ->
                xRotationRingSequence
                    |> toRightTriangles xyRightTriangle
                    |> List.map (legacyInitFaceNormal backFaceIndices)
                    |> Expect.vec3s xAntiNormalRingSequence
        , test "works for a right-handed triangle flipped around the y axis" <|
            \_ ->
                yRotationRingSequence
                    |> toRightTriangles yzRightTriangle
                    |> List.map (legacyInitFaceNormal faceIndices)
                    |> Expect.vec3s yNormalRingSequence
        , test "works for a left-handed triangle flipped around the y axis" <|
            \_ ->
                yRotationRingSequence
                    |> toRightTriangles yzRightTriangle
                    |> List.map (legacyInitFaceNormal backFaceIndices)
                    |> Expect.vec3s yAntiNormalRingSequence
        , test "works for a right-handed triangle flipped around the z axis" <|
            \_ ->
                zRotationRingSequence
                    |> toRightTriangles zxRightTriangle
                    |> List.map (legacyInitFaceNormal faceIndices)
                    |> Expect.vec3s zNormalRingSequence
        , test "works for a left-handed triangle flipped around the z axis" <|
            \_ ->
                zRotationRingSequence
                    |> toRightTriangles zxRightTriangle
                    |> List.map (legacyInitFaceNormal backFaceIndices)
                    |> Expect.vec3s zAntiNormalRingSequence
        ]


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
                Convex.fromBlock 1 1 1
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
                        [ Vec3.xAxis
                        , Vec3.yAxis
                        , Vec3.zAxis
                        ]
                in
                Convex.fromBlock 1 1 1
                    |> addEdgesOfConvex fullSeedSet
                    |> Expect.equal fullSeedSet
        , test "works for the box with negatively directed seeds" <|
            \_ ->
                let
                    fullSeedSet =
                        [ Vec3.xNegative
                        , Vec3.yNegative
                        , Vec3.zNegative
                        ]
                in
                Convex.fromBlock 1 1 1
                    |> addEdgesOfConvex fullSeedSet
                    |> Expect.equal fullSeedSet
        , test "works for the box with partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the
                    -- addition of complementary edges. This tests that the
                    -- de-duping is not wildly over- or under- aggressive.
                    partialSeedSet =
                        [ Vec3.xNegative
                        , Vec3.zAxis
                        ]
                in
                Convex.fromBlock 1 1 1
                    |> countEdgesOfConvex partialSeedSet
                    |> Expect.equal 3
        , test "works for the box with different partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the
                    -- addition of complementary edges. This tests that the
                    -- de-duping is not wildly over- or under- aggressive.
                    partialSeedSet =
                        [ Vec3.zAxis ]
                in
                Convex.fromBlock 1 1 1
                    |> countEdgesOfConvex partialSeedSet
                    |> Expect.equal 3
        , test "works for the box with other different partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the
                    -- addition of complementary edges. This tests that the
                    -- de-duping is not wildly over- or under- aggressive.
                    partialSeedSet =
                        [ Vec3.yAxis ]
                in
                Convex.fromBlock 1 1 1
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
                        [ { x = 1 - Const.precision / 3.0, y = 0, z = 0 }
                        , { x = 0, y = 1 + Const.precision / 3.0, z = 0 }
                        , { x = 0, y = 0, z = -1 - Const.precision / 3.0 }
                        ]
                in
                Convex.fromBlock 1 1 1
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
                        [ { x = 1, y = 1, z = 0 }
                        , { x = 1, y = Const.precision * 3.0, z = 0 }
                        , { x = 1, y = 0, z = Const.precision * 3.0 }
                        ]
                in
                Convex.fromBlock 1 1 1
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
                        [ Vec3.xAxis
                        , Vec3.yAxis
                        ]
                in
                Fixtures.Convex.squarePyramid
                    |> countEdgesOfConvex partialSeedSet
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ Vec3.xAxis
                        , Vec3.yAxis
                        ]
                in
                Fixtures.Convex.askewSquarePyramid
                    |> countEdgesOfConvex partialSeedSet
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                let
                    partialSeedSet =
                        [ Vec3.xAxis
                        , Vec3.yAxis
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
                    (Convex.fromBlock 1 1 1).uniqueEdges
                    [ Vec3.xAxis
                    , Vec3.yAxis
                    , Vec3.zAxis
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
    List.foldl
        (\face edges ->
            Convex.foldFaceEdges
                (\v1 v2 ->
                    Convex.addDirectionIfDistinct (Vec3.direction v1 v2)
                )
                edges
                face.vertices
        )
        []
        faces


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
    List.foldl
        (\face edges ->
            Convex.foldFaceEdges
                (\v1 v2 ->
                    Convex.addDirectionIfDistinct (Vec3.direction v1 v2)
                )
                edges
                face.vertices
        )
        seedEdges
        faces


{-| Useful variant of addEdgesOfConvex that abstracts out the count
for a less-detailed result.
-}
countEdgesOfConvex : List Vec3 -> Convex -> Int
countEdgesOfConvex seedEdges hull =
    List.length <| addEdgesOfConvex seedEdges hull
