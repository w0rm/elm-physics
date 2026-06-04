module Shapes.ConvexTest exposing
    ( centerOfMass
    , extendContour
    , faces
    , inertia
    , uniqeNormals
    , uniqueEdges
    , volume
    )

import Expect
import Extra.Expect as Expect
import Fixtures.Convex
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


inertia : Test
inertia =
    describe "inertia"
        [ test "inertia of a Convex.fromBlock is the same as Convex.fromTriangularMesh" <|
            \_ ->
                (Fixtures.Convex.block Transform3d.atOrigin 2 3 5).inertia
                    |> Expect.mat3 (Convex.fromBlock 2 3 5).inertia
        , test "inertia of transformed geometry is the same as transformed inertia of original geometry" <|
            \_ ->
                let
                    transform3d =
                        Transform3d.atPoint { x = 12, y = 2, z = -3 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 5)
                            |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 5)
                in
                (Fixtures.Convex.block transform3d 2 3 5).inertia
                    |> Expect.mat3 (Transform3d.inertiaRotateIn transform3d (Convex.fromBlock 2 3 5).inertia)
        ]


centerOfMass : Test
centerOfMass =
    describe "centerOfMass"
        [ test "centerOfMass of transformed geometry is the same as transformed centerOfMass" <|
            \_ ->
                let
                    transform3d =
                        Transform3d.atPoint { x = 2, y = 1, z = -2 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 5)
                            |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 5)
                in
                (Fixtures.Convex.block transform3d 2 3 5).position
                    |> Expect.vec3 (Convex.placeIn transform3d (Convex.fromBlock 2 3 5)).position
        ]


volume : Test
volume =
    describe ".volume"
        [ test "volume of a Convex.fromBlock is the same as Convex.fromTriangularMesh" <|
            \_ ->
                (Fixtures.Convex.block Transform3d.atOrigin 2 3 1).volume
                    |> Expect.equal (Convex.fromBlock 2 3 1).volume
        ]


extendContour : Test
extendContour =
    describe "Convex.extendContour"
        [ test "works for the first point" <|
            \_ ->
                Convex.extendContour ( 666, 4, 3 ) [ 1, 2, 3, 4, 5, 6 ]
                    |> Expect.equal [ 1, 2, 3, 666, 4, 5, 6 ]
        , test "works for the second point" <|
            \_ ->
                Convex.extendContour ( 3, 666, 4 ) [ 1, 2, 3, 4, 5, 6 ]
                    |> Expect.equal [ 1, 2, 3, 666, 4, 5, 6 ]
        , test "works for the third point" <|
            \_ ->
                Convex.extendContour ( 4, 3, 666 ) [ 1, 2, 3, 4, 5, 6 ]
                    |> Expect.equal [ 1, 2, 3, 666, 4, 5, 6 ]
        , test "works for the first point at the end" <|
            \_ ->
                Convex.extendContour ( 666, 1, 6 ) [ 1, 2, 3, 4, 5, 6 ]
                    |> Expect.equal [ 1, 2, 3, 4, 5, 6, 666 ]
        , test "works for the second point at the end" <|
            \_ ->
                Convex.extendContour ( 6, 666, 1 ) [ 1, 2, 3, 4, 5, 6 ]
                    |> Expect.equal [ 1, 2, 3, 4, 5, 6, 666 ]
        , test "works for the third point at the end" <|
            \_ ->
                Convex.extendContour ( 1, 6, 666 ) [ 1, 2, 3, 4, 5, 6 ]
                    |> Expect.equal [ 1, 2, 3, 4, 5, 6, 666 ]
        ]


faces : Test
faces =
    let
        flatFaces convex =
            List.concatMap
                (\group ->
                    case group of
                        Convex.OneSidedFace n i _ _ ->
                            [ { normal = n, vertices = i } ]

                        Convex.TwoSidedFace n1 i1 n2 i2 ->
                            [ { normal = n1, vertices = i1 }, { normal = n2, vertices = i2 } ]
                )
                convex.faces

        normalsPointOutside convex =
            Expect.all
                (List.map
                    (\face ->
                        \_ ->
                            Expect.equal True
                                (List.all
                                    (\v ->
                                        let
                                            pointsOutside =
                                                Vec3.dot (Vec3.sub v convex.position) face.normal > 0
                                        in
                                        pointsOutside
                                    )
                                    (Convex.faceVertices convex.vertexBuffer face)
                                )
                    )
                    (flatFaces convex)
                )
                ()

        hasCorrectWindingOrder convex =
            Expect.all
                (List.map
                    (\face ->
                        case Convex.faceVertices convex.vertexBuffer face of
                            p1 :: p2 :: p3 :: rest ->
                                \_ -> faceWindingOrderHelp face.normal p1 (p2 :: p3 :: rest) []

                            _ ->
                                \_ -> Expect.fail "face with wrong number of vertices"
                    )
                    (flatFaces convex)
                )
                ()

        faceWindingOrderHelp normal firstVertex vertices expectations =
            case vertices of
                p1 :: p2 :: restVertices ->
                    faceWindingOrderHelp normal
                        firstVertex
                        (p2 :: restVertices)
                        (Expect.vec3 normal (Convex.computeNormal firstVertex p1 p2) :: expectations)

                _ ->
                    Expect.all (List.map always expectations) ()
    in
    describe ".faces"
        [ test "block faces have correct normals" <|
            \_ ->
                List.map .normal (flatFaces (Convex.fromBlock 2 2 2))
                    |> Expect.equal
                        [ Vec3.zAxis
                        , Vec3.zNegative
                        , Vec3.yAxis
                        , Vec3.yNegative
                        , Vec3.xAxis
                        , Vec3.xNegative
                        ]
        , test "block faces have correct winding order" <|
            \_ -> hasCorrectWindingOrder (Convex.fromBlock 2 2 2)
        , test "block face normals point outside" <|
            \_ -> normalsPointOutside (Convex.fromBlock 2 2 2)
        , test "square pyramid face normals point outside" <|
            \_ -> normalsPointOutside Fixtures.Convex.squarePyramid
        , test "cylinder face normals point outside" <|
            \_ -> normalsPointOutside (Convex.fromCylinder 6 4 5)
        , test "cylinder faces have correct winding order" <|
            \_ -> hasCorrectWindingOrder (Convex.fromCylinder 6 4 5)
        , test "convex block face normals point outside" <|
            \_ -> normalsPointOutside (Fixtures.Convex.block Transform3d.atOrigin 4 4 4)
        , test "convex block face have correct winding order" <|
            \_ -> hasCorrectWindingOrder (Fixtures.Convex.block Transform3d.atOrigin 4 4 4)
        , test "huge convex face normals point outside" <|
            \_ -> normalsPointOutside Fixtures.Convex.hugeConvex
        , test "huge convex faces have correct winding order" <|
            \_ -> hasCorrectWindingOrder Fixtures.Convex.hugeConvex
        ]


uniqeNormals : Test
uniqeNormals =
    describe ".uniqeNormals"
        [ test "works for a block" <|
            \_ ->
                List.map Convex.faceGroupNormal (Convex.fromBlock 2 2 2).faces
                    |> Expect.equal [ Vec3.zAxis, Vec3.yAxis, Vec3.xAxis ]
        , test "works for a square pyramid" <|
            \_ ->
                List.length Fixtures.Convex.squarePyramid.faces
                    |> Expect.equal 5
        ]


uniqueEdges : Test
uniqueEdges =
    describe ".uniqueEdges"
        [ test "works for a block" <|
            \_ ->
                -- uniqueEdges now holds vertex-buffer indices in the canonical
                -- placed traversal order (outer groups reversed, each group's
                -- endpoints reversed) — read two-at-a-time per edge.
                (Convex.fromBlock 2 2 2).uniqueEdges
                    |> Expect.equal
                        [ [ 0, 4, 3, 7, 2, 6, 1, 5 ]
                        , [ 0, 3, 5, 6, 4, 7, 1, 2 ]
                        , [ 0, 1, 7, 6, 4, 5, 3, 2 ]
                        ]
        , test "block uniqueEdges has 12 edges across 3 directions" <|
            \_ ->
                (Convex.fromBlock 2 3 5).uniqueEdges
                    |> List.map (\group -> List.length group // 2)
                    |> Expect.equal [ 4, 4, 4 ]
        , test "block from triangular mesh has 12 edges across 3 directions" <|
            \_ ->
                (Fixtures.Convex.block Transform3d.atOrigin 2 3 5).uniqueEdges
                    |> List.map (\group -> List.length group // 2)
                    |> List.sort
                    |> Expect.equal [ 4, 4, 4 ]
        , test "works for a square pyramid" <|
            \_ ->
                List.length Fixtures.Convex.squarePyramid.uniqueEdges
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                List.length Fixtures.Convex.askewSquarePyramid.uniqueEdges
                    -- all edges unique, none parallel
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                List.length Fixtures.Convex.nonSquareQuadPyramid.uniqueEdges
                    -- all edges unique, none parallel
                    |> Expect.equal 8
        ]
