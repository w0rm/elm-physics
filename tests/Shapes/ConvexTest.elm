module Shapes.ConvexTest exposing
    ( centerOfMass
    , extendContour
    , faces
    , inertia
    , uniqeNormals
    , uniqueEdges
    , volume
    )

import Block3d exposing (vertices)
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
        normalsPointOutside convex =
            Expect.all
                (List.map
                    (\{ normal, vertices } ->
                        \_ ->
                            Expect.true
                                "normal points inside"
                                (List.all
                                    (\v ->
                                        let
                                            pointsOutside =
                                                Vec3.dot (Vec3.sub v convex.position) normal > 0
                                        in
                                        pointsOutside
                                    )
                                    vertices
                                )
                    )
                    convex.faces
                )
                ()

        hasCorrectWindingOrder convex =
            Expect.all
                (List.map
                    (\{ vertices, normal } ->
                        case vertices of
                            p1 :: p2 :: p3 :: rest ->
                                \_ -> faceWindingOrderHelp normal p1 (p2 :: p3 :: rest) []

                            _ ->
                                \_ -> Expect.fail "face with wrong number of vertices"
                    )
                    convex.faces
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
                List.map .normal (Convex.fromBlock 2 2 2).faces
                    |> Expect.equal
                        [ Vec3.zNegative
                        , Vec3.zAxis
                        , Vec3.yNegative
                        , Vec3.yAxis
                        , Vec3.xNegative
                        , Vec3.xAxis
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
                (Convex.fromBlock 2 2 2).uniqueNormals
                    |> Expect.equal Vec3.basis
        , test "works for a square pyramid" <|
            \_ ->
                List.length Fixtures.Convex.squarePyramid.uniqueNormals
                    |> Expect.equal 5
        ]


uniqueEdges : Test
uniqueEdges =
    describe ".uniqueEdges"
        [ test "works for a block" <|
            \_ ->
                (Convex.fromBlock 2 2 2).uniqueEdges
                    |> Expect.equal Vec3.basis
        , test "works for a square pyramid" <|
            \_ ->
                List.length Fixtures.Convex.squarePyramid.uniqueEdges
                    |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                List.length Fixtures.Convex.askewSquarePyramid.uniqueEdges
                    |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                List.length Fixtures.Convex.nonSquareQuadPyramid.uniqueEdges
                    -- all edges unique, none parallel
                    |> Expect.equal 8
        ]
