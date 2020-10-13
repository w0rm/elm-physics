module Fixtures.Convex exposing
    ( askewSquarePyramid
    , block
    , blockOfTetrahedrons
    , hugeConvex
    , nonSquareQuadPyramid
    , octoHull
    , squarePyramid
    )

import Array
import Internal.Const as Const
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Shapes.Convex as Convex exposing (Convex)



-- Test data generators


block : Transform3d coord define -> Float -> Float -> Float -> Convex.Convex
block transform sizeX sizeY sizeZ =
    let
        halfExtentX =
            sizeX / 2

        halfExtentY =
            sizeY / 2

        halfExtentZ =
            sizeZ / 2

        t p =
            Transform3d.pointPlaceIn transform p
    in
    Convex.fromTriangularMesh
        [ ( 1, 7, 5 )
        , ( 1, 2, 7 )
        , ( 4, 7, 6 )
        , ( 4, 5, 7 )
        , ( 6, 2, 3 )
        , ( 6, 7, 2 )
        , ( 3, 4, 6 )
        , ( 3, 0, 4 )
        , ( 0, 5, 4 )
        , ( 0, 1, 5 )
        , ( 3, 1, 0 )
        , ( 3, 2, 1 )
        ]
        (Array.fromList
            [ t { x = halfExtentX, y = -halfExtentY, z = -halfExtentZ }
            , t { x = halfExtentX, y = halfExtentY, z = -halfExtentZ }
            , t { x = -halfExtentX, y = halfExtentY, z = -halfExtentZ }
            , t { x = -halfExtentX, y = -halfExtentY, z = -halfExtentZ }
            , t { x = halfExtentX, y = -halfExtentY, z = halfExtentZ }
            , t { x = halfExtentX, y = halfExtentY, z = halfExtentZ }
            , t { x = -halfExtentX, y = -halfExtentY, z = halfExtentZ }
            , t { x = -halfExtentX, y = halfExtentY, z = halfExtentZ }
            ]
        )


blockOfTetrahedrons : Float -> Float -> Float -> List Convex.Convex
blockOfTetrahedrons sizeX sizeY sizeZ =
    let
        lX =
            sizeX / 2

        lY =
            sizeY / 2

        lZ =
            sizeZ / 2

        p0 =
            { x = 0, y = 0, z = 0 }
    in
    List.map
        (\( p1, p2, p3 ) ->
            Convex.fromTriangularMesh
                [ ( 1, 0, 2 ), ( 2, 0, 3 ), ( 3, 0, 1 ), ( 3, 1, 2 ) ]
                (Array.fromList [ p0, p1, p2, p3 ])
        )
        [ ( { x = lX, y = lY, z = -lZ }, { x = -lX, y = lY, z = lZ }, { x = lX, y = lY, z = lZ } )
        , ( { x = lX, y = lY, z = -lZ }, { x = -lX, y = lY, z = -lZ }, { x = -lX, y = lY, z = lZ } )
        , ( { x = lX, y = -lY, z = lZ }, { x = -lX, y = lY, z = lZ }, { x = -lX, y = -lY, z = lZ } )
        , ( { x = lX, y = -lY, z = lZ }, { x = lX, y = lY, z = lZ }, { x = -lX, y = lY, z = lZ } )
        , ( { x = -lX, y = -lY, z = lZ }, { x = -lX, y = lY, z = -lZ }, { x = -lX, y = -lY, z = -lZ } )
        , ( { x = -lX, y = -lY, z = lZ }, { x = -lX, y = lY, z = lZ }, { x = -lX, y = lY, z = -lZ } )
        , ( { x = -lX, y = -lY, z = -lZ }, { x = lX, y = -lY, z = lZ }, { x = -lX, y = -lY, z = lZ } )
        , ( { x = -lX, y = -lY, z = -lZ }, { x = lX, y = -lY, z = -lZ }, { x = lX, y = -lY, z = lZ } )
        , ( { x = lX, y = -lY, z = -lZ }, { x = lX, y = lY, z = lZ }, { x = lX, y = -lY, z = lZ } )
        , ( { x = lX, y = -lY, z = -lZ }, { x = lX, y = lY, z = -lZ }, { x = lX, y = lY, z = lZ } )
        , ( { x = -lX, y = -lY, z = -lZ }, { x = lX, y = lY, z = -lZ }, { x = lX, y = -lY, z = -lZ } )
        , ( { x = -lX, y = -lY, z = -lZ }, { x = -lX, y = lY, z = -lZ }, { x = lX, y = lY, z = -lZ } )
        ]


octoHull : Float -> Convex.Convex
octoHull halfExtent =
    Convex.fromTriangularMesh
        [ ( 2, 1, 0 )
        , ( 0, 5, 2 )
        , ( 1, 2, 4 )
        , ( 3, 0, 1 )
        , ( 2, 5, 4 )
        , ( 4, 3, 1 )
        , ( 5, 0, 3 )
        , ( 3, 4, 5 )
        ]
        (Array.fromList
            [ { x = 0, y = 0, z = halfExtent }
            , { x = 0, y = halfExtent, z = 0 }
            , { x = halfExtent, y = 0, z = 0 }
            , { x = -halfExtent, y = 0, z = 0 }
            , { x = 0, y = 0, z = -halfExtent }
            , { x = 0, y = -halfExtent, z = 0 }
            ]
        )


squarePyramid : Convex
squarePyramid =
    -- Specify 0 for exact precision
    squareLikePyramid 0.0


askewSquarePyramid : Convex
askewSquarePyramid =
    -- Use an insignificant epsilon for an approximately square base
    squareLikePyramid (Const.precision / 3.0)


nonSquareQuadPyramid : Convex
nonSquareQuadPyramid =
    -- Use a significant epsilon for a not even approximately square base
    squareLikePyramid (Const.precision * 3.0)


squareLikePyramid : Float -> Convex
squareLikePyramid epsilon =
    let
        x =
            1

        y =
            1

        z =
            1

        -- zOffset is the height of the pyramid's center of gravity above its
        -- base -- the cube root of 1/2.
        -- It serves to keep the object vertically centered.
        zOffset =
            z * (0.5 ^ (1.0 / 3.0))

        vertexIndices =
            [ ( 3, 2, 1 )
            , ( 3, 1, 0 )
            , ( 0, 1, 4 )
            , ( 1, 2, 4 )
            , ( 2, 3, 4 )
            , ( 3, 0, 4 )
            ]

        vertices =
            Array.fromList
                [ { x = -x, y = -y, z = -zOffset }
                , { x = x, y = -y, z = -zOffset }

                -- An optional adjustment of one base corner controls
                -- the number (0 or 2) of edge pairs that are exactly
                -- parallel OR approximately parallel.
                , { x = x + epsilon, y = y + epsilon, z = -zOffset }
                , { x = -x, y = y, z = -zOffset }
                , { x = 0, y = 0, z = z - zOffset }
                ]
    in
    Convex.fromTriangularMesh vertexIndices vertices


hugeConvex : Convex
hugeConvex =
    Convex.fromTriangularMesh
        [ ( 32, 58, 33 )
        , ( 61, 56, 44 )
        , ( 55, 52, 7 )
        , ( 0, 3, 25 )
        , ( 0, 2, 3 )
        , ( 48, 9, 12 )
        , ( 37, 15, 25 )
        , ( 15, 0, 25 )
        , ( 33, 45, 41 )
        , ( 33, 58, 45 )
        , ( 2, 0, 17 )
        , ( 17, 21, 18 )
        , ( 18, 21, 12 )
        , ( 7, 19, 5 )
        , ( 6, 22, 31 )
        , ( 4, 22, 6 )
        , ( 40, 26, 37 )
        , ( 25, 28, 29 )
        , ( 24, 26, 22 )
        , ( 27, 29, 41 )
        , ( 26, 31, 22 )
        , ( 35, 34, 29 )
        , ( 35, 23, 36 )
        , ( 34, 35, 36 )
        , ( 27, 38, 25 )
        , ( 30, 40, 44 )
        , ( 42, 40, 37 )
        , ( 34, 41, 29 )
        , ( 41, 34, 33 )
        , ( 41, 45, 43 )
        , ( 44, 42, 39 )
        , ( 43, 39, 42 )
        , ( 13, 36, 23 )
        , ( 46, 48, 49 )
        , ( 48, 51, 49 )
        , ( 47, 49, 50 )
        , ( 50, 54, 53 )
        , ( 54, 51, 52 )
        , ( 58, 59, 60 )
        , ( 61, 58, 60 )
        , ( 45, 39, 43 )
        , ( 61, 44, 39 )
        , ( 61, 60, 56 )
        , ( 55, 7, 6 )
        , ( 52, 19, 7 )
        , ( 52, 51, 19 )
        , ( 51, 48, 16 )
        , ( 18, 12, 10 )
        , ( 58, 32, 57 )
        , ( 58, 61, 45 )
        , ( 61, 39, 45 )
        , ( 54, 52, 55 )
        , ( 3, 28, 25 )
        , ( 38, 37, 25 )
        , ( 1, 14, 0 )
        , ( 9, 8, 10 )
        , ( 8, 13, 10 )
        , ( 8, 32, 13 )
        , ( 32, 36, 13 )
        , ( 26, 24, 37 )
        , ( 53, 54, 56 )
        , ( 54, 55, 56 )
        , ( 60, 62, 56 )
        , ( 62, 53, 56 )
        , ( 27, 25, 29 )
        , ( 46, 8, 9 )
        , ( 14, 17, 0 )
        , ( 46, 47, 8 )
        , ( 47, 32, 8 )
        , ( 47, 57, 32 )
        , ( 50, 59, 47 )
        , ( 35, 29, 28 )
        , ( 43, 27, 41 )
        , ( 50, 53, 59 )
        , ( 49, 47, 46 )
        , ( 36, 32, 34 )
        , ( 32, 33, 34 )
        , ( 56, 30, 44 )
        , ( 56, 31, 30 )
        , ( 4, 6, 5 )
        , ( 6, 7, 5 )
        , ( 16, 21, 20 )
        , ( 43, 38, 27 )
        , ( 43, 42, 38 )
        , ( 42, 37, 38 )
        , ( 11, 3, 2 )
        , ( 23, 3, 11 )
        , ( 18, 2, 17 )
        , ( 42, 44, 40 )
        , ( 51, 16, 19 )
        , ( 16, 48, 12 )
        , ( 48, 46, 9 )
        , ( 59, 57, 47 )
        , ( 53, 62, 59 )
        , ( 62, 60, 59 )
        , ( 50, 49, 54 )
        , ( 49, 51, 54 )
        , ( 24, 15, 37 )
        , ( 24, 22, 15 )
        , ( 22, 4, 15 )
        , ( 4, 1, 15 )
        , ( 1, 0, 15 )
        , ( 23, 35, 3 )
        , ( 35, 28, 3 )
        , ( 30, 26, 40 )
        , ( 30, 31, 26 )
        , ( 5, 1, 4 )
        , ( 5, 14, 1 )
        , ( 14, 20, 17 )
        , ( 20, 21, 17 )
        , ( 18, 11, 2 )
        , ( 5, 20, 14 )
        , ( 18, 10, 11 )
        , ( 10, 13, 11 )
        , ( 13, 23, 11 )
        , ( 56, 6, 31 )
        , ( 56, 55, 6 )
        , ( 19, 20, 5 )
        , ( 19, 16, 20 )
        , ( 16, 12, 21 )
        , ( 12, 9, 10 )
        , ( 57, 59, 58 )
        ]
        (Array.fromList
            [ { x = -0.11459, y = -0.298243, z = 0.100413 }
            , { x = 0.436854, y = -0.300164, z = 0.1143 }
            , { x = -0.409218, y = -0.296382, z = 0.186322 }
            , { x = -0.441714, y = -0.072783, z = 0.146552 }
            , { x = 0.733418, y = -0.149194, z = 0.183468 }
            , { x = 0.602326, y = -0.395949, z = 0.281944 }
            , { x = 0.818608, y = -0.167292, z = 0.42114 }
            , { x = 0.660817, y = -0.392449, z = 0.485555 }
            , { x = -0.547257, y = -0.088449, z = 0.899761 }
            , { x = -0.536566, y = -0.269735, z = 0.747375 }
            , { x = -0.608158, y = -0.285728, z = 0.559957 }
            , { x = -0.57747, y = -0.2151, z = 0.335446 }
            , { x = -0.149689, y = -0.515712, z = 0.480973 }
            , { x = -0.679758, y = -0.06998, z = 0.585114 }
            , { x = 0.151798, y = -0.364164, z = 0.1141 }
            , { x = 0.55179, y = -0.114214, z = 0.100309 }
            , { x = 0.09126, y = -0.527181, z = 0.558675 }
            , { x = -0.130679, y = -0.456457, z = 0.235531 }
            , { x = -0.317681, y = -0.457769, z = 0.366178 }
            , { x = 0.46511, y = -0.477899, z = 0.427776 }
            , { x = 0.282, y = -0.514911, z = 0.349703 }
            , { x = 0.0303, y = -0.524675, z = 0.354574 }
            , { x = 0.759308, y = 0.10619, z = 0.182734 }
            , { x = -0.64683, y = 0.01928, z = 0.341592 }
            , { x = 0.570919, y = 0.095929, z = 0.100917 }
            , { x = -0.25119, y = 0.30186, z = 0.099294 }
            , { x = 0.555386, y = 0.302365, z = 0.114663 }
            , { x = -0.227101, y = 0.481723, z = 0.183816 }
            , { x = -0.441233, y = 0.186976, z = 0.144447 }
            , { x = -0.45535, y = 0.385063, z = 0.236628 }
            , { x = 0.646226, y = 0.465931, z = 0.415905 }
            , { x = 0.806278, y = 0.239899, z = 0.353877 }
            , { x = -0.617107, y = 0.209783, z = 0.81899 }
            , { x = -0.46719, y = 0.480974, z = 0.553544 }
            , { x = -0.589689, y = 0.355858, z = 0.437199 }
            , { x = -0.592654, y = 0.202361, z = 0.279119 }
            , { x = -0.68018, y = 0.178183, z = 0.582133 }
            , { x = 0.254849, y = 0.379992, z = 0.099383 }
            , { x = -0.032588, y = 0.439714, z = 0.11387 }
            , { x = 0.099846, y = 0.607803, z = 0.480417 }
            , { x = 0.465132, y = 0.51505, z = 0.29225 }
            , { x = -0.323879, y = 0.526379, z = 0.37056 }
            , { x = 0.237499, y = 0.576722, z = 0.294563 }
            , { x = -0.068028, y = 0.572111, z = 0.301029 }
            , { x = 0.461069, y = 0.549594, z = 0.497648 }
            , { x = -0.199928, y = 0.554805, z = 0.570804 }
            , { x = 0.004558, y = -0.167478, z = 1.44564 }
            , { x = 0.084454, y = -0.020208, z = 1.57338 }
            , { x = 0.206856, y = -0.363481, z = 1.3067 }
            , { x = 0.20283, y = -0.233369, z = 1.52392 }
            , { x = 0.280317, y = -0.054322, z = 1.62761 }
            , { x = 0.376109, y = -0.323103, z = 1.41943 }
            , { x = 0.531913, y = -0.304605, z = 1.29392 }
            , { x = 0.507305, y = 0.021512, z = 1.59939 }
            , { x = 0.560087, y = -0.21699, z = 1.47327 }
            , { x = 0.934426, y = 0.045556, z = 1.25358 }
            , { x = 0.955972, y = 0.229201, z = 1.36005 }
            , { x = -0.010552, y = 0.166565, z = 1.46812 }
            , { x = 0.09539, y = 0.353323, z = 1.39163 }
            , { x = 0.208829, y = 0.198483, z = 1.59289 }
            , { x = 0.279242, y = 0.362635, z = 1.48749 }
            , { x = 0.312043, y = 0.451888, z = 1.27797 }
            , { x = 0.499772, y = 0.253275, z = 1.55213 }
            ]
        )
