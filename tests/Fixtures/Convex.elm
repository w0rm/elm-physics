module Fixtures.Convex exposing
    ( askewSquarePyramid
    , nonSquareQuadPyramid
    , octoHull
    , squarePyramid
    )

import Array
import Internal.Const as Const
import Shapes.Convex as Convex exposing (Convex)



-- Test data generators


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
