module Fixtures.Convex exposing
    ( askewSquarePyramid
    , boxHull
    , boxVertexIndices
    , nonSquareQuadPyramid
    , octoHull
    , octoVertexIndices
    , octoVertices
    , originalBoxHull
    , originalOctoHull
    , squareLikePyramid
    , squarePyramid
    )

import Array exposing (Array)
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Vector3 exposing (Vec3)



-- Test data generators


{-| A Convex for a cube with the given half-extent, constructed
using optimized box-specific initializers.
-}
boxHull : Float -> Convex
boxHull halfExtent =
    Convex.fromBox { x = halfExtent, y = halfExtent, z = halfExtent }


originalBoxHull : Float -> Convex
originalBoxHull halfExtent =
    Convex.fromBox { x = halfExtent, y = halfExtent, z = halfExtent }


boxVertexIndices : List (List Int)
boxVertexIndices =
    [ [ 3, 2, 1, 0 ]
    , [ 4, 5, 6, 7 ]
    , [ 5, 4, 0, 1 ]
    , [ 2, 3, 7, 6 ]
    , [ 0, 4, 7, 3 ]
    , [ 1, 2, 6, 5 ]
    ]


octoVertices : Float -> Array Vec3
octoVertices halfExtent =
    Array.fromList
        [ { x = 0, y = 0, z = halfExtent }
        , { x = 0, y = halfExtent, z = 0 }
        , { x = halfExtent, y = 0, z = 0 }
        , { x = -halfExtent, y = 0, z = 0 }
        , { x = 0, y = 0, z = -halfExtent }
        , { x = 0, y = -halfExtent, z = 0 }
        ]


octoHull : Float -> Convex.Convex
octoHull halfExtent =
    octoVertices halfExtent
        |> Convex.init octoVertexIndices


originalOctoHull : Float -> Convex.Convex
originalOctoHull halfExtent =
    octoVertices halfExtent
        |> Convex.init octoVertexIndices


octoVertexIndices : List (List Int)
octoVertexIndices =
    [ [ 2, 1, 0 ]
    , [ 0, 5, 2 ]
    , [ 1, 2, 4 ]
    , [ 3, 0, 1 ]
    , [ 2, 5, 4 ]
    , [ 4, 3, 1 ]
    , [ 5, 0, 3 ]
    , [ 3, 4, 5 ]
    ]


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

        faces =
            [ [ 3, 2, 1, 0 ]
            , [ 0, 1, 4 ]
            , [ 1, 2, 4 ]
            , [ 2, 3, 4 ]
            , [ 3, 0, 4 ]
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
    Convex.init faces vertices
