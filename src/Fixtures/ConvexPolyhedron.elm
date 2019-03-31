module Fixtures.ConvexPolyhedron exposing (askewSquarePyramid, boxHull, boxVertexIndices, boxyHull, nonSquareQuadPyramid, octoHull, octoVertexIndices, octoVertices, originalBoxHull, originalOctoHull, squareLikePyramid, squarePyramid, vec3HalfExtent)

import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Array exposing (Array)
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)



-- Test data generators


vec3HalfExtent : Float -> Vec3
vec3HalfExtent halfExtent =
    vec3 halfExtent halfExtent halfExtent


{-| A ConvexPolyhedron for a cube with the given half-extent, constructed
using optimized box-specific initializers.
-}
boxHull : Float -> ConvexPolyhedron
boxHull halfExtent =
    ConvexPolyhedron.fromBox <| vec3HalfExtent halfExtent


originalBoxHull : Float -> ConvexPolyhedron
originalBoxHull halfExtent =
    ConvexPolyhedron.fromBox <| vec3HalfExtent halfExtent


{-| A replacement for boxhull/ConvexPolyhedron.fromBox that introduces some
minor imprecision into one of the box vertices and can NOT be constructed
using optimized box-specific initializers.
-}
boxyHull : Float -> ConvexPolyhedron
boxyHull halfExtent =
    let
        vertices =
            Array.fromList
                [ vec3 -halfExtent -halfExtent -halfExtent
                , vec3 halfExtent -halfExtent -halfExtent
                , vec3 halfExtent halfExtent -halfExtent
                , vec3 -halfExtent halfExtent -halfExtent
                , vec3 -halfExtent -halfExtent halfExtent
                , vec3 halfExtent -halfExtent halfExtent

                -- Insignificantly adjust two vertex coordinates to force the 3
                -- connected edges to be insignificantly off-parallel.
                -- This should NOT alter the number of uniqueEdges
                , vec3 halfExtent (halfExtent - Const.precision / 3.0) (halfExtent + Const.precision / 3.0)
                , vec3 -halfExtent halfExtent halfExtent
                ]
    in
    -- To test the handling of minor imprecision in a general
    -- ConvexPolyhedron, purposely bypass the box-specific
    -- optimizations in boxNormals and boxEdges and use instead
    -- the general purpose calculations.
    ConvexPolyhedron.init boxVertexIndices vertices


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
    [ vec3 0 0 halfExtent
    , vec3 0 halfExtent 0
    , vec3 halfExtent 0 0
    , vec3 -halfExtent 0 0
    , vec3 0 0 -halfExtent
    , vec3 0 -halfExtent 0
    ]
        |> Array.fromList


octoHull : Float -> ConvexPolyhedron.ConvexPolyhedron
octoHull halfExtent =
    octoVertices halfExtent
        |> ConvexPolyhedron.init octoVertexIndices


originalOctoHull : Float -> ConvexPolyhedron.ConvexPolyhedron
originalOctoHull halfExtent =
    octoVertices halfExtent
        |> ConvexPolyhedron.init octoVertexIndices


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


squarePyramid : ConvexPolyhedron
squarePyramid =
    -- Specify 0 for exact precision
    squareLikePyramid 0.0


askewSquarePyramid : ConvexPolyhedron
askewSquarePyramid =
    -- Use an insignificant epsilon for an approximately square base
    squareLikePyramid (Const.precision / 3.0)


nonSquareQuadPyramid : ConvexPolyhedron
nonSquareQuadPyramid =
    -- Use a significant epsilon for a not even approximately square base
    squareLikePyramid (Const.precision * 3.0)


squareLikePyramid : Float -> ConvexPolyhedron
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
                [ vec3 -x -y -zOffset
                , vec3 x -y -zOffset

                -- An optional adjustment of one base corner controls
                -- the number (0 or 2) of edge pairs that are exactly
                -- parallel OR approximately parallel.
                , vec3 (x + epsilon) (y + epsilon) -zOffset
                , vec3 -x y -zOffset
                , vec3 0 0 (z - zOffset)
                ]
    in
    ConvexPolyhedron.init faces vertices
