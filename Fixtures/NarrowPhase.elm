module Fixtures.NarrowPhase exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Const as Const


sphereContactBoxPositions : Float -> Float -> List Vec3
sphereContactBoxPositions radius boxHalfExtent =
    let
        delta =
            3 * Const.precision

        nearEdgeOffset =
            boxHalfExtent - delta

        -- Reposition the box so that it contacts the sphere at each:
        -- vertex
        -- edge (midpoint)
        -- face (center)
        -- face (at a point near a vertex)
        -- face (at a point near an edge midpoint).
        -- The adjustment of -Const.precision represents a minimum
        -- penetration value.
        vertexDimension =
            boxHalfExtent + radius / (sqrt 3) - Const.precision

        edgeDimension =
            boxHalfExtent + radius / (sqrt 2) - Const.precision

        faceDimension =
            boxHalfExtent + radius - Const.precision
    in
        -- the 8 vertex contacts
        [ (vec3 vertexDimension vertexDimension vertexDimension)
        , (vec3 (-vertexDimension) vertexDimension vertexDimension)
        , (vec3 vertexDimension (-vertexDimension) vertexDimension)
        , (vec3 (-vertexDimension) (-vertexDimension) vertexDimension)
        , (vec3 vertexDimension vertexDimension (-vertexDimension))
        , (vec3 (-vertexDimension) vertexDimension (-vertexDimension))
        , (vec3 vertexDimension (-vertexDimension) (-vertexDimension))
        , (vec3 (-vertexDimension) (-vertexDimension) (-vertexDimension))
        
        -- the 12 edge (midpoint) contacts
        , (vec3 edgeDimension edgeDimension 0)
        , (vec3 0 edgeDimension edgeDimension)
        , (vec3 edgeDimension 0 edgeDimension)
        , (vec3 (-edgeDimension) edgeDimension 0)
        , (vec3 0 (-edgeDimension) edgeDimension)
        , (vec3 edgeDimension 0 (-edgeDimension))
        , (vec3 edgeDimension (-edgeDimension) 0)
        , (vec3 0 edgeDimension (-edgeDimension))
        , (vec3 (-edgeDimension) 0 edgeDimension)
        , (vec3 (-edgeDimension) (-edgeDimension) 0)
        , (vec3 0 (-edgeDimension) (-edgeDimension))
        , (vec3 (-edgeDimension) 0 (-edgeDimension))

        -- the 6 face (center) contacts
        , (vec3 faceDimension 0 0)
        , (vec3 0 faceDimension 0)
        , (vec3 0 0 faceDimension)
        , (vec3 (-faceDimension) 0 0)
        , (vec3 0 (-faceDimension) 0)
        , (vec3 0 0 (-faceDimension))

        -- 3 sample face contacts very near a vertex
        , (vec3 nearEdgeOffset faceDimension nearEdgeOffset)
        , (vec3 (-faceDimension) nearEdgeOffset nearEdgeOffset)
        , (vec3 nearEdgeOffset nearEdgeOffset (-faceDimension))

        -- 3 sample face contacts very near an edge (midpoint)
        , (vec3 faceDimension nearEdgeOffset 0)
        , (vec3 nearEdgeOffset 0 faceDimension)
        , (vec3 0 (-faceDimension) nearEdgeOffset)
        ]
