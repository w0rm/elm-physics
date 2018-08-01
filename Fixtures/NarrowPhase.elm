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
        -- face (at a point near an edge midpoint)
        vertexDistance =
            (sqrt 3) * boxHalfExtent + radius

        edgeDistance =
            (sqrt 2) * boxHalfExtent + radius

        faceDistance =
            boxHalfExtent + radius
    in
        -- the 8 vertex contacts
        [ (vec3 vertexDistance vertexDistance vertexDistance)
        , (vec3 (-vertexDistance) vertexDistance vertexDistance)
        , (vec3 vertexDistance (-vertexDistance) vertexDistance)
        , (vec3 (-vertexDistance) (-vertexDistance) vertexDistance)
        , (vec3 vertexDistance vertexDistance (-vertexDistance))
        , (vec3 (-vertexDistance) vertexDistance (-vertexDistance))
        , (vec3 vertexDistance (-vertexDistance) (-vertexDistance))
        , (vec3 (-vertexDistance) (-vertexDistance) (-vertexDistance))

        -- the 12 edge (midpoint) contacts
        , (vec3 faceDistance faceDistance 0)
        , (vec3 0 faceDistance faceDistance)
        , (vec3 faceDistance 0 faceDistance)
        , (vec3 (-faceDistance) faceDistance 0)
        , (vec3 0 (-faceDistance) faceDistance)
        , (vec3 faceDistance 0 (-faceDistance))
        , (vec3 faceDistance (-faceDistance) 0)
        , (vec3 0 faceDistance (-faceDistance))
        , (vec3 (-faceDistance) 0 faceDistance)
        , (vec3 (-faceDistance) (-faceDistance) 0)
        , (vec3 0 (-faceDistance) (-faceDistance))
        , (vec3 (-faceDistance) 0 (-faceDistance))

        -- the 6 face (center) contacts
        , (vec3 faceDistance 0 0)
        , (vec3 0 faceDistance 0)
        , (vec3 0 0 faceDistance)
        , (vec3 (-faceDistance) 0 0)
        , (vec3 0 (-faceDistance) 0)
        , (vec3 0 0 (-faceDistance))

        -- 3 sample face contacts very near a vertex
        , (vec3 nearEdgeOffset faceDistance nearEdgeOffset)
        , (vec3 (-faceDistance) nearEdgeOffset nearEdgeOffset)
        , (vec3 nearEdgeOffset nearEdgeOffset (-faceDistance))

        -- 3 sample face contacts very near an edge (midpoint)
        , (vec3 faceDistance nearEdgeOffset 0)
        , (vec3 nearEdgeOffset 0 faceDistance)
        , (vec3 0 (-faceDistance) nearEdgeOffset)
        ]
