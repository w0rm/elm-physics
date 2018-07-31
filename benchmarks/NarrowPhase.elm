module NarrowPhase exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Body as Body exposing (Body)
import Physics.Const as Const
import Physics.ConvexPolyhedron as ConvexPolyhedron
import Physics.NarrowPhase as NarrowPhase
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform


{- For a useful benchmark,
   copy and rename an older baseline version of Physics/NarrowPhase.elm
   to Physics/OriginalNarrowPhase.elm and toggle the import below
   from:

      import Physics.NarrowPhase as OriginalNarrowPhase

    to:

      import Physics.OriginalNarrowPhase as OriginalNarrowPhase

    Switching it back to use the (current) NarrowPhase.elm through the
    OriginalNarrowPhase alias keeps obsolete or redundant code out of
    the repo while the comparison benchmarks continue to be maintained and
    built and run essentially as absolute non-comparison benchmarks until
    they are needed again in another round of performance work.
-}

import Physics.NarrowPhase as OriginalNarrowPhase
import Physics.ConvexPolyhedron as OriginalConvexPolyhedron


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        radius =
            1

        boxHalfExtent =
            1

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

        positions =
            [ (vec3 0 0 0)

            -- 8 vertex contacts
            , (vec3 vertexDistance vertexDistance vertexDistance)
            , (vec3 (-vertexDistance) vertexDistance vertexDistance)
            , (vec3 vertexDistance (-vertexDistance) vertexDistance)
            , (vec3 (-vertexDistance) (-vertexDistance) vertexDistance)
            , (vec3 vertexDistance vertexDistance (-vertexDistance))
            , (vec3 (-vertexDistance) vertexDistance (-vertexDistance))
            , (vec3 vertexDistance (-vertexDistance) (-vertexDistance))
            , (vec3 (-vertexDistance) (-vertexDistance) (-vertexDistance))

            -- 12 edge (midpoint) contacts
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

            -- 6 face (center) contacts
            , (vec3 faceDistance 0 0)
            , (vec3 0 faceDistance 0)
            , (vec3 0 0 faceDistance)
            , (vec3 (-faceDistance) 0 0)
            , (vec3 0 (-faceDistance) 0)
            , (vec3 0 0 (-faceDistance))

            -- 3 face contacts near vertex
            , (vec3 nearEdgeOffset faceDistance nearEdgeOffset)
            , (vec3 (-faceDistance) nearEdgeOffset nearEdgeOffset)
            , (vec3 nearEdgeOffset nearEdgeOffset (-faceDistance))

            -- 3 face contacts near edge
            , (vec3 faceDistance nearEdgeOffset 0)
            , (vec3 nearEdgeOffset 0 faceDistance)
            , (vec3 0 (-faceDistance) nearEdgeOffset)
            ]

        vec3HalfExtent =
            (vec3 boxHalfExtent boxHalfExtent boxHalfExtent)

        boxHull =
            ConvexPolyhedron.fromBox vec3HalfExtent

        originalBoxHull =
            OriginalConvexPolyhedron.fromBox vec3HalfExtent

    in
        describe "NarrowPhase"
            [ Benchmark.compare "addSphereConvexContacts"
                "baseline"
                (\_ ->
                    positions
                        |> List.map
                            (\position ->
                                OriginalNarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    originalBoxHull
                                    1
                                    []
                            )
                )
                "latest code"
                (\_ ->
                    positions
                        |> List.map
                            (\position ->
                                NarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    boxHull
                                    1
                                    []
                            )
                )
            ]
