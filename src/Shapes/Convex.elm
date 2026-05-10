module Shapes.Convex exposing
    ( Convex
    , Face
    , computeNormal
    , expandBoundingSphereRadius
    , extendContour
    , foldFaceEdges
    , fromBlock
    , fromCylinder
    , fromTriangularMesh
    , placeIn
    , raycast
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Set exposing (Set)


type alias Face =
    { vertices : List Vec3
    , normal : Vec3
    }


type alias Convex =
    -- Faces grouped by parallel-or-antiparallel normal direction. For a
    -- convex polytope each group has at most two faces (one face plus its
    -- antipodal partner — coplanar adjacent triangles are merged in
    -- `initFacesHelp`, so non-trivial parallel pairs never arise). The
    -- partner is `Nothing` for a 1-face group (e.g. an icosphere face
    -- with no antipode in the mesh) or `Just` the antipodal partner.
    --
    -- `uniqueEdges` groups edges by parallel-or-antiparallel direction:
    -- each entry is `( firstEdge, [ otherParallelEdges... ] )`. The
    -- first edge doubles as the direction representative — collision
    -- code derives the direction via `Vec3.direction v1 v2` on demand
    -- instead of carrying a separate normalized Vec3. Each physical
    -- edge appears exactly once across all groups (no face-shared
    -- double-visit); its `(dirIdx, edgeIdx)` is stable under `placeIn`
    -- so collision code can encode the indices into contact ids for
    -- warm-start cache stability.
    { faces : List ( Face, Maybe Face )
    , vertices : List Vec3 -- cached for performance
    , uniqueEdges : List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) )
    , position : Vec3
    , inertia : Mat3
    , volume : Float
    }


placeIn : Transform3d coordinates defines -> Convex -> Convex
placeIn transform3d { faces, vertices, uniqueEdges, position, volume, inertia } =
    { faces = faceGroupsPlaceInHelp transform3d faces []
    , vertices = Transform3d.pointsPlaceIn transform3d vertices
    , uniqueEdges = uniqueEdgesPlaceIn transform3d uniqueEdges []
    , volume = volume
    , position = Transform3d.pointPlaceIn transform3d position
    , inertia = Transform3d.inertiaRotateIn transform3d inertia
    }


{-| Place each unique-edge group: transform every endpoint pair (first
edge + others). Outer-group order and inner-edge order are reversed by
the prepend-into-accumulator pattern, but collision code derives
direction symmetrically (parallel-or-antiparallel match) and reads
midpoints symmetrically (`v1.x + v2.x`), so neither reversal is
visible. The reversals are stable per source-convex+transform, so
`(dirIdx, edgeIdx)` indices used as warm-start cache keys remain stable
across frames.
-}
uniqueEdgesPlaceIn : Transform3d coordinates defines -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) )
uniqueEdgesPlaceIn transform3d groups result =
    case groups of
        ( firstEdge, otherEdges ) :: rest ->
            uniqueEdgesPlaceIn
                transform3d
                rest
                (( placeEdge transform3d firstEdge
                 , uniqueEdgeEndpointsPlaceIn transform3d otherEdges []
                 )
                    :: result
                )

        [] ->
            result


placeEdge : Transform3d coordinates defines -> ( Vec3, Vec3 ) -> ( Vec3, Vec3 )
placeEdge transform3d ( v1, v2 ) =
    ( Transform3d.pointPlaceIn transform3d v1
    , Transform3d.pointPlaceIn transform3d v2
    )


uniqueEdgeEndpointsPlaceIn : Transform3d coordinates defines -> List ( Vec3, Vec3 ) -> List ( Vec3, Vec3 ) -> List ( Vec3, Vec3 )
uniqueEdgeEndpointsPlaceIn transform3d edges result =
    case edges of
        edge :: rest ->
            uniqueEdgeEndpointsPlaceIn
                transform3d
                rest
                (placeEdge transform3d edge :: result)

        [] ->
            result


{-| Place each face group: transform the primary face and any antipodal
partner. Both outer-group order and inner-partner order are reversed by
the prepend-into-accumulator pattern (same as `List.foldl`). Downstream
collision code treats each pair as antiparallel via `primary.normal · axis`
(negated for the partner), which is invariant under any number of reversals.
-}
faceGroupsPlaceInHelp : Transform3d coordinates defines -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face )
faceGroupsPlaceInHelp transform3d groups result =
    case groups of
        ( primary, partner ) :: rest ->
            faceGroupsPlaceInHelp
                transform3d
                rest
                (( placeFace transform3d primary
                 , Maybe.map (placeFace transform3d) partner
                 )
                    :: result
                )

        [] ->
            result


placeFace : Transform3d coordinates defines -> Face -> Face
placeFace transform3d { vertices, normal } =
    { vertices = List.reverse (Transform3d.pointsPlaceIn transform3d vertices)
    , normal = Transform3d.directionPlaceIn transform3d normal
    }


fromTriangularMesh : List ( Int, Int, Int ) -> Array Vec3 -> Convex
fromTriangularMesh faceIndices vertices =
    let
        faces =
            initFaces faceIndices vertices

        allVertices =
            Array.toList vertices

        averageCenter =
            case allVertices of
                { x, y, z } :: rest ->
                    averageCenterHelp rest 1 x y z

                [] ->
                    Vec3.zero

        ( volume, position, inertia ) =
            convexMassProperties averageCenter faceIndices vertices 0 0 0 0 Mat3.zero
    in
    { faces = groupFacesByNormal faces
    , vertices = allVertices
    , uniqueEdges = groupEdgesByDirection faces
    , position = position
    , volume = volume
    , inertia = inertia
    }


{-| Bucket every face's edges into direction groups
(parallel-or-antiparallel). Each physical edge is shared between two
faces, so the same endpoint pair arrives twice; `addEdgeToGroups`
deduplicates by endpoint identity within the matching direction
group.

Each group is `( firstEdge, otherParallelEdges )`. The first edge
doubles as the direction representative; we derive the direction
on demand via `Vec3.direction firstEdge.v1 firstEdge.v2`.

-}
groupEdgesByDirection : List Face -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) )
groupEdgesByDirection faces =
    List.foldl
        (\face groups ->
            foldFaceEdges addEdgeToGroups groups face.vertices
        )
        []
        faces


addEdgeToGroups : Vec3 -> Vec3 -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) )
addEdgeToGroups v1 v2 groups =
    let
        direction =
            Vec3.direction v1 v2
    in
    addEdgeToGroupsHelp v1 v2 direction groups []


addEdgeToGroupsHelp : Vec3 -> Vec3 -> Vec3 -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) ) -> List ( ( Vec3, Vec3 ), List ( Vec3, Vec3 ) )
addEdgeToGroupsHelp v1 v2 direction groups acc =
    case groups of
        [] ->
            -- New direction: prepend a fresh group to the original list.
            -- `acc` holds groups already inspected (in reverse) so we
            -- splice them back in front to preserve insertion order.
            prependReversed acc [ ( ( v1, v2 ), [] ) ]

        (( ( gv1, gv2 ) as firstEdge, otherEdges ) as group) :: rest ->
            let
                groupDirection =
                    Vec3.direction gv1 gv2
            in
            -- Use a length-squared tolerance instead of the component-wise
            -- `almostZero`: nearly-parallel unit vectors can still produce a
            -- cross with components above 1e-6 (e.g., the icosphere's
            -- antipodal edge pairs differ by a few microradians from
            -- 6-decimal OBJ rounding). Splitting them across direction
            -- groups would make `pickSupportEdge` miss the parallel edge
            -- closest to the contact, which produces wildly off contact
            -- points. sin²(0.057°) ≈ 1e-6 is well above the worst case
            -- here and well below any genuine non-parallel direction.
            if Vec3.lengthSquared (Vec3.cross direction groupDirection) - parallelTolerance < 0 then
                if (v1 == gv1 && v2 == gv2) || (v1 == gv2 && v2 == gv1) || hasEdge v1 v2 otherEdges then
                    -- Already added (face-shared): skip.
                    prependReversed acc groups

                else
                    prependReversed acc (( firstEdge, ( v1, v2 ) :: otherEdges ) :: rest)

            else
                addEdgeToGroupsHelp v1 v2 direction rest (group :: acc)


parallelTolerance : Float
parallelTolerance =
    1.0e-6


hasEdge : Vec3 -> Vec3 -> List ( Vec3, Vec3 ) -> Bool
hasEdge v1 v2 edges =
    case edges of
        ( e1, e2 ) :: rest ->
            if (v1 == e1 && v2 == e2) || (v1 == e2 && v2 == e1) then
                True

            else
                hasEdge v1 v2 rest

        [] ->
            False


prependReversed : List a -> List a -> List a
prependReversed src dst =
    case src of
        x :: rest ->
            prependReversed rest (x :: dst)

        [] ->
            dst


{-| Group faces by parallel-or-antiparallel normal direction.

The first face inserted into a group becomes the group's primary face;
a later match becomes the partner. Collision code reads
`primary.normal · axis` directly (and negates for the partner), so
the relationship between primary and partner does not matter for
correctness.

For a convex polytope each direction matches at most two faces (the
upstream coplanar merge in `initFacesHelp` collapses parallel
neighbours), so a third match should never arrive — if one did, we
would replace the existing partner.

Outer order: insert-first (groups in the order their direction was
first encountered).

-}
groupFacesByNormal : List Face -> List ( Face, Maybe Face )
groupFacesByNormal allFaces =
    groupFacesByNormalHelp allFaces []


groupFacesByNormalHelp : List Face -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face )
groupFacesByNormalHelp faces acc =
    case faces of
        [] ->
            acc

        face :: rest ->
            groupFacesByNormalHelp rest (insertFaceByNormal face acc)


insertFaceByNormal : Face -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face )
insertFaceByNormal face groups =
    insertFaceByNormalHelp face groups []


insertFaceByNormalHelp : Face -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face ) -> List ( Face, Maybe Face )
insertFaceByNormalHelp face groups acc =
    case groups of
        [] ->
            -- New direction: append at the end (insertion-order preserved
            -- via `prependReversed acc` putting inspected groups back in
            -- order ahead of the new singleton).
            prependReversed acc [ ( face, Nothing ) ]

        (( primary, _ ) as group) :: rest ->
            if Vec3.almostZero (Vec3.cross primary.normal face.normal) then
                prependReversed acc (( primary, Just face ) :: rest)

            else
                insertFaceByNormalHelp face rest (group :: acc)


convexMassProperties : Vec3 -> List ( Int, Int, Int ) -> Array Vec3 -> Float -> Float -> Float -> Float -> Mat3 -> ( Float, Vec3, Mat3 )
convexMassProperties center faceIndices vertices cX cY cZ totalVolume totalInertia =
    case faceIndices of
        ( i1, i2, i3 ) :: rest ->
            case ( Array.get i1 vertices, Array.get i2 vertices, Array.get i3 vertices ) of
                ( Just p1, Just p2, Just p3 ) ->
                    let
                        newX =
                            (center.x + p1.x + p2.x + p3.x) / 4

                        newY =
                            (center.y + p1.y + p2.y + p3.y) / 4

                        newZ =
                            (center.z + p1.z + p2.z + p3.z) / 4

                        volume =
                            Vec3.dot (Vec3.sub p1 center) (Vec3.cross (Vec3.sub p2 center) (Vec3.sub p3 center)) / 6

                        newInertia =
                            Mat3.tetrahedronInertia volume
                                center
                                p1
                                p2
                                p3
                    in
                    convexMassProperties
                        center
                        rest
                        vertices
                        (cX + newX * volume)
                        (cY + newY * volume)
                        (cZ + newZ * volume)
                        (totalVolume + volume)
                        (Mat3.add totalInertia newInertia)

                _ ->
                    convexMassProperties center rest vertices cX cY cZ totalVolume totalInertia

        [] ->
            let
                centerOfMass =
                    { x = cX / totalVolume
                    , y = cY / totalVolume
                    , z = cZ / totalVolume
                    }

                pointInertia =
                    Mat3.pointInertia totalVolume
                        (centerOfMass.x - center.x)
                        (centerOfMass.y - center.y)
                        (centerOfMass.z - center.z)
            in
            ( totalVolume
            , centerOfMass
              -- inertia about origin = inertia about center of mass + point inertia about origin
              -- inertia about center of mass = inertia about origin - point inertia about origin
            , Mat3.sub totalInertia pointInertia
            )


averageCenterHelp : List Vec3 -> Float -> Float -> Float -> Float -> Vec3
averageCenterHelp vertices n cX cY cZ =
    case vertices of
        { x, y, z } :: rest ->
            averageCenterHelp rest (n + 1) (cX + x) (cY + y) (cZ + z)

        [] ->
            { x = cX / n, y = cY / n, z = cZ / n }


initFaces : List ( Int, Int, Int ) -> Array Vec3 -> List Face
initFaces vertexIndices convexVertices =
    let
        faceByEdgeIndex =
            List.foldl
                (\(( i1, i2, i3 ) as indices) dict ->
                    case Array.get i1 convexVertices of
                        Just p1 ->
                            case Array.get i2 convexVertices of
                                Just p2 ->
                                    case Array.get i3 convexVertices of
                                        Just p3 ->
                                            let
                                                face =
                                                    { indices = indices
                                                    , normal = computeNormal p1 p2 p3
                                                    }
                                            in
                                            dict
                                                |> Dict.insert ( i1, i2 ) face
                                                |> Dict.insert ( i2, i3 ) face
                                                |> Dict.insert ( i3, i1 ) face

                                        Nothing ->
                                            dict

                                Nothing ->
                                    dict

                        Nothing ->
                            dict
                )
                Dict.empty
                vertexIndices
    in
    case vertexIndices of
        (( i1, i2, i3 ) as indices) :: _ ->
            case Array.get i1 convexVertices of
                Just p1 ->
                    case Array.get i2 convexVertices of
                        Just p2 ->
                            case Array.get i3 convexVertices of
                                Just p3 ->
                                    initFacesHelp
                                        (Set.singleton indices)
                                        convexVertices
                                        faceByEdgeIndex
                                        []
                                        [ ( i2, i1 ), ( i3, i2 ), ( i1, i3 ) ]
                                        (computeNormal p1 p2 p3)
                                        [ i1, i2, i3 ]
                                        []

                                Nothing ->
                                    []

                        Nothing ->
                            []

                Nothing ->
                    []

        [] ->
            []


initFacesHelp :
    Set ( Int, Int, Int )
    -> Array Vec3
    -> Dict ( Int, Int ) { indices : ( Int, Int, Int ), normal : Vec3 }
    -> List { indices : ( Int, Int, Int ), normal : Vec3 }
    -> List ( Int, Int )
    -> Vec3
    -> List Int
    -> List Face
    -> List Face
initFacesHelp visited vertices faceByEdgeIndex facesToCheck edgesToCheck currentNormal currentContour result =
    let
        adjacentFaces =
            edgesToCheck
                |> List.filterMap (\edge -> Dict.get edge faceByEdgeIndex)
                |> List.filter (\{ indices } -> not (Set.member indices visited))

        ( coplanar, nonCoplanar ) =
            List.partition
                (\{ normal } -> Vec3.almostZero (Vec3.sub normal currentNormal))
                adjacentFaces

        newVisited =
            List.foldl (\{ indices } -> Set.insert indices) visited coplanar

        newEdgesToCheck =
            List.foldl
                (\{ indices } res ->
                    let
                        ( i1, i2, i3 ) =
                            indices
                    in
                    ( i2, i1 ) :: ( i3, i2 ) :: ( i1, i3 ) :: res
                )
                []
                coplanar

        newFacesToCheck =
            nonCoplanar ++ facesToCheck

        newContour =
            List.foldl (\{ indices } -> extendContour indices) currentContour coplanar
    in
    case coplanar of
        _ :: _ ->
            -- grow the contour
            initFacesHelp
                newVisited
                vertices
                faceByEdgeIndex
                newFacesToCheck
                newEdgesToCheck
                currentNormal
                newContour
                result

        [] ->
            -- couldn’t grow the contour
            let
                faceToAdd =
                    { normal = currentNormal
                    , vertices = List.filterMap (\i -> Array.get i vertices) newContour
                    }

                updatedFacesToCheck =
                    List.filter
                        (\{ indices } -> not (Set.member indices newVisited))
                        newFacesToCheck
            in
            case updatedFacesToCheck of
                -- pick a non coplanar face
                { indices, normal } :: remainingFacesToCheck ->
                    let
                        ( i1, i2, i3 ) =
                            indices
                    in
                    initFacesHelp
                        (Set.insert indices newVisited)
                        vertices
                        faceByEdgeIndex
                        remainingFacesToCheck
                        [ ( i2, i1 ), ( i3, i2 ), ( i1, i3 ) ]
                        normal
                        [ i1, i2, i3 ]
                        (faceToAdd :: result)

                -- end the recursion
                [] ->
                    faceToAdd :: result


extendContour : ( Int, Int, Int ) -> List Int -> List Int
extendContour triangle currentContour =
    case currentContour of
        i1 :: _ :: _ ->
            extendContourHelp triangle i1 currentContour []

        _ ->
            currentContour


extendContourHelp : ( Int, Int, Int ) -> Int -> List Int -> List Int -> List Int
extendContourHelp (( ti1, ti2, ti3 ) as triangle) i1 currentContour result =
    case currentContour of
        ci1 :: rest1 ->
            case rest1 of
                ci2 :: _ ->
                    if (ci1 - ti2 == 0) && (ci2 - ti1 == 0) then
                        -- insert ti3
                        List.reverse result ++ (ci1 :: ti3 :: rest1)

                    else if (ci1 - ti3 == 0) && (ci2 - ti2 == 0) then
                        -- insert ti1
                        List.reverse result ++ (ci1 :: ti1 :: rest1)

                    else if (ci1 - ti1 == 0) && (ci2 - ti3 == 0) then
                        -- insert ti2
                        List.reverse result ++ (ci1 :: ti2 :: rest1)

                    else
                        extendContourHelp triangle i1 rest1 (ci1 :: result)

                [] ->
                    if (ci1 - ti2 == 0) && (i1 - ti1 == 0) then
                        -- insert ti3
                        List.reverse (ti3 :: ci1 :: result)

                    else if (ci1 - ti3 == 0) && (i1 - ti2 == 0) then
                        -- insert ti1
                        List.reverse (ti1 :: ci1 :: result)

                    else if (ci1 - ti1 == 0) && (i1 - ti3 == 0) then
                        -- insert ti2
                        List.reverse (ti2 :: ci1 :: result)

                    else
                        List.reverse result

        [] ->
            List.reverse result


computeNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
computeNormal v1 v2 v3 =
    Vec3.normalize (Vec3.cross (Vec3.sub v3 v2) (Vec3.sub v1 v2))


fromBlock : Float -> Float -> Float -> Convex
fromBlock sizeX sizeY sizeZ =
    let
        x =
            sizeX / 2

        y =
            sizeY / 2

        z =
            sizeZ / 2

        v0 =
            { x = -x, y = -y, z = -z }

        v1 =
            { x = x, y = -y, z = -z }

        v2 =
            { x = x, y = y, z = -z }

        v3 =
            { x = -x, y = y, z = -z }

        v4 =
            { x = -x, y = -y, z = z }

        v5 =
            { x = x, y = -y, z = z }

        v6 =
            { x = x, y = y, z = z }

        v7 =
            { x = -x, y = y, z = z }

        volume =
            sizeX * sizeY * sizeZ

        inertia =
            { m11 = volume / 12 * (sizeY * sizeY + sizeZ * sizeZ)
            , m21 = 0
            , m31 = 0
            , m12 = 0
            , m22 = volume / 12 * (sizeX * sizeX + sizeZ * sizeZ)
            , m32 = 0
            , m13 = 0
            , m23 = 0
            , m33 = volume / 12 * (sizeY * sizeY + sizeX * sizeX)
            }
    in
    { faces =
        -- Each entry is `( primaryFace, Just antipodalPartner )`. Pair
        -- order and choice of which face is primary are arbitrary for
        -- collision — `bestFace` reads `primary.normal · axis` directly
        -- and negates for the partner.
        --
        -- The layout below is chosen so that AFTER a single `placeIn` (how
        -- collision tests set up bodies, which reverses the outer list), the
        -- flat traversal order is `[+x, -x, +y, -y, +z, -z]` — the IDs that
        -- the `-fN` assertions in `CapsuleConvexTest` reference.
        [ ( { vertices = [ v4, v5, v6, v7 ], normal = Vec3.zAxis }
          , Just { vertices = [ v3, v2, v1, v0 ], normal = Vec3.zNegative }
          )
        , ( { vertices = [ v2, v3, v7, v6 ], normal = Vec3.yAxis }
          , Just { vertices = [ v5, v4, v0, v1 ], normal = Vec3.yNegative }
          )
        , ( { vertices = [ v1, v2, v6, v5 ], normal = Vec3.xAxis }
          , Just { vertices = [ v0, v4, v7, v3 ], normal = Vec3.xNegative }
          )
        ]
    , vertices = [ v0, v1, v2, v3, v4, v5, v6, v7 ]
    , uniqueEdges =
        -- Three direction groups, each with four parallel edges of the
        -- cube — first edge in each group acts as the direction
        -- representative. Endpoints are picked so the edge "starts" at
        -- the vertex with the smaller axis-component along the
        -- direction (so the implicit direction is +xAxis / +yAxis /
        -- +zAxis respectively), keeping the data canonical for
        -- testing.
        [ ( ( v0, v1 ), [ ( v3, v2 ), ( v4, v5 ), ( v7, v6 ) ] )
        , ( ( v0, v3 ), [ ( v1, v2 ), ( v4, v7 ), ( v5, v6 ) ] )
        , ( ( v0, v4 ), [ ( v1, v5 ), ( v2, v6 ), ( v3, v7 ) ] )
        ]
    , volume = volume
    , position = Vec3.zero
    , inertia = inertia
    }


fromCylinder : Int -> Float -> Float -> Convex
fromCylinder subdivisions radius length =
    let
        top =
            length * 0.5

        bottom =
            length * -0.5

        sides =
            List.map
                (\value ->
                    let
                        r0 =
                            2 * pi * (toFloat value - 0.5) / toFloat subdivisions

                        r1 =
                            2 * pi * toFloat value / toFloat subdivisions

                        r2 =
                            2 * pi * (toFloat value + 0.5) / toFloat subdivisions
                    in
                    { normal = { x = sin r1, y = cos r1, z = 0 }
                    , v0 = { x = sin r0 * radius, y = cos r0 * radius, z = top }
                    , v1 = { x = sin r2 * radius, y = cos r2 * radius, z = top }
                    , v2 = { x = sin r2 * radius, y = cos r2 * radius, z = bottom }
                    , v3 = { x = sin r0 * radius, y = cos r0 * radius, z = bottom }
                    }
                )
                (List.range 0 (subdivisions - 1))

        volume =
            2 * pi * length * radius ^ 2

        cap z =
            List.map
                (\value ->
                    let
                        r0 =
                            2 * pi * (toFloat value - 0.5) / toFloat subdivisions
                    in
                    { x = sin r0 * radius, y = cos r0 * radius, z = z }
                )
                (List.range 0 (subdivisions - 1))

        bottomCap =
            { vertices = cap bottom, normal = Vec3.zNegative }

        topCap =
            { vertices = List.reverse (cap top), normal = Vec3.zAxis }

        sideFaces =
            List.map
                (\{ v0, v1, v2, v3, normal } ->
                    { vertices = [ v0, v1, v2, v3 ], normal = normal }
                )
                sides

        allFaces =
            topCap :: bottomCap :: sideFaces
    in
    { faces = groupFacesByNormal allFaces
    , vertices = topCap.vertices ++ bottomCap.vertices
    , uniqueEdges = groupEdgesByDirection allFaces
    , position = Vec3.zero
    , inertia = Mat3.cylinderInertia volume radius length
    , volume = volume
    }


expandBoundingSphereRadius : Convex -> Float -> Float
expandBoundingSphereRadius { vertices } boundingSphereRadius =
    vertices
        |> List.foldl
            (\vertex ->
                max (Vec3.lengthSquared vertex)
            )
            (boundingSphereRadius * boundingSphereRadius)
        |> sqrt


raycast : { from : Vec3, direction : Vec3 } -> Convex -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { direction, from } convex =
    case convex.faces of
        ( primary, partner ) :: rest ->
            raycastWalk direction from primary partner rest Nothing

        [] ->
            Nothing


raycastWalk : Vec3 -> Vec3 -> Face -> Maybe Face -> List ( Face, Maybe Face ) -> Maybe { distance : Float, point : Vec3, normal : Vec3 } -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycastWalk direction from currentFace nextFace queuedGroups maybeHit =
    let
        newHit =
            raycastFace direction from currentFace maybeHit
    in
    case nextFace of
        Just face ->
            raycastWalk direction from face Nothing queuedGroups newHit

        Nothing ->
            case queuedGroups of
                ( primary, partner ) :: restGroups ->
                    raycastWalk direction from primary partner restGroups newHit

                [] ->
                    newHit


raycastFace : Vec3 -> Vec3 -> Face -> Maybe { distance : Float, point : Vec3, normal : Vec3 } -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycastFace direction from { normal, vertices } maybeHit =
    let
        dot =
            Vec3.dot direction normal

        point =
            case vertices of
                first :: _ ->
                    first

                [] ->
                    Vec3.zero
    in
    if dot < 0 then
        let
            pointToFrom =
                Vec3.sub point from

            scalar =
                Vec3.dot normal pointToFrom / dot
        in
        if scalar >= 0 then
            let
                intersectionPoint =
                    { x = direction.x * scalar + from.x
                    , y = direction.y * scalar + from.y
                    , z = direction.z * scalar + from.z
                    }

                isInsidePolygon =
                    foldFaceEdges
                        (\p1 p2 result ->
                            result
                                && (Vec3.dot
                                        (Vec3.sub intersectionPoint p1)
                                        (Vec3.cross normal (Vec3.sub p2 p1))
                                        > 0
                                   )
                        )
                        True
                        vertices
            in
            if isInsidePolygon then
                case maybeHit of
                    Just { distance } ->
                        if scalar - distance < 0 then
                            Just
                                { distance = scalar
                                , point = intersectionPoint
                                , normal = normal
                                }

                        else
                            maybeHit

                    Nothing ->
                        Just
                            { distance = scalar
                            , point = intersectionPoint
                            , normal = normal
                            }

            else
                maybeHit

        else
            maybeHit

    else
        maybeHit


{-| Map the function to pairs of consecutive vertices,
starting with the pair (first, second), and so on, until (last, first).
-}
foldFaceEdges : (Vec3 -> Vec3 -> b -> b) -> b -> List Vec3 -> b
foldFaceEdges fn resultSeed vertices =
    case vertices of
        first :: _ :: _ ->
            foldFaceEdgesHelp fn first resultSeed vertices

        _ ->
            -- The list is empty or contains one element.
            resultSeed


foldFaceEdgesHelp : (Vec3 -> Vec3 -> b -> b) -> Vec3 -> b -> List Vec3 -> b
foldFaceEdgesHelp fn seed resultSeed vertices =
    case vertices of
        el1 :: rest1 ->
            case rest1 of
                [] ->
                    fn el1 seed resultSeed

                el2 :: _ ->
                    foldFaceEdgesHelp
                        fn
                        seed
                        (fn el1 el2 resultSeed)
                        rest1

        [] ->
            resultSeed
