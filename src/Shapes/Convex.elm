module Shapes.Convex exposing
    ( Convex
    , Face
    , FaceGroup(..)
    , FaceVertices
    , Obb(..)
    , computeNormal
    , convexVertices
    , expandBoundingSphereRadius
    , extendContour
    , faceGroupNormal
    , faceVertices
    , foldFaceEdges
    , fromBlock
    , fromCone
    , fromCylinder
    , fromTriangularMesh
    , indexedFaceVertices
    , init
    , placeIn
    , raycast
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Const as Const
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.VertexBuffer as VertexBuffer exposing (VertexBuffer)
import Set exposing (Set)


{-| Two box axes count as perpendicular when |cos θ| between them is below this.
-}
orthogonalTolerance : Float
orthogonalTolerance =
    1.0e-6


type alias Face =
    -- A single face: vertices are indices into the owning convex's
    -- `vertexBuffer`. Materialise with `faceVertices`, passing the buffer from
    -- the `Convex` the caller already holds. Used as the SAT-winner / clip face.
    { vertices : List Int
    , normal : Vec3
    }


{-| A direction group: a primary face and, for a closed polytope, its antipodal
partner — flattened so a group is one object instead of `( Face, Maybe Face )`.
Fields: `normal, vertexIndices, faceDist[, partnerNormal, partnerVertexIndices,
partnerDist]`; `()` pad `OneSidedFace` to the same six-field shape.

`faceDist`/`partnerDist` are the constant body-space signed distances of the
face/partner from the centroid along the (primary) normal. They let face-SAT
project the owning convex onto its own normal as `dist + dot(normal, position)` —
one dot product, no vertex scan — since a rigid body's extent along a body-fixed
direction only shifts by `dot(normal, centroid)` each frame.

-}
type FaceGroup
    = OneSidedFace Vec3 (List Int) Float () () ()
    | TwoSidedFace Vec3 (List Int) Float Vec3 (List Int) Float


{-| Construction-time face: explicit `Vec3` vertices, before `init` packs them
into the shared `VertexBuffer` and replaces them with indices.
-}
type alias FaceVertices =
    { vertices : List Vec3
    , normal : Vec3
    }


type alias Convex =
    -- Faces grouped by parallel/antiparallel normal direction: each group is a
    -- primary face and `Just` its antipodal partner, or `Nothing` for a 1-face
    -- group. uniqueEdges groups edges similarly; each physical edge appears
    -- exactly once. Each group is a flat list of endpoint indices read
    -- two-at-a-time (the first pair is the direction representative). (dirIdx,
    -- edgeIdx) is stable under `placeIn` so collision code can encode indices
    -- into contact ids for warm-start cache stability.
    --
    -- `faces` and `uniqueEdges` hold `Int` indices into `vertexBuffer`, fixed at
    -- `init`. `placeIn` places the buffer once and rebuilds only `faces` (to
    -- place the normals); each face's vertex-index list is shared unchanged and
    -- `uniqueEdges` is shared whole, so no per-vertex `Vec3` list is rebuilt —
    -- collision dereferences indices via `VertexBuffer.get`. `obb` carries the
    -- placed `List Vec3` for a general hull (`NotBox`), or the box's axes +
    -- half-extents for a box (`Box`); see `Obb` and `convexVertices`.
    { faces : List FaceGroup
    , uniqueEdges : List (List Int)
    , vertexBuffer : VertexBuffer
    , obb : Obb
    , position : Vec3
    , inertia : Mat3
    , volume : Float
    }


{-| Box fast-path data, or the placed vertex list for everything else. `Box ax
ay az he`: the placed unit axes (rotated each frame) and constant half-extents
`he` (`he.x` pairs with `ax`, etc.); the centre is `position`. Its eight vertices
are derived on demand (`convexVertices`/`boxCorners`), and SAT projects it in
O(1) via `dot(axis, position) ± Σ|axis·axisᵢ|·heᵢ` — no vertex scan. `NotBox vs`
holds the placed `Vec3` list the general SAT / `PlaneConvex` / `CapsuleConvex`
iterate. `()` pad both to one object shape.
-}
type Obb
    = Box Vec3 Vec3 Vec3 Vec3
    | NotBox (List Vec3) () () ()


{-| Materialise a face's vertices from the convex's buffer, in walk order.
On-demand helper for collision code that still consumes a `List Vec3` (sphere/
capsule/particle/raycast); the convex-convex hot path reads indices directly.
-}
faceVertices : VertexBuffer -> Face -> List Vec3
faceVertices buffer face =
    materialize buffer face.vertices


materialize : VertexBuffer -> List Int -> List Vec3
materialize buffer indices =
    -- IGNORE TCO — short face lists; order feeds clip winding
    case indices of
        i :: rest ->
            VertexBuffer.get i buffer :: materialize buffer rest

        [] ->
            []


{-| Face vertices keyed by their buffer index, `( index, point )`.
-}
indexedFaceVertices : VertexBuffer -> Face -> List ( Int, Vec3 )
indexedFaceVertices buffer face =
    materializeIndexed buffer face.vertices


materializeIndexed : VertexBuffer -> List Int -> List ( Int, Vec3 )
materializeIndexed buffer indices =
    -- IGNORE TCO — short face lists; order feeds clip winding
    case indices of
        i :: rest ->
            ( i, VertexBuffer.get i buffer ) :: materializeIndexed buffer rest

        [] ->
            []


{-| The primary face normal of a group (the partner's is its negation). Lets
collision code that only needs the direction read a group without unpacking it.
-}
faceGroupNormal : FaceGroup -> Vec3
faceGroupNormal group =
    case group of
        OneSidedFace normal _ _ _ _ _ ->
            normal

        TwoSidedFace normal _ _ _ _ _ ->
            normal


placeIn : Transform3d coordinates defines -> Convex -> Convex
placeIn transform3d convex =
    let
        placedBuffer =
            VertexBuffer.map (Transform3d.pointPlaceIn transform3d) convex.vertexBuffer
    in
    { faces = placeFaces transform3d convex.faces []
    , uniqueEdges = convex.uniqueEdges
    , vertexBuffer = placedBuffer
    , obb = placeObb transform3d placedBuffer convex.obb
    , position = Transform3d.pointPlaceIn transform3d convex.position
    , inertia = Transform3d.inertiaRotateIn transform3d convex.inertia
    , volume = convex.volume
    }


{-| Place the box's axes (rotation only; half-extents are invariant), or refold
the non-box's placed vertex list from the buffer. `foldl` visits the buffer in
ascending key order and prepends, so the list comes out in descending key order
— the order the old per-frame placement produced, which `PlaneConvex` keys
contacts by.
-}
placeObb : Transform3d coordinates defines -> VertexBuffer -> Obb -> Obb
placeObb transform3d placedBuffer obb =
    case obb of
        Box ax ay az he ->
            Box
                (Transform3d.directionPlaceIn transform3d ax)
                (Transform3d.directionPlaceIn transform3d ay)
                (Transform3d.directionPlaceIn transform3d az)
                he

        NotBox _ _ _ _ ->
            NotBox (VertexBuffer.foldl (::) [] placedBuffer) () () ()


{-| The convex's placed vertices: the stored list for a general hull, or the
box's eight corners derived from its axes + half-extents.
-}
convexVertices : Convex -> List Vec3
convexVertices convex =
    case convex.obb of
        NotBox vs _ _ _ ->
            vs

        Box ax ay az he ->
            boxCorners convex.position ax ay az he


{-| The eight corners of a box: `centre ± he.x·ax ± he.y·ay ± he.z·az`.
-}
boxCorners : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Vec3
boxCorners c ax ay az he =
    let
        corner sx sy sz =
            { x = c.x + sx * he.x * ax.x + sy * he.y * ay.x + sz * he.z * az.x
            , y = c.y + sx * he.x * ax.y + sy * he.y * ay.y + sz * he.z * az.y
            , z = c.z + sx * he.x * ax.z + sy * he.y * ay.z + sz * he.z * az.z
            }
    in
    [ corner 1 1 1
    , corner 1 1 -1
    , corner 1 -1 1
    , corner 1 -1 -1
    , corner -1 1 1
    , corner -1 1 -1
    , corner -1 -1 1
    , corner -1 -1 -1
    ]


{-| Detect a box: exactly three two-sided face groups with mutually orthogonal
normals. The group normals are the box axes; each `faceDist` is the half-extent
along that axis. Anything else is `NotBox`, carrying its vertex list.
-}
detectObb : List FaceGroup -> List Vec3 -> Obb
detectObb faces vertices =
    case faces of
        [ TwoSidedFace n0 _ d0 _ _ _, TwoSidedFace n1 _ d1 _ _ _, TwoSidedFace n2 _ d2 _ _ _ ] ->
            if orthogonal n0 n1 && orthogonal n1 n2 && orthogonal n0 n2 then
                Box n0 n1 n2 { x = d0, y = d1, z = d2 }

            else
                NotBox vertices () () ()

        _ ->
            NotBox vertices () () ()


orthogonal : Vec3 -> Vec3 -> Bool
orthogonal a b =
    abs (Vec3.dot a b) - orthogonalTolerance < 0


{-| Rebuild the face list, placing each normal; the vertex-index lists are
shared unchanged. Tail-recursive, so it reverses the group order — `init` stores
faces in source order so this single reversal lands the canonical placed order
collision encodes into contact ids (the old per-frame placement reversed too).
-}
placeFaces : Transform3d coordinates defines -> List FaceGroup -> List FaceGroup -> List FaceGroup
placeFaces transform3d faces result =
    case faces of
        (OneSidedFace normal indices faceDist a b c) :: rest ->
            placeFaces transform3d
                rest
                (OneSidedFace (Transform3d.directionPlaceIn transform3d normal) indices faceDist a b c :: result)

        (TwoSidedFace n1 i1 d1 n2 i2 d2) :: rest ->
            placeFaces transform3d
                rest
                (TwoSidedFace
                    (Transform3d.directionPlaceIn transform3d n1)
                    i1
                    d1
                    (Transform3d.directionPlaceIn transform3d n2)
                    i2
                    d2
                    :: result
                )

        [] ->
            result


{-| Assemble a `Convex` from explicit geometry, packing the vertices into a
shared `VertexBuffer` and replacing every face/edge endpoint with its index
(found by structural equality against `vertices`). Edge index lists are stored
in the canonical placed order (outer reversed, each group inner reversed); faces
are stored in source order and reversed each frame by the tail-recursive
`placeFaces`. Either way contact ids built from these positions are unchanged.
-}
init :
    { faces : List ( FaceVertices, Maybe FaceVertices )
    , vertices : List Vec3
    , uniqueEdges : List (List Vec3)
    , position : Vec3
    , inertia : Mat3
    , volume : Float
    }
    -> Convex
init geometry =
    let
        buffer =
            VertexBuffer.fromList geometry.vertices

        faces =
            indexFaces geometry.position geometry.vertices geometry.faces
    in
    { faces = faces
    , uniqueEdges = indexEdges geometry.vertices (List.reverse geometry.uniqueEdges)
    , vertexBuffer = buffer
    , obb = detectObb faces geometry.vertices
    , position = geometry.position
    , inertia = geometry.inertia
    , volume = geometry.volume
    }


indexFaces : Vec3 -> List Vec3 -> List ( FaceVertices, Maybe FaceVertices ) -> List FaceGroup
indexFaces centroid vertices faces =
    List.map
        (\( primary, partner ) ->
            case partner of
                Just p ->
                    TwoSidedFace
                        primary.normal
                        (indexFaceVertices vertices primary)
                        (faceDistance primary.normal centroid primary.vertices)
                        p.normal
                        (indexFaceVertices vertices p)
                        (faceDistance primary.normal centroid p.vertices)

                Nothing ->
                    OneSidedFace
                        primary.normal
                        (indexFaceVertices vertices primary)
                        (faceDistance primary.normal centroid primary.vertices)
                        ()
                        ()
                        ()
        )
        faces


indexFaceVertices : List Vec3 -> FaceVertices -> List Int
indexFaceVertices vertices face =
    List.map (\v -> indexOf v vertices) face.vertices


{-| Signed distance of a face from the centroid along `normal` (constant in body
space — all the face's vertices share it, so the first suffices).
-}
faceDistance : Vec3 -> Vec3 -> List Vec3 -> Float
faceDistance normal centroid vertices =
    case vertices of
        v :: _ ->
            Vec3.dot normal (Vec3.sub v centroid)

        [] ->
            0


{-| Each group's endpoints, reversed so collision walks them in the order the
old per-frame `pointsPlaceIn` produced.
-}
indexEdges : List Vec3 -> List (List Vec3) -> List (List Int)
indexEdges vertices groups =
    List.map (\group -> List.reverse (List.map (\v -> indexOf v vertices) group)) groups


{-| Distinct vertices by structural equality, preserving none-in-particular
order (only feeds the index match + min/max projection). Setup-only.
-}
dedupVertices : List Vec3 -> List Vec3 -> List Vec3
dedupVertices remaining acc =
    case remaining of
        v :: rest ->
            if indexOf v acc < 0 then
                dedupVertices rest (v :: acc)

            else
                dedupVertices rest acc

        [] ->
            acc


indexOf : Vec3 -> List Vec3 -> Int
indexOf target vertices =
    indexOfHelp target vertices 0


indexOfHelp : Vec3 -> List Vec3 -> Int -> Int
indexOfHelp target vertices i =
    case vertices of
        v :: rest ->
            if v == target then
                i

            else
                indexOfHelp target rest (i + 1)

        [] ->
            -1


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
    init
        { faces = groupFacesByNormal faces
        , vertices = allVertices
        , uniqueEdges = groupEdgesByDirection faces
        , position = position
        , volume = volume
        , inertia = inertia
        }


{-| Bucket every face's edges into parallel/antiparallel direction groups.
`addEdgeToGroups` dedupes the duplicate edge each face contributes. Each
group is a flat endpoint list (`[ a1, b1, a2, b2, … ]`) read two-at-a-time;
the first pair doubles as the direction representative (via `Vec3.direction`).
-}
groupEdgesByDirection : List FaceVertices -> List (List Vec3)
groupEdgesByDirection faces =
    List.foldl
        (\face groups ->
            foldFaceEdges addEdgeToGroups groups face.vertices
        )
        []
        faces


addEdgeToGroups : Vec3 -> Vec3 -> List (List Vec3) -> List (List Vec3)
addEdgeToGroups v1 v2 groups =
    let
        direction =
            Vec3.direction v1 v2
    in
    addEdgeToGroupsHelp v1 v2 direction groups []


addEdgeToGroupsHelp : Vec3 -> Vec3 -> Vec3 -> List (List Vec3) -> List (List Vec3) -> List (List Vec3)
addEdgeToGroupsHelp v1 v2 direction groups acc =
    case groups of
        [] ->
            -- New direction: prepend a fresh group to the original list.
            -- `acc` holds groups already inspected (in reverse) so we
            -- splice them back in front to preserve insertion order.
            -- Endpoints stored flipped so the single `pointsPlaceIn` reversal
            -- at place time lands them forward (no per-frame `List.reverse`).
            prependReversed acc [ [ v2, v1 ] ]

        ((gv1 :: gv2 :: _) as group) :: rest ->
            let
                groupDirection =
                    Vec3.direction gv1 gv2
            in
            -- Parallel to this group? `cross` of two unit dirs has length
            -- sin θ, so length² < tol is the parallel test. Looser than
            -- `almostZero` on the cross, which rounding noise (the icosphere's
            -- near-antipodal edges) would over-split, making `pickSupportEdge`
            -- miss the parallel edge nearest the contact.
            if Vec3.lengthSquared (Vec3.cross direction groupDirection) - Const.parallelTolerance < 0 then
                if hasEdge v1 v2 group then
                    -- Already added (face-shared): skip.
                    prependReversed acc groups

                else
                    -- Prepend with swapped endpoints: after the place-time
                    -- reversal this yields the same add-order, endpoints-intact
                    -- walk the old (firstEdge, reversed others) layout produced —
                    -- keeps `edgeIdx` and contact ids stable, and stays O(1).
                    prependReversed acc ((v2 :: v1 :: group) :: rest)

            else
                addEdgeToGroupsHelp v1 v2 direction rest (group :: acc)

        group :: rest ->
            -- malformed group (< 2 points): skip past it
            addEdgeToGroupsHelp v1 v2 direction rest (group :: acc)


hasEdge : Vec3 -> Vec3 -> List Vec3 -> Bool
hasEdge v1 v2 points =
    case points of
        e1 :: e2 :: rest ->
            if (v1 == e1 && v2 == e2) || (v1 == e2 && v2 == e1) then
                True

            else
                hasEdge v1 v2 rest

        _ ->
            False


prependReversed : List a -> List a -> List a
prependReversed src dst =
    case src of
        x :: rest ->
            prependReversed rest (x :: dst)

        [] ->
            dst


{-| Group faces by parallel/antiparallel normal direction. First insert
becomes the primary; a later match becomes the partner. For a convex
polytope at most two faces match (coplanar merge in `initFacesHelp`
collapses parallel neighbours).
-}
groupFacesByNormal : List FaceVertices -> List ( FaceVertices, Maybe FaceVertices )
groupFacesByNormal allFaces =
    groupFacesByNormalHelp allFaces []


groupFacesByNormalHelp : List FaceVertices -> List ( FaceVertices, Maybe FaceVertices ) -> List ( FaceVertices, Maybe FaceVertices )
groupFacesByNormalHelp faces acc =
    case faces of
        [] ->
            acc

        face :: rest ->
            groupFacesByNormalHelp rest (insertFaceByNormal face acc)


insertFaceByNormal : FaceVertices -> List ( FaceVertices, Maybe FaceVertices ) -> List ( FaceVertices, Maybe FaceVertices )
insertFaceByNormal face groups =
    insertFaceByNormalHelp face groups []


insertFaceByNormalHelp : FaceVertices -> List ( FaceVertices, Maybe FaceVertices ) -> List ( FaceVertices, Maybe FaceVertices ) -> List ( FaceVertices, Maybe FaceVertices )
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


initFaces : List ( Int, Int, Int ) -> Array Vec3 -> List FaceVertices
initFaces vertexIndices meshVertices =
    let
        faceByEdgeIndex =
            List.foldl
                (\(( i1, i2, i3 ) as indices) dict ->
                    case Array.get i1 meshVertices of
                        Just p1 ->
                            case Array.get i2 meshVertices of
                                Just p2 ->
                                    case Array.get i3 meshVertices of
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
            case Array.get i1 meshVertices of
                Just p1 ->
                    case Array.get i2 meshVertices of
                        Just p2 ->
                            case Array.get i3 meshVertices of
                                Just p3 ->
                                    initFacesHelp
                                        (Set.singleton indices)
                                        meshVertices
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
    -> List FaceVertices
    -> List FaceVertices
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

        vertices =
            [ v0, v1, v2, v3, v4, v5, v6, v7 ]

        faces =
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

        uniqueEdges =
            -- Three direction groups, each with four parallel edges of the
            -- cube — first edge in each group acts as the direction
            -- representative. Endpoints are picked so the edge "starts" at
            -- the vertex with the smaller axis-component along the
            -- direction (so the implicit direction is +xAxis / +yAxis /
            -- +zAxis respectively), keeping the data canonical for
            -- testing.
            [ [ v2, v3, v5, v4, v6, v7, v1, v0 ]
            , [ v2, v1, v7, v4, v6, v5, v3, v0 ]
            , [ v5, v1, v6, v2, v7, v3, v4, v0 ]
            ]
    in
    init
        { faces = faces
        , vertices = vertices
        , uniqueEdges = uniqueEdges
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

        -- All distinct vertices any face references — side faces recompute
        -- their ring points, which can differ from the caps' in the last bits
        -- at the wrap-around, so collecting from the faces (rather than reusing
        -- the caps) guarantees every face/edge endpoint is found by `==`.
        vertices =
            dedupVertices (List.concatMap .vertices allFaces) []
    in
    init
        { faces = groupFacesByNormal allFaces
        , vertices = vertices
        , uniqueEdges = groupEdgesByDirection allFaces
        , position = Vec3.zero
        , inertia = Mat3.cylinderInertia volume radius length
        , volume = volume
        }


fromCone : Int -> Float -> Float -> Convex
fromCone subdivisions radius length =
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
                    , v0 = { x = 0, y = 0, z = top }
                    , v1 = { x = sin r2 * radius, y = cos r2 * radius, z = bottom }
                    , v2 = { x = sin r0 * radius, y = cos r0 * radius, z = bottom }
                    }
                )
                (List.range 0 (subdivisions - 1))

        volume =
            (1 / 3) * pi * length * radius ^ 2

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

        sideFaces =
            List.map
                (\{ v0, v1, v2, normal } ->
                    { vertices = [ v0, v1, v2 ], normal = normal }
                )
                sides

        allFaces =
            bottomCap :: sideFaces

        -- All distinct vertices any face references — side faces recompute
        -- their ring points, which can differ from the caps' in the last bits
        -- at the wrap-around, so collecting from the faces (rather than reusing
        -- the caps) guarantees every face/edge endpoint is found by `==`.
        vertices =
            dedupVertices (List.concatMap .vertices allFaces) []
    in
    init
        { faces = groupFacesByNormal allFaces
        , vertices = vertices
        , uniqueEdges = groupEdgesByDirection allFaces
        , position = Vec3.zero
        , inertia = Mat3.coneInertia volume radius length
        , volume = volume
        }


expandBoundingSphereRadius : Convex -> Float -> Float
expandBoundingSphereRadius { vertexBuffer } boundingSphereRadius =
    sqrt
        (VertexBuffer.foldl
            (\vertex -> max (Vec3.lengthSquared vertex))
            (boundingSphereRadius * boundingSphereRadius)
            vertexBuffer
        )


raycast : { from : Vec3, direction : Vec3 } -> Convex -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { direction, from } convex =
    raycastWalk convex.vertexBuffer direction from convex.faces Nothing


raycastWalk : VertexBuffer -> Vec3 -> Vec3 -> List FaceGroup -> Maybe { distance : Float, point : Vec3, normal : Vec3 } -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycastWalk buffer direction from groups maybeHit =
    case groups of
        (OneSidedFace normal indices _ _ _ _) :: rest ->
            raycastWalk buffer direction from rest (raycastFace buffer direction from normal indices maybeHit)

        (TwoSidedFace n1 i1 _ n2 i2 _) :: rest ->
            raycastWalk buffer
                direction
                from
                rest
                (raycastFace buffer direction from n2 i2 (raycastFace buffer direction from n1 i1 maybeHit))

        [] ->
            maybeHit


raycastFace : VertexBuffer -> Vec3 -> Vec3 -> Vec3 -> List Int -> Maybe { distance : Float, point : Vec3, normal : Vec3 } -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycastFace buffer direction from normal indices maybeHit =
    let
        vertices =
            materialize buffer indices

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
foldFaceEdges : (a -> a -> b -> b) -> b -> List a -> b
foldFaceEdges fn resultSeed vertices =
    case vertices of
        first :: _ :: _ ->
            foldFaceEdgesHelp fn first resultSeed vertices

        _ ->
            -- The list is empty or contains one element.
            resultSeed


foldFaceEdgesHelp : (a -> a -> b -> b) -> a -> b -> List a -> b
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
