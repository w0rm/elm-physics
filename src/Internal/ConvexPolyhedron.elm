module Internal.ConvexPolyhedron exposing
    ( ConvexPolyhedron
    , addFaceEdges
    , clipAgainstHull
    , clipFaceAgainstHull
    , clipFaceAgainstPlane
    , expandBoundingSphereRadius
    , faceAdjacency
    , findSeparatingAxis
    , foldFaceNormals
    , foldUniqueEdges
    , fromBox
    , init
    , initUniqueEdges
    , project
    , raycast
    , sphereContact
    , testSepAxis
    )

import Array exposing (Array)
import Dict
import Internal.Const as Const
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Set


almostZero : Vec3 -> Bool
almostZero { x, y, z } =
    (abs x <= Const.precision)
        && (abs y <= Const.precision)
        && (abs z <= Const.precision)


type alias Face =
    { vertices : List Vec3
    , normal : Vec3
    , point : Vec3 -- the first point on the face
    , adjacentFaces : List { point : Vec3, normal : Vec3 }
    }


type alias ConvexPolyhedron =
    { faces : List Face
    , vertices : List Vec3 -- cached for performance
    , edges : List Vec3
    }


init : List (List Int) -> Array Vec3 -> ConvexPolyhedron
init faceVertexLists vertices =
    let
        faces =
            initFaces faceVertexLists vertices
    in
    { faces = initFaces faceVertexLists vertices
    , vertices = Array.toList vertices
    , edges = initUniqueEdges faces
    }


initFaces : List (List Int) -> Array Vec3 -> List Face
initFaces faceVertexLists convexVertices =
    let
        adjacents =
            faceAdjacency faceVertexLists

        facesWithoutAdjacentFaces =
            List.map
                (\vertexIndices ->
                    let
                        vertices =
                            List.filterMap (\i -> Array.get i convexVertices) vertexIndices

                        ( point, normal ) =
                            case vertices of
                                v1 :: v2 :: v3 :: _ ->
                                    ( v1, computeNormal v1 v2 v3 )

                                _ ->
                                    identityOrCrash "Needs at least three vertices" ( Vec3.zero, Vec3.zero )
                    in
                    { vertices = vertices
                    , normal = normal
                    , point = point
                    , adjacentFaces = []
                    }
                )
                faceVertexLists

        facesWithoutAdjacentFacesArray =
            Array.fromList facesWithoutAdjacentFaces
    in
    List.map2
        (\face adjacentIndices ->
            { face
                | adjacentFaces =
                    List.foldl
                        (\index ->
                            case Array.get index facesWithoutAdjacentFacesArray of
                                Just { point, normal } ->
                                    (::) { point = point, normal = normal }

                                Nothing ->
                                    identity
                        )
                        []
                        adjacentIndices
            }
        )
        facesWithoutAdjacentFaces
        adjacents


computeNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
computeNormal v1 v2 v3 =
    Vec3.normalize (Vec3.cross (Vec3.sub v3 v2) (Vec3.sub v1 v2))


faceAdjacency : List (List Int) -> List (List Int)
faceAdjacency faceVertexLists =
    let
        {- Like faceVertexLists, but with each vertex
           annotated with the list's face number.
        -}
        faceIndexedLists : List (List ( Int, Int ))
        faceIndexedLists =
            List.indexedMap
                (\face vertexList ->
                    List.map (\b -> ( face, b )) vertexList
                )
                faceVertexLists

        {- Invert the collections of vertices listed by face into
           a collection of faces indexed by vertex
        -}
        vertexToFacesMap : Dict.Dict Int (List Int)
        vertexToFacesMap =
            List.foldl
                (\indexedList acc ->
                    List.foldl
                        (\( face, vertex ) acc1 ->
                            Dict.insert
                                vertex
                                (face
                                    :: (case Dict.get vertex acc1 of
                                            Nothing ->
                                                []

                                            Just existingList ->
                                                existingList
                                       )
                                )
                                acc1
                        )
                        acc
                        indexedList
                )
                Dict.empty
                faceIndexedLists

        {- Merge each listed vertex's containing faces into a set,
           excluding the self-references to the current face.
        -}
        addUniqueContainingFaces : ( Int, Int ) -> Set.Set Int -> Set.Set Int
        addUniqueContainingFaces ( face, vertex ) acc =
            Dict.get vertex vertexToFacesMap
                |> Maybe.withDefault []
                |> List.foldl Set.insert acc
                |> Set.remove face
    in
    List.map
        (List.foldl
            addUniqueContainingFaces
            Set.empty
            >> Set.toList
        )
        faceIndexedLists


fromBox : Vec3 -> ConvexPolyhedron
fromBox { x, y, z } =
    let
        v0 =
            vec3 -x -y -z

        v1 =
            vec3 x -y -z

        v2 =
            vec3 x y -z

        v3 =
            vec3 -x y -z

        v4 =
            vec3 -x -y z

        v5 =
            vec3 x -y z

        v6 =
            vec3 x y z

        v7 =
            vec3 -x y z

        p0 =
            v3

        n0 =
            vec3 0 0 -1

        p1 =
            v4

        n1 =
            vec3 0 0 1

        p2 =
            v5

        n2 =
            vec3 0 -1 0

        p3 =
            v2

        n3 =
            vec3 0 1 0

        p4 =
            v0

        n4 =
            vec3 -1 0 0

        p5 =
            v1

        n5 =
            vec3 1 0 0
    in
    { faces =
        [ -- face 0
          { vertices = [ v3, v2, v1, v0 ]
          , point = p0
          , normal = n0
          , adjacentFaces = [ { point = p2, normal = n2 }, { point = p3, normal = n3 }, { point = p4, normal = n4 }, { point = p5, normal = n5 } ]
          }

        -- face 1
        , { vertices = [ v4, v5, v6, v7 ]
          , point = p1
          , normal = n1
          , adjacentFaces = [ { point = p2, normal = n2 }, { point = p3, normal = n3 }, { point = p4, normal = n4 }, { point = p5, normal = n5 } ]
          }

        -- face 2
        , { vertices = [ v5, v4, v0, v1 ]
          , point = p2
          , normal = n2
          , adjacentFaces = [ { point = p0, normal = n0 }, { point = p1, normal = n1 }, { point = p4, normal = n4 }, { point = p5, normal = n5 } ]
          }

        -- face 3
        , { vertices = [ v2, v3, v7, v6 ]
          , point = p3
          , normal = n3
          , adjacentFaces = [ { point = p0, normal = n0 }, { point = p1, normal = n1 }, { point = p4, normal = n4 }, { point = p5, normal = n5 } ]
          }

        -- face 4
        , { vertices = [ v0, v4, v7, v3 ]
          , point = p4
          , normal = n4
          , adjacentFaces = [ { point = p0, normal = n0 }, { point = p1, normal = n1 }, { point = p2, normal = n2 }, { point = p3, normal = n3 } ]
          }

        -- face 5
        , { vertices = [ v1, v2, v6, v5 ]
          , point = p5
          , normal = n5
          , adjacentFaces = [ { point = p0, normal = n0 }, { point = p1, normal = n1 }, { point = p2, normal = n2 }, { point = p3, normal = n3 } ]
          }
        ]
    , vertices = [ v0, v1, v2, v3, v4, v5, v6, v7 ]
    , edges = [ Vec3.i, Vec3.j, Vec3.k ]
    }


initUniqueEdges : List Face -> List Vec3
initUniqueEdges faces =
    List.foldl addFaceEdges [] faces


addFaceEdges : Face -> List Vec3 -> List Vec3
addFaceEdges { vertices } edges =
    listRingFoldStaggeredPairs
        addEdgeIfDistinct
        edges
        vertices


{-| Add a candidate edge between two vertices to a set if it is not a
near duplicate or near opposite of an edge already in the set.
-}
addEdgeIfDistinct : Vec3 -> Vec3 -> List Vec3 -> List Vec3
addEdgeIfDistinct prevVertex currentVertex uniques =
    let
        candidateEdge =
            Vec3.direction prevVertex currentVertex

        alreadyInTheSet =
            List.any
                (\member ->
                    (Vec3.sub member candidateEdge |> almostZero)
                        || (Vec3.add member candidateEdge |> almostZero)
                )
                uniques
    in
    if alreadyInTheSet then
        uniques

    else
        candidateEdge :: uniques


type alias ClipResult =
    { point : Vec3
    , normal : Vec3
    , depth : Float
    }


clipAgainstHull : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Vec3 -> Float -> Float -> List ClipResult
clipAgainstHull t1 hull1 t2 hull2 separatingNormal minDist maxDist =
    case bestFace Farthest t2 hull2.faces separatingNormal of
        Just { vertices } ->
            clipFaceAgainstHull
                t1
                hull1
                separatingNormal
                (List.map (Transform.pointToWorldFrame t2) vertices)
                minDist
                maxDist

        Nothing ->
            []


clipFaceAgainstHull : Transform -> ConvexPolyhedron -> Vec3 -> List Vec3 -> Float -> Float -> List ClipResult
clipFaceAgainstHull t1 hull1 separatingNormal worldVertsB minDist maxDist =
    case bestFace Nearest t1 hull1.faces separatingNormal of
        Just { vertices, point, normal, adjacentFaces } ->
            let
                localPlaneEq =
                    -(Vec3.dot normal point)

                planeNormalWS =
                    Quaternion.rotate t1.orientation normal

                planeEqWS =
                    localPlaneEq - Vec3.dot planeNormalWS t1.position
            in
            adjacentFaces
                |> List.foldl
                    (\otherFace ->
                        let
                            localPlaneEq_ =
                                -(Vec3.dot otherFace.point otherFace.normal)

                            planeNormalWS_ =
                                Quaternion.rotate t1.orientation otherFace.normal

                            planeEqWS_ =
                                localPlaneEq_ - Vec3.dot planeNormalWS_ t1.position
                        in
                        clipFaceAgainstPlane planeNormalWS_ planeEqWS_
                    )
                    worldVertsB
                |> List.foldl
                    (\vertex result ->
                        let
                            depth =
                                max minDist (Vec3.dot planeNormalWS vertex + planeEqWS)
                        in
                        if depth <= maxDist && depth <= 0 then
                            { point = vertex
                            , normal = planeNormalWS
                            , depth = depth
                            }
                                :: result

                        else
                            result
                    )
                    []

        Nothing ->
            []


{-| Encapsulate a compatible comparison operator and a worst case value
according to that operator such that for any DistanceCriterion dc:
(compareOp dc) \_ (worstValue dc) == True
(compareOp dc) (worstValue dc) \_ == False
DistanceCriterion prevents accidental combination of comparators with
inappropriate worst case values such as when starting an iterative run-off.
It should be easier and less error prone to write and use generic run-off
folds dependent on DistanceCriterion.
-}
type DistanceCriterion
    = Nearest
    | Farthest


compareOp : DistanceCriterion -> (Float -> Float -> Bool)
compareOp dc =
    case dc of
        Nearest ->
            (<)

        Farthest ->
            (>)


worstValue : DistanceCriterion -> Float
worstValue dc =
    case dc of
        Nearest ->
            Const.maxNumber

        Farthest ->
            -Const.maxNumber


bestFace : DistanceCriterion -> Transform -> List Face -> Vec3 -> Maybe Face
bestFace comparator transform faces separatingNormal =
    let
        worstDistance =
            worstValue comparator

        compareFunc =
            compareOp comparator
    in
    faces
        |> List.foldl
            (\face (( _, bestDistance ) as bestPair) ->
                let
                    faceDistance =
                        face.normal
                            |> Quaternion.rotate transform.orientation
                            |> Vec3.dot separatingNormal
                in
                if compareFunc faceDistance bestDistance then
                    ( Just face, faceDistance )

                else
                    bestPair
            )
            ( Nothing, worstDistance )
        |> Tuple.first


clipFaceAgainstPlane : Vec3 -> Float -> List Vec3 -> List Vec3
clipFaceAgainstPlane planeNormal planeConstant vertices =
    listRingFoldStaggeredPairs
        (clipFaceAgainstPlaneAdd planeNormal planeConstant)
        []
        vertices


clipFaceAgainstPlaneAdd : Vec3 -> Float -> Vec3 -> Vec3 -> List Vec3 -> List Vec3
clipFaceAgainstPlaneAdd planeNormal planeConstant prev next result =
    let
        nDotPrev =
            Vec3.dot planeNormal prev + planeConstant

        nDotNext =
            Vec3.dot planeNormal next + planeConstant
    in
    if nDotPrev < 0 then
        if nDotNext < 0 then
            next :: result

        else
            Vec3.lerp (nDotPrev / (nDotPrev - nDotNext)) prev next
                :: result

    else if nDotNext < 0 then
        next
            :: Vec3.lerp (nDotPrev / (nDotPrev - nDotNext)) prev next
            :: result

    else
        result


findSeparatingAxis : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Maybe Vec3
findSeparatingAxis t1 hull1 t2 hull2 =
    let
        ctx =
            TestContext t1 hull1 t2 hull2
    in
    testFaceNormals ctx
        t1.orientation
        hull1.faces
        (testFaceNormals ctx
            t2.orientation
            hull2.faces
            (let
                worldEdges1 =
                    List.foldl (Quaternion.rotate ctx.t1.orientation >> (::)) [] ctx.hull1.edges

                worldEdges2 =
                    List.foldl (Quaternion.rotate ctx.t2.orientation >> (::)) [] ctx.hull2.edges
             in
             testEdges ctx worldEdges2 worldEdges1 worldEdges2
            )
        )
        Vec3.zero
        Const.maxNumber


type alias TestContext =
    { t1 : Transform
    , hull1 : ConvexPolyhedron
    , t2 : Transform
    , hull2 : ConvexPolyhedron
    }


testFaceNormals :
    TestContext
    -> Quaternion
    -> List Face
    -> (Vec3 -> Float -> Maybe Vec3)
    -> Vec3
    -> Float
    -> Maybe Vec3
testFaceNormals ctx quat faces fn target dmin =
    case faces of
        [] ->
            fn target dmin

        { normal } :: restFaces ->
            let
                rotatedNormal =
                    Quaternion.rotate quat normal
            in
            case testSepAxis ctx rotatedNormal of
                Nothing ->
                    Nothing

                Just d ->
                    if d - dmin < 0 then
                        testFaceNormals ctx quat restFaces fn rotatedNormal d

                    else
                        testFaceNormals ctx quat restFaces fn target dmin


testEdges : TestContext -> List Vec3 -> List Vec3 -> List Vec3 -> Vec3 -> Float -> Maybe Vec3
testEdges ctx initEdges2 edges1 edges2 target dmin =
    case edges1 of
        [] ->
            if Vec3.dot (Vec3.sub ctx.t2.position ctx.t1.position) target > 0 then
                Just (Vec3.negate target)

            else
                Just target

        worldEdge1 :: remainingEdges1 ->
            case edges2 of
                [] ->
                    -- requeue edges2
                    testEdges ctx initEdges2 remainingEdges1 initEdges2 target dmin

                worldEdge2 :: remainingEdges2 ->
                    let
                        cross =
                            Vec3.cross worldEdge1 worldEdge2
                    in
                    if almostZero cross then
                        -- continue because edges are parallel
                        testEdges ctx initEdges2 edges1 remainingEdges2 target dmin

                    else
                        let
                            normalizedCross =
                                Vec3.normalize cross
                        in
                        case testSepAxis ctx normalizedCross of
                            Nothing ->
                                -- exit because hulls don't collide
                                Nothing

                            Just dist ->
                                if dist - dmin < 0 then
                                    -- update target and dmin
                                    testEdges ctx initEdges2 edges1 remainingEdges2 normalizedCross dist

                                else
                                    -- continue
                                    testEdges ctx initEdges2 edges1 remainingEdges2 target dmin


testSepAxis : TestContext -> Vec3 -> Maybe Float
testSepAxis { t1, hull1, t2, hull2 } axis =
    let
        ( max1, min1 ) =
            project t1 hull1 axis

        ( max2, min2 ) =
            project t2 hull2 axis
    in
    if max1 - min2 < 0 || max2 - min1 < 0 then
        Nothing

    else
        Just (min (max1 - min2) (max2 - min1))


{-| Get max and min dot product of a convex hull at Transform projected onto an axis.
-}
project : Transform -> ConvexPolyhedron -> Vec3 -> ( Float, Float )
project transform { vertices } axis =
    let
        -- TODO: consider inlining all the operations here, this is a very HOT path
        localAxis =
            Transform.vectorToLocalFrame transform axis

        add =
            Vec3.zero
                |> Transform.pointToLocalFrame transform
                |> Vec3.dot localAxis

        ( maxVal, minVal ) =
            projectHelp localAxis -Const.maxNumber Const.maxNumber vertices
    in
    ( maxVal - add, minVal - add )


projectHelp : Vec3 -> Float -> Float -> List Vec3 -> ( Float, Float )
projectHelp localAxis maxVal minVal currentVertices =
    case currentVertices of
        [] ->
            ( maxVal, minVal )

        vec :: remainingVertices ->
            let
                {- val =
                   Vec3.dot vec localAxis
                -}
                val =
                    vec.x * localAxis.x + vec.y * localAxis.y + vec.z * localAxis.z
            in
            projectHelp
                localAxis
                (max maxVal val)
                (min minVal val)
                remainingVertices


max : Float -> Float -> Float
max a b =
    if a - b > 0 then
        a

    else
        b


min : Float -> Float -> Float
min a b =
    if b - a > 0 then
        a

    else
        b


{-| Encapsulated result of sphereTestFace
-}
type TestFaceResult
    = QualifiedEdges (List (List ( Vec3, Vec3 )))
    | FaceContact Vec3 Float


isAFaceContact : TestFaceResult -> Bool
isAFaceContact testFaceResult =
    case testFaceResult of
        FaceContact _ _ ->
            True

        _ ->
            False


type TestBoundaryResult
    = PossibleVertexContact ( Maybe Vec3, Float )
    | EdgeContact ( Vec3, Float )


isAnEdgeContact : TestBoundaryResult -> Bool
isAnEdgeContact testEdgeResult =
    case testEdgeResult of
        EdgeContact _ ->
            True

        _ ->
            False


{-| The contact point, if any, of a ConvexPolyhedron with a sphere, and
the sphere's penetration into the ConvexPolyhedron beyond that contact.
-}
sphereContact : Vec3 -> Float -> Transform -> ConvexPolyhedron -> ( Maybe Vec3, Float )
sphereContact center radius t2 { vertices, faces } =
    let
        sphereFaceContact : Vec3 -> Float -> ( Maybe Vec3, Float )
        sphereFaceContact normal distance =
            -- The world frame contact is located distance away from
            -- the world frame sphere center in the OPPOSITE direction of
            -- the normal.
            ( Just (Vec3.sub center (Vec3.scale distance normal))
            , radius - distance
            )

        sphereBoundaryContact : Vec3 -> Float -> ( Maybe Vec3, Float )
        sphereBoundaryContact localContact distanceSq =
            ( Just (Vec3.add localContact center)
            , radius - sqrt distanceSq
            )

        spherePossibleBoundaryContact : List (List ( Vec3, Vec3 )) -> ( Maybe Vec3, Float )
        spherePossibleBoundaryContact faceEdgeList =
            case sphereTestBoundaries radius faceEdgeList of
                PossibleVertexContact ( Just localContact, distanceSq ) ->
                    sphereBoundaryContact localContact distanceSq

                PossibleVertexContact noContact ->
                    noContact

                EdgeContact ( localContact, distanceSq ) ->
                    sphereBoundaryContact localContact distanceSq

        reframedVertices faceVertices =
            List.foldl
                (\vertex acc ->
                    Vec3.sub (Transform.pointToWorldFrame t2 vertex) center :: acc
                )
                []
                faceVertices

        -- Find the details of the closest faces.
        testFaceResult =
            listRecurseUntil
                isAFaceContact
                (\face statusQuo ->
                    case statusQuo of
                        QualifiedEdges acc ->
                            sphereTestFace
                                radius
                                (Quaternion.rotate t2.orientation face.normal)
                                (reframedVertices face.vertices)
                                acc

                        FaceContact _ _ ->
                            -- Since a FaceContact short circuits the
                            -- recursion, this case is not expected.
                            statusQuo
                )
                (QualifiedEdges [])
                faces
    in
    case testFaceResult of
        QualifiedEdges faceEdgeList ->
            -- Check the candidate faces' edges and vertices.
            spherePossibleBoundaryContact faceEdgeList

        FaceContact faceNormal faceDistance ->
            sphereFaceContact faceNormal faceDistance


{-| The contact point and distance, if any, of a ConvexPolyhedron's face
with a sphere, or otherwise a list of the face's edges that may contain an
edge or vertex contact.
-}
sphereTestFace : Float -> Vec3 -> List Vec3 -> List (List ( Vec3, Vec3 )) -> TestFaceResult
sphereTestFace radius normal vertices acc =
    let
        -- Use an arbitrary vertex from the face to measure the distance to
        -- the origin (sphere center) along the face normal.
        faceDistance =
            case vertices of
                point :: _ ->
                    -(Vec3.dot normal point)

                [] ->
                    -- a negative value prevents a face or edge contact match
                    -1
    in
    if faceDistance - radius < 0 && faceDistance > 0.0 then
        -- Sphere intersects the face plane.
        -- Assume 3 or more valid vertices to proceed
        -- Check if the sphere center projects onto the face plane INSIDE the face polygon.
        case originProjection vertices normal of
            [] ->
                -- The projection falls within all the face's edges.
                FaceContact normal faceDistance

            separatingEdges ->
                -- These origin-excluding edges are candidates for
                -- having an edge or vertex contact.
                QualifiedEdges (separatingEdges :: acc)

    else
        QualifiedEdges acc


{-| The edge or vertex contact point and its distance (squared), if any,
of a ConvexPolyhedron's edges with a sphere, limited to a pre-qualified
list of edges per face.
-}
sphereTestBoundaries : Float -> List (List ( Vec3, Vec3 )) -> TestBoundaryResult
sphereTestBoundaries radius faceEdgeList =
    List.foldl
        sphereTestBoundary
        (PossibleVertexContact ( Nothing, radius * radius ))
        faceEdgeList


{-| The edge or possible vertex contact point and its distance (squared),
if any, of a ConvexPolyhedron face's pre-qualified edges with a sphere.
-}
sphereTestBoundary : List ( Vec3, Vec3 ) -> TestBoundaryResult -> TestBoundaryResult
sphereTestBoundary faceEdges statusQuo =
    listRecurseUntil
        isAnEdgeContact
        (\( prevVertex, vertex ) statusQuo1 ->
            case statusQuo1 of
                PossibleVertexContact soFar ->
                    sphereTestEdge prevVertex vertex soFar

                EdgeContact _ ->
                    -- Since an EdgeContact stops the recursion,
                    -- this case is not expected.
                    statusQuo1
        )
        statusQuo
        faceEdges


{-| The edge or possible vertex contact point and its distance (squared),
if any, of a ConvexPolyhedron face's pre-qualified edge with a sphere.
-}
sphereTestEdge : Vec3 -> Vec3 -> ( Maybe Vec3, Float ) -> TestBoundaryResult
sphereTestEdge prevVertex vertex (( _, minDistanceSq ) as statusQuo) =
    let
        betterVertexContact : Vec3 -> ( Maybe Vec3, Float )
        betterVertexContact candidate =
            let
                -- Note: the vector length of a sphere-framed vertex
                -- is its distance from the sphere center
                vertexLengthSq =
                    Vec3.lengthSquared candidate
            in
            if vertexLengthSq - minDistanceSq < 0 then
                ( Just candidate, vertexLengthSq )

            else
                statusQuo

        edge =
            Vec3.sub vertex prevVertex

        edgeUnit =
            Vec3.normalize edge

        -- The potential contact is where the sphere center
        -- projects onto the edge.
        -- offset is the directed distance between the edge's
        -- starting vertex and that projection. If it is not
        -- between 0 and the edge's length, there is no edge contact.
        -- Yet there may be a contact with whichever vertex is closest
        -- to the projection.
        offset =
            -(Vec3.dot prevVertex edgeUnit)
    in
    if offset < 0 then
        -- prevVertex is closest in this edge,
        -- but there may be a closer edge or
        -- no contact.
        PossibleVertexContact (betterVertexContact prevVertex)

    else if offset * offset - Vec3.lengthSquared edge > 0 then
        -- vertex is closest in this edge,
        -- but there may be a closer edge or
        -- no contact.
        PossibleVertexContact (betterVertexContact vertex)

    else
        let
            edgeContact =
                Vec3.add prevVertex (Vec3.scale offset edgeUnit)

            edgeDistanceSq =
                Vec3.lengthSquared edgeContact
        in
        if edgeDistanceSq - minDistanceSq < 0 then
            EdgeContact ( edgeContact, edgeDistanceSq )

        else
            PossibleVertexContact statusQuo


{-| A 2D point-in-polygon check for the projection of the origin
(e.g. the center of a sphere within its own frame of reference) within a
polygon (e.g. a ConvexPolyhedron face). To simplify post-processing,
return a relatively short but complete list of qualified edges (adjacent
vertex pairs) whose lines separate the projection from the polygon.
If the list is empty, the projection is within the polygon.
-}
originProjection : List Vec3 -> Vec3 -> List ( Vec3, Vec3 )
originProjection vertices normal =
    listRingFoldStaggeredPairs
        (\prevVertex vertex acc ->
            let
                edge_x_normal =
                    Vec3.cross normal (Vec3.sub vertex prevVertex)
            in
            -- The sign of this dot product determines on which
            -- side of the directed edge the projected point lies,
            -- left or right, within the face plane.
            -- For the projection to be within the face, the sign
            -- must always be non-negative when circling from vertex
            -- to vertex in the listed (counter-clockwise) direction.
            -- Retain any edge that tests negative as a candidate
            -- for an edge or vertex contact.
            if Vec3.dot edge_x_normal prevVertex < 0 then
                ( prevVertex, vertex ) :: acc

            else
                acc
        )
        []
        vertices


foldFaceNormals : (Vec3 -> Vec3 -> a -> a) -> a -> ConvexPolyhedron -> a
foldFaceNormals fn acc { faces } =
    List.foldl
        (\{ vertices, normal } acc1 ->
            let
                vsum =
                    List.foldl Vec3.add Vec3.zero vertices

                vcount =
                    List.length vertices
            in
            fn normal (Vec3.scale (1.0 / toFloat vcount) vsum) acc1
        )
        acc
        faces


foldUniqueEdges : (Vec3 -> Vec3 -> a -> a) -> a -> ConvexPolyhedron -> a
foldUniqueEdges fn acc { vertices, edges } =
    case vertices of
        vertex0 :: _ ->
            List.foldl (fn vertex0) acc edges

        [] ->
            acc


expandBoundingSphereRadius : Transform -> ConvexPolyhedron -> Float -> Float
expandBoundingSphereRadius shapeTransform { vertices } boundingSphereRadius =
    vertices
        |> List.foldl
            (\vertex ->
                vertex
                    |> Transform.pointToWorldFrame shapeTransform
                    |> Vec3.lengthSquared
                    |> max
            )
            (boundingSphereRadius * boundingSphereRadius)
        |> sqrt


raycast : { from : Vec3, direction : Vec3 } -> Transform -> ConvexPolyhedron -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { direction, from } transform convex =
    List.foldl
        (\face maybeHit ->
            let
                faceNormalWS =
                    Quaternion.rotate transform.orientation face.normal

                dot =
                    Vec3.dot direction faceNormalWS
            in
            if dot < 0 then
                let
                    pointOnFaceWS =
                        Transform.pointToWorldFrame transform face.point

                    pointToFrom =
                        Vec3.sub pointOnFaceWS from

                    scalar =
                        Vec3.dot faceNormalWS pointToFrom / dot
                in
                if scalar >= 0 then
                    let
                        intersectionPoint =
                            { x = direction.x * scalar + from.x
                            , y = direction.y * scalar + from.y
                            , z = direction.z * scalar + from.z
                            }

                        isInsidePolygon =
                            face.vertices
                                |> List.foldl
                                    (\p acc1 ->
                                        Transform.pointToWorldFrame transform p :: acc1
                                    )
                                    []
                                |> listRingFoldStaggeredPairs
                                    (\p1 p2 result ->
                                        result
                                            && (Vec3.dot
                                                    (Vec3.sub intersectionPoint p1)
                                                    (Vec3.sub p2 p1)
                                                    > 0
                                               )
                                    )
                                    True
                    in
                    if isInsidePolygon then
                        case maybeHit of
                            Just { distance } ->
                                if scalar - distance < 0 then
                                    Just
                                        { distance = scalar
                                        , point = intersectionPoint
                                        , normal = faceNormalWS
                                        }

                                else
                                    maybeHit

                            Nothing ->
                                Just
                                    { distance = scalar
                                    , point = intersectionPoint
                                    , normal = faceNormalWS
                                    }

                    else
                        maybeHit

                else
                    maybeHit

            else
                maybeHit
        )
        Nothing
        convex.faces



-- Generic utilities, listed alphabetically.
-- TODO: Consider migrating these to one or more utility modules
-- if they are found useful elsewhere.


{-| Easily disabled wrapper for Debug.crash.
KEEP DISABLED in published production code.
-}
identityOrCrash : String -> a -> a
identityOrCrash message value =
    -- enabled: Debug.crash message value
    -- disabled: KEEP DISABLED in published production code.
    value


{-| Fold the function over pairs of consecutive elements in the list,
starting with the pair (first, second), and so on until (last, seed).
-}
listFoldStaggeredPairs : (a -> a -> b -> b) -> a -> b -> List a -> b
listFoldStaggeredPairs fn seed resultSeed list =
    case list of
        el1 :: rest1 ->
            case rest1 of
                [] ->
                    fn el1 seed resultSeed

                el2 :: rest2 ->
                    listFoldStaggeredPairs
                        fn
                        seed
                        (fn el1 el2 resultSeed)
                        rest1

        [] ->
            resultSeed


{-| Recursively "foldl" the function over the elements of the list,
until the result passes a test. Using recursion in the place of a true
fold allows a short-circuit return as soon as the test passes.
Note: If the short-circuit condition is unlikely, especially towards
the beginning of the list, it MAY be more efficient to use foldl,
integrating the short-circuit test into the folding function as an up-front
pass-through condition.
-}
listRecurseUntil : (b -> Bool) -> (a -> b -> b) -> b -> List a -> b
listRecurseUntil test fn resultSoFar list =
    if test resultSoFar then
        resultSoFar

    else
        case list of
            head :: tail ->
                let
                    acc =
                        fn head resultSoFar
                in
                listRecurseUntil test fn acc tail

            _ ->
                resultSoFar


{-| Map the function to pairs of consecutive elements in the ring list,
starting with the pair (first, second), and so on, until (last, first).
-}
listRingFoldStaggeredPairs : (a -> a -> b -> b) -> b -> List a -> b
listRingFoldStaggeredPairs fn resultSeed list =
    case list of
        first :: _ :: rest ->
            listFoldStaggeredPairs fn first resultSeed list

        _ ->
            -- The list is empty or contains one element.
            resultSeed


{-| Crash-on-Nothing equivalent of Maybe.withDefault for use in debugging.
KEEP DISABLED in published production code.
-}
maybeWithDefaultOrCrash : String -> a -> Maybe a -> a
maybeWithDefaultOrCrash message default maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            --Debug.crash message
            default
