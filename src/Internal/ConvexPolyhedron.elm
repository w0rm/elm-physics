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
    , initFaceNormal
    , initUniqueEdges
    , project
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
    { vertexIndices : List Int
    , normal : Vec3
    , adjacentFaces : List Int
    }


type alias ConvexPolyhedron =
    { faces : Array Face
    , vertices : Array Vec3
    , edges : List Vec3
    }


init : List (List Int) -> Array Vec3 -> ConvexPolyhedron
init faceVertexLists vertices =
    { faces = initFaces faceVertexLists vertices
    , vertices = vertices
    , edges = initUniqueEdges faceVertexLists vertices
    }


initFaces : List (List Int) -> Array Vec3 -> Array Face
initFaces faceVertexLists vertices =
    let
        adjacents =
            faceAdjacency faceVertexLists
    in
    List.map2
        (\vertexIndices adjacentFaces ->
            { vertexIndices = vertexIndices
            , normal = initFaceNormal vertexIndices vertices
            , adjacentFaces = adjacentFaces
            }
        )
        faceVertexLists
        adjacents
        |> Array.fromList


faceAdjacency : List (List Int) -> List (List Int)
faceAdjacency faceVertexLists =
    let
        {- Like faceVertexLists, but with each vertex
           annotated with the list's face number.
        -}
        faceIndexedLists : List (List ( Int, Int ))
        faceIndexedLists =
            faceVertexLists
                |> List.indexedMap
                    (\face vertexList ->
                        vertexList
                            |> List.map (\b -> ( face, b ))
                    )

        {- Invert the collections of vertices listed by face into
           a collection of faces indexed by vertex
        -}
        vertexToFacesMap : Dict.Dict Int (List Int)
        vertexToFacesMap =
            faceIndexedLists
                |> List.foldl
                    (\indexedList acc ->
                        indexedList
                            |> List.foldl
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
                    )
                    Dict.empty

        {- Merge each listed vertex's containing faces into a set,
           excluding the self-references to the current face.
        -}
        addUniqueContainingFaces : ( Int, Int ) -> Set.Set Int -> Set.Set Int
        addUniqueContainingFaces ( face, vertex ) acc =
            Dict.get vertex vertexToFacesMap
                |> Maybe.withDefault []
                |> List.foldl
                    Set.insert
                    acc
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
    { faces = boxFaces
    , vertices =
        Array.fromList
            [ vec3 -x -y -z
            , vec3 x -y -z
            , vec3 x y -z
            , vec3 -x y -z
            , vec3 -x -y z
            , vec3 x -y z
            , vec3 x y z
            , vec3 -x y z
            ]
    , edges = boxUniqueEdges
    }


boxFaces : Array Face
boxFaces =
    Array.fromList
        [ { vertexIndices = [ 3, 2, 1, 0 ], normal = vec3 0 0 -1, adjacentFaces = [ 2, 3, 4, 5 ] }
        , { vertexIndices = [ 4, 5, 6, 7 ], normal = vec3 0 0 1, adjacentFaces = [ 2, 3, 4, 5 ] }
        , { vertexIndices = [ 5, 4, 0, 1 ], normal = vec3 0 -1 0, adjacentFaces = [ 0, 1, 4, 5 ] }
        , { vertexIndices = [ 2, 3, 7, 6 ], normal = vec3 0 1 0, adjacentFaces = [ 0, 1, 4, 5 ] }
        , { vertexIndices = [ 0, 4, 7, 3 ], normal = vec3 -1 0 0, adjacentFaces = [ 0, 1, 2, 3 ] }
        , { vertexIndices = [ 1, 2, 6, 5 ], normal = vec3 1 0 0, adjacentFaces = [ 0, 1, 2, 3 ] }
        ]


boxUniqueEdges : List Vec3
boxUniqueEdges =
    [ Vec3.i
    , Vec3.j
    , Vec3.k
    ]


initFaceNormal : List Int -> Array Vec3 -> Vec3
initFaceNormal indices vertices =
    case indices of
        i1 :: i2 :: i3 :: _ ->
            Maybe.map3 computeNormal
                (Array.get i1 vertices)
                (Array.get i2 vertices)
                (Array.get i3 vertices)
                |> maybeWithDefaultOrCrash
                    "Couldn't compute normal with invalid vertex index"
                    Const.zero3

        _ ->
            identityOrCrash
                "Couldn't compute normal with < 3 vertices"
                Const.zero3


initUniqueEdges : List (List Int) -> Array Vec3 -> List Vec3
initUniqueEdges faceVertexLists vertices =
    faceVertexLists
        |> List.foldl (addFaceEdges vertices) []


addFaceEdges : Array Vec3 -> List Int -> List Vec3 -> List Vec3
addFaceEdges vertices vertexIndices edges =
    vertexIndices
        |> listRingFoldStaggeredPairs
            (\prev current acc ->
                addEdgeIfDistinct
                    (Array.get prev vertices)
                    (Array.get current vertices)
                    acc
            )
            edges


{-| Add a candidate edge between two vertices to a set if it is not a
near duplicate or near opposite of an edge already in the set.
-}
addEdgeIfDistinct : Maybe Vec3 -> Maybe Vec3 -> List Vec3 -> List Vec3
addEdgeIfDistinct prevVertex currentVertex uniques =
    let
        candidateEdge =
            Maybe.map2
                Vec3.direction
                prevVertex
                currentVertex
    in
    uniques
        |> List.foldl
            (\member candidate ->
                candidate
                    |> Maybe.andThen (distinctOrNothing member)
            )
            candidateEdge
        |> listMaybeAdd uniques


{-| Eliminate a candidate that is a near duplicate or near
opposite of an edge that is already a member of a set.
-}
distinctOrNothing : Vec3 -> Vec3 -> Maybe Vec3
distinctOrNothing member candidate =
    if
        (Vec3.sub member candidate |> almostZero)
            || (Vec3.add member candidate |> almostZero)
    then
        Nothing

    else
        Just candidate


computeNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
computeNormal v0 v1 v2 =
    Vec3.cross (Vec3.sub v2 v1) (Vec3.sub v0 v1)
        |> Vec3.normalize


type alias ClipResult =
    { point : Vec3
    , normal : Vec3
    , depth : Float
    }


clipAgainstHull : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Vec3 -> Float -> Float -> List ClipResult
clipAgainstHull t1 hull1 t2 hull2 separatingNormal minDist maxDist =
    case bestFace Farthest t2 hull2.faces separatingNormal of
        Just { vertexIndices } ->
            clipFaceAgainstHull
                t1
                hull1
                separatingNormal
                (vertexIndices
                    |> List.map
                        (getIndexedVertex hull2.vertices
                            >> Transform.pointToWorldFrame t2
                        )
                )
                minDist
                maxDist

        Nothing ->
            []


getIndexedVertex : Array Vec3 -> Int -> Vec3
getIndexedVertex vertices i =
    Array.get i vertices
        |> Maybe.withDefault Const.zero3


getIndexedFace : Array Face -> Int -> Face
getIndexedFace faces i =
    Array.get i faces
        -- This default should never get triggered in production.
        -- It is type-correct but intentionally invalid for most purposes
        -- in the hopes that it is detected and handled ASAP downstream.
        |> Maybe.withDefault
            { vertexIndices = []
            , normal = Const.zero3
            , adjacentFaces = []
            }


clipFaceAgainstHull : Transform -> ConvexPolyhedron -> Vec3 -> List Vec3 -> Float -> Float -> List ClipResult
clipFaceAgainstHull t1 hull1 separatingNormal worldVertsB minDist maxDist =
    case bestFace Nearest t1 hull1.faces separatingNormal of
        Just { vertexIndices, normal, adjacentFaces } ->
            let
                localPlaneEq =
                    -(List.head vertexIndices
                        |> maybeWithDefaultOrCrash
                            "empty face vertexIndices"
                            -1
                        |> getIndexedVertex hull1.vertices
                        |> Vec3.dot normal
                     )

                planeNormalWS =
                    Quaternion.rotate t1.orientation normal

                planeEqWS =
                    localPlaneEq - Vec3.dot planeNormalWS t1.position

                otherFaceDetails : Int -> ( Vec3, Vec3 )
                otherFaceDetails otherFaceIndex =
                    getIndexedFace hull1.faces otherFaceIndex
                        |> (\otherFace ->
                                List.head otherFace.vertexIndices
                                    |> maybeWithDefaultOrCrash
                                        "vertexIndices is empty for face"
                                        -1
                                    |> getIndexedVertex hull1.vertices
                                    |> (\b -> ( otherFace.normal, b ))
                           )
            in
            adjacentFaces
                |> List.foldl
                    (\otherFaceIndex ->
                        let
                            ( otherFaceNormal, otherFaceVertex ) =
                                otherFaceDetails otherFaceIndex

                            localPlaneEq_ =
                                -(Vec3.dot otherFaceVertex otherFaceNormal)

                            planeNormalWS_ =
                                Quaternion.rotate t1.orientation otherFaceNormal

                            planeEqWS_ =
                                localPlaneEq_ - Vec3.dot planeNormalWS_ t1.position
                        in
                        clipFaceAgainstPlane planeNormalWS_ planeEqWS_
                    )
                    worldVertsB
                |> List.foldl
                    (\point result ->
                        let
                            depth =
                                max minDist (Vec3.dot planeNormalWS point + planeEqWS)
                        in
                        if depth <= maxDist && depth <= 0 then
                            { point = point
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


bestFace : DistanceCriterion -> Transform -> Array Face -> Vec3 -> Maybe Face
bestFace comparator transform faces separatingNormal =
    let
        worstDistance =
            worstValue comparator

        compareFunc =
            compareOp comparator
    in
    faces
        |> Array.foldl
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
    case vertices of
        -- guarantee at least two, keep the first to match with the last
        fst :: snd :: rest ->
            clipFaceAgainstPlaneHelp planeNormal planeConstant fst vertices []

        _ ->
            []


clipFaceAgainstPlaneHelp : Vec3 -> Float -> Vec3 -> List Vec3 -> List Vec3 -> List Vec3
clipFaceAgainstPlaneHelp planeNormal planeConstant first vertices result =
    case vertices of
        [] ->
            result

        fst :: snd :: remaining ->
            clipFaceAgainstPlaneHelp
                planeNormal
                planeConstant
                first
                (snd :: remaining)
                (clipFaceAgainstPlaneAdd planeNormal planeConstant fst snd result)

        last :: [] ->
            clipFaceAgainstPlaneHelp
                planeNormal
                planeConstant
                first
                []
                (clipFaceAgainstPlaneAdd planeNormal planeConstant last first result)


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
    { dmin = Const.maxNumber, target = Const.zero3 }
        |> testFaceNormals t1 hull1 t2 hull2 t1.orientation (Array.toList hull1.faces)
        |> Maybe.andThen (testFaceNormals t1 hull1 t2 hull2 t2.orientation (Array.toList hull2.faces))
        |> Maybe.andThen (testEdges t1 hull1 t2 hull2)
        |> Maybe.map
            (\{ target } ->
                if Vec3.dot (Vec3.sub t2.position t1.position) target > 0 then
                    Vec3.negate target

                else
                    target
            )


testFaceNormals : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Quaternion -> List Face -> { target : Vec3, dmin : Float } -> Maybe { target : Vec3, dmin : Float }
testFaceNormals t1 hull1 t2 hull2 quat faces bestSoFar =
    case faces of
        [] ->
            Just bestSoFar

        { normal } :: restFaces ->
            let
                rotatedNormal =
                    Quaternion.rotate quat normal
            in
            case testSepAxis t1 hull1 t2 hull2 rotatedNormal of
                Nothing ->
                    Nothing

                Just d ->
                    testFaceNormals
                        t1
                        hull1
                        t2
                        hull2
                        quat
                        restFaces
                        (if d < bestSoFar.dmin then
                            { dmin = d, target = rotatedNormal }

                         else
                            bestSoFar
                        )


testEdges : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> { target : Vec3, dmin : Float } -> Maybe { target : Vec3, dmin : Float }
testEdges t1 hull1 t2 hull2 bestSoFar =
    -- TODO: short circuit the iteraions
    List.foldl
        (\edge1 bestSoFar1 ->
            let
                worldEdge1 =
                    Quaternion.rotate t1.orientation edge1
            in
            List.foldl
                (\edge2 bestSoFar2 ->
                    let
                        worldEdge2 =
                            Quaternion.rotate t2.orientation edge2

                        cross =
                            Vec3.cross worldEdge1 worldEdge2
                    in
                    if almostZero cross then
                        bestSoFar2

                    else
                        case bestSoFar2 of
                            Just { dmin } ->
                                case testSepAxis t1 hull1 t2 hull2 (Vec3.normalize cross) of
                                    Just dist ->
                                        if dist < dmin then
                                            Just
                                                { dmin = dist
                                                , target = Vec3.normalize cross
                                                }

                                        else
                                            bestSoFar2

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                )
                bestSoFar1
                hull2.edges
        )
        (Just bestSoFar)
        hull1.edges


testSepAxis : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Vec3 -> Maybe Float
testSepAxis t1 hull1 t2 hull2 axis =
    let
        ( max1, min1 ) =
            project t1 hull1 axis

        ( max2, min2 ) =
            project t2 hull2 axis
    in
    if max1 < min2 || max2 < min1 then
        Nothing

    else
        Just (min (max1 - min2) (max2 - min1))


{-| Get max and min dot product of a convex hull at Transform projected onto an axis.
-}
project : Transform -> ConvexPolyhedron -> Vec3 -> ( Float, Float )
project transform { vertices } axis =
    let
        localAxis =
            Transform.vectorToLocalFrame transform axis

        add =
            Const.zero3
                |> Transform.pointToLocalFrame transform
                |> Vec3.dot localAxis
    in
    Array.foldl
        (\vec ( maxVal, minVal ) ->
            let
                val =
                    Vec3.dot vec localAxis
            in
            ( max maxVal val, min minVal val )
        )
        ( -Const.maxNumber, Const.maxNumber )
        vertices
        |> (\( maxVal, minVal ) -> ( maxVal - add, minVal - add ))


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
            ( Just <|
                Vec3.sub center <|
                    Vec3.scale distance normal
            , radius - distance
            )

        sphereBoundaryContact : Vec3 -> Float -> ( Maybe Vec3, Float )
        sphereBoundaryContact localContact distanceSq =
            ( Just <| Vec3.add localContact center
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

        reframedVertices =
            vertices
                |> Array.map
                    (\vertex ->
                        Vec3.sub
                            (Transform.pointToWorldFrame t2 vertex)
                            center
                    )

        -- Find the details of the closest faces.
        testFaceResult =
            faces
                |> arrayRecurseUntil
                    isAFaceContact
                    (\{ vertexIndices, normal } statusQuo ->
                        case statusQuo of
                            QualifiedEdges acc ->
                                sphereTestFace
                                    radius
                                    (Quaternion.rotate t2.orientation normal)
                                    reframedVertices
                                    vertexIndices
                                    acc

                            FaceContact _ _ ->
                                -- Since a FaceContact short circuits the
                                -- recursion, this case is not expected.
                                statusQuo
                    )
                    (QualifiedEdges [])
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
sphereTestFace : Float -> Vec3 -> Array Vec3 -> List Int -> List (List ( Vec3, Vec3 )) -> TestFaceResult
sphereTestFace radius normal vertices vertexIndices acc =
    let
        -- Use an arbitrary vertex from the face to measure the distance to
        -- the origin (sphere center) along the face normal.
        faceDistance =
            List.head vertexIndices
                |> Maybe.andThen (\i -> Array.get i vertices)
                |> Maybe.map
                    (\point ->
                        -(Vec3.dot normal point)
                    )
                -- a negative value prevents a face or edge contact match
                |> Maybe.withDefault -1

        faceVertices =
            if faceDistance < radius && faceDistance > 0.0 then
                -- Sphere intersects the face plane.
                -- Check that all the vertices are valid.
                vertexIndices
                    |> List.foldl
                        (\index acc1 ->
                            Maybe.map2
                                (::)
                                (Array.get index vertices)
                                acc1
                        )
                        (Just [])

            else
                Nothing
    in
    case faceVertices of
        -- Require 3 or more valid vertices to proceed
        Just ((_ :: _ :: _ :: _) as validVertices) ->
            -- If vertices are valid, check if the sphere center
            -- projects onto the face plane INSIDE the face polygon.
            case originProjection validVertices normal of
                [] ->
                    -- The projection falls within all the face's edges.
                    FaceContact normal faceDistance

                separatingEdges ->
                    -- These origin-excluding edges are candidates for
                    -- having an edge or vertex contact.
                    QualifiedEdges <| separatingEdges :: acc

        _ ->
            QualifiedEdges acc


{-| The edge or vertex contact point and its distance (squared), if any,
of a ConvexPolyhedron's edges with a sphere, limited to a pre-qualified
list of edges per face.
-}
sphereTestBoundaries : Float -> List (List ( Vec3, Vec3 )) -> TestBoundaryResult
sphereTestBoundaries radius faceEdgeList =
    faceEdgeList
        |> List.foldl
            sphereTestBoundary
            (PossibleVertexContact ( Nothing, radius ^ 2 ))


{-| The edge or possible vertex contact point and its distance (squared),
if any, of a ConvexPolyhedron face's pre-qualified edges with a sphere.
-}
sphereTestBoundary : List ( Vec3, Vec3 ) -> TestBoundaryResult -> TestBoundaryResult
sphereTestBoundary faceEdges statusQuo =
    faceEdges
        |> listRecurseUntil
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
            if vertexLengthSq < minDistanceSq then
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
        PossibleVertexContact <| betterVertexContact prevVertex

    else if offset ^ 2 > Vec3.lengthSquared edge then
        -- vertex is closest in this edge,
        -- but there may be a closer edge or
        -- no contact.
        PossibleVertexContact <| betterVertexContact vertex

    else
        let
            edgeContact =
                Vec3.add prevVertex <|
                    Vec3.scale offset edgeUnit

            edgeDistanceSq =
                Vec3.lengthSquared edgeContact
        in
        if edgeDistanceSq < minDistanceSq then
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
    vertices
        |> listRingFoldStaggeredPairs
            (\prevVertex vertex acc ->
                let
                    edge_x_normal =
                        Vec3.sub vertex prevVertex
                            |> Vec3.cross normal
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


foldFaceNormals : (Vec3 -> Vec3 -> a -> a) -> a -> ConvexPolyhedron -> a
foldFaceNormals fn acc { vertices, faces } =
    faces
        |> Array.foldl
            (\{ vertexIndices, normal } acc1 ->
                let
                    vsum =
                        vertexIndices
                            |> List.foldl
                                (\index acc2 ->
                                    getIndexedVertex vertices index
                                        |> Vec3.add acc2
                                )
                                Const.zero3

                    vcount =
                        List.length vertexIndices
                in
                fn normal (Vec3.scale (1.0 / toFloat vcount) vsum) acc1
            )
            acc


foldUniqueEdges : (Vec3 -> Vec3 -> a -> a) -> a -> ConvexPolyhedron -> a
foldUniqueEdges fn acc { vertices, edges } =
    case Array.get 0 vertices of
        Nothing ->
            acc

        Just vertex0 ->
            edges
                |> List.foldl
                    (\edge -> fn edge vertex0)
                    acc


expandBoundingSphereRadius : Transform -> ConvexPolyhedron -> Float -> Float
expandBoundingSphereRadius shapeTransform { vertices } boundingSphereRadius =
    vertices
        |> Array.foldl
            (\vertex ->
                vertex
                    |> Transform.pointToWorldFrame shapeTransform
                    |> Vec3.lengthSquared
                    |> max
            )
            (boundingSphereRadius * boundingSphereRadius)
        |> sqrt



-- Generic utilities, listed alphabetically.
-- TODO: Consider migrating these to one or more utility modules
-- if they are found useful elsewhere.


arrayRecurseUntil : (b -> Bool) -> (a -> b -> b) -> b -> Array a -> b
arrayRecurseUntil test fn seed array =
    let
        recurse index acc =
            case Array.get index array of
                Just element ->
                    if test acc then
                        acc

                    else
                        fn element acc
                            |> recurse (index + 1)

                Nothing ->
                    acc
    in
    recurse 0 seed


{-| Easily disabled wrapper for Debug.crash.
KEEP DISABLED in published production code.
-}
identityOrCrash : String -> a -> a
identityOrCrash message value =
    -- enabled: Debug.crash message value
    -- disabled: KEEP DISABLED in published production code.
    value


{-| Fold the function over pairs of consecutive elements in the list,
starting with the pair (seed, first), then (first, second), and so on.
-}
listFoldStaggeredPairs : (a -> a -> b -> b) -> b -> a -> List a -> b
listFoldStaggeredPairs fn resultSeed seed list =
    list
        |> List.foldl
            (\current ( acc, staggered ) ->
                case staggered of
                    prev :: tail ->
                        ( fn prev current acc
                        , tail
                        )

                    _ ->
                        -- Since the original list should run out of elements
                        -- one iteration before the staggered list does,
                        -- this case is not expected.
                        ( acc, [] )
            )
            ( resultSeed, seed :: list )
        |> Tuple.first


{-| Just the last element of a list, or Nothing for an empty list.
Equivalent to elm-community/list-extra/7.1.0/List-Extra last.
-}
listLast : List a -> Maybe a
listLast list =
    list
        |> List.drop (List.length list - 1)
        |> List.head


{-| A generic List/Maybe-related utility.
For "Just x", add x to the list; for "Nothing", do nothing.
Examples:
listMaybeAdd [] (Just 1) === [ 1 ]
listMaybeAdd [] Nothing === []
listMaybeAdd [ 2, 1 ] (Just 3) === [ 3, 2, 1 ]
listMaybeAdd [ 2, 1 ] Nothing === [ 2, 1 ]
-}
listMaybeAdd : List a -> Maybe a -> List a
listMaybeAdd list maybe =
    case maybe of
        Nothing ->
            list

        Just head ->
            head :: list


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
starting with the pair (last, first), then (first, second), and so on.
-}
listRingFoldStaggeredPairs : (a -> a -> b -> b) -> b -> List a -> b
listRingFoldStaggeredPairs fn resultSeed list =
    case listLast list of
        Nothing ->
            -- The ring is empty.
            resultSeed

        Just last ->
            listFoldStaggeredPairs fn resultSeed last list


{-| Crash-on-Nothing equivalent of Maybe.withDefault for use in debugging.
KEEP DISABLED in published production code.
-}
maybeWithDefaultOrCrash : String -> a -> Maybe a -> a
maybeWithDefaultOrCrash message default maybe =
    {--enabled:
    case maybe of
        Just value ->
            value

        Nothing ->
            --Debug.crash message
    --}
    -- disabled: KEEP DISABLED in published production code.
    Maybe.withDefault default maybe
