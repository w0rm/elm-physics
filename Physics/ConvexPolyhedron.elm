module Physics.ConvexPolyhedron
    exposing
        ( ConvexPolyhedron
        , Face
        , findSeparatingAxis
        , clipAgainstHull
        , fromBox
        , expandBoundingSphereRadius
        , sphereContact
          -- exposed only for tests
        , testSepAxis
        , addFaceEdges
        , init
        , faceAdjacency
        , faceNormal
        , uniqueEdges
        , project
        , clipFaceAgainstHull
        , clipFaceAgainstPlane
        , foldFaceNormals
        , foldUniqueEdges
        )

import Array.Hamt as Array exposing (Array)
import Dict
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Physics.Const as Const
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)
import Set


almostZero : Vec3 -> Bool
almostZero vec =
    let
        { x, y, z } =
            Vec3.toRecord vec
    in
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
    , edges = uniqueEdges faceVertexLists vertices
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
                , normal = faceNormal vertexIndices vertices
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
                            |> List.map ((,) face)
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
fromBox halfExtents =
    let
        { x, y, z } =
            Vec3.toRecord halfExtents
    in
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


faceNormal : List Int -> Array Vec3 -> Vec3
faceNormal indices vertices =
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
            defaultOrCrash
                "Couldn't compute normal with < 3 vertices"
                Const.zero3


uniqueEdges : List (List Int) -> Array Vec3 -> List Vec3
uniqueEdges faceVertexLists vertices =
    faceVertexLists
        |> List.foldl (addFaceEdges vertices) []


addFaceEdges : Array Vec3 -> List Int -> List Vec3 -> List Vec3
addFaceEdges vertices vertexIndices edges =
    vertexIndices
        |> listRingFoldStaggeredPairs
            (\prev current acc ->
                addEdgeIfDistinct
                    (Array.get current vertices)
                    (Array.get prev vertices)
                    acc
            )
            edges


{-| Add a candidate edge between two vertices to a set if it is not a
near duplicate or near opposite of an edge already in the set.
-}
addEdgeIfDistinct : Maybe Vec3 -> Maybe Vec3 -> List Vec3 -> List Vec3
addEdgeIfDistinct currentVertex prevVertex uniques =
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
                    Quaternion.rotate t1.quaternion normal

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
                                    |> (,) otherFace.normal
                           )
            in
                adjacentFaces
                    |> List.foldl
                        (\otherFaceIndex ->
                            let
                                ( otherFaceNormal, otherFaceVertex ) =
                                    otherFaceDetails otherFaceIndex

                                localPlaneEq =
                                    -(Vec3.dot otherFaceVertex otherFaceNormal)

                                planeNormalWS =
                                    Quaternion.rotate t1.quaternion otherFaceNormal

                                planeEqWS =
                                    localPlaneEq - Vec3.dot planeNormalWS t1.position
                            in
                                clipFaceAgainstPlane planeNormalWS planeEqWS
                        )
                        worldVertsB
                    |> List.foldl
                        (\point result ->
                            let
                                depth =
                                    max minDist (Vec3.dot planeNormalWS point + planeEqWS)
                            in
                                if depth <= maxDist then
                                    if depth <= 0 then
                                        { point = point
                                        , normal = planeNormalWS
                                        , depth = depth
                                        }
                                            :: result
                                    else
                                        result
                                else
                                    result
                        )
                        []

        Nothing ->
            []


{-| Encapsulate a compatible comparison operator and a worst case value
according to that operator such that for any DistanceCriterion dc:
(compareOp dc) _ (worstValue dc) == True
(compareOp dc) (worstValue dc) _ == False
DistanceCriterion prevents accidental combination of comparators with
inappropriate worst case values such as when starting an iterative run-off.
It should be easier and less error prone to write and use generic run-off
folds dependent on a DistanceCriteria.
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
                                |> Quaternion.rotate transform.quaternion
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
                |> (\l ->
                        -- move the last element to be the first element
                        -- TODO: not sure if this is needed
                        let
                            head =
                                List.take 1 l

                            tail =
                                List.drop 1 l
                        in
                            head ++ List.reverse tail
                   )

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
                (lerp (nDotPrev / (nDotPrev - nDotNext)) prev next)
                    :: result
        else if nDotNext < 0 then
            next
                :: (lerp (nDotPrev / (nDotPrev - nDotNext)) prev next)
                :: result
        else
            result


lerp : Float -> Vec3 -> Vec3 -> Vec3
lerp t v1 v2 =
    Vec3.add v1 (Vec3.scale t (Vec3.sub v2 v1))


findSeparatingAxis : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Maybe Vec3
findSeparatingAxis t1 hull1 t2 hull2 =
    let
        bestFaceNormal : Vec4 -> List Face -> { target : Vec3, dmin : Float } -> Maybe { target : Vec3, dmin : Float }
        bestFaceNormal quat faces bestSoFar =
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
                                (if d < bestSoFar.dmin then
                                    { dmin = d, target = rotatedNormal }
                                 else
                                    bestSoFar
                                )
                                    |> bestFaceNormal
                                        quat
                                        restFaces
    in
        { dmin = Const.maxNumber, target = Const.zero3 }
            |> bestFaceNormal t1.quaternion (Array.toList hull1.faces)
            |> Maybe.andThen (bestFaceNormal t2.quaternion (Array.toList hull2.faces))
            |> Maybe.andThen (testEdges t1 hull1 t2 hull2)
            |> Maybe.map
                (\{ target } ->
                    if Vec3.dot (Vec3.sub t2.position t1.position) target > 0 then
                        Vec3.negate target
                    else
                        target
                )


testEdges : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> { target : Vec3, dmin : Float } -> Maybe { target : Vec3, dmin : Float }
testEdges t1 hull1 t2 hull2 context =
    -- TODO: short circuit the iteraions
    List.foldl
        (\edge1 acc1 ->
            let
                worldEdge1 =
                    Quaternion.rotate t1.quaternion edge1
            in
                List.foldl
                    (\edge2 acc2 ->
                        let
                            worldEdge2 =
                                Quaternion.rotate t2.quaternion edge2

                            cross =
                                Vec3.cross worldEdge1 worldEdge2
                        in
                            if almostZero cross then
                                acc2
                            else
                                case acc2 of
                                    Just context ->
                                        case testSepAxis t1 hull1 t2 hull2 (Vec3.normalize cross) of
                                            Just dist ->
                                                if dist < context.dmin then
                                                    Just
                                                        { dmin = dist
                                                        , target = Vec3.normalize cross
                                                        }
                                                else
                                                    acc2

                                            Nothing ->
                                                Nothing

                                    Nothing ->
                                        Nothing
                    )
                    acc1
                    hull2.edges
        )
        (Just context)
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
            |> (\( maxVal, minVal ) ->
                    ( maxVal - add
                    , minVal - add
                    )
               )


sphereContact : Vec3 -> Float -> Transform -> ConvexPolyhedron -> ( Maybe Vec3, Float )
sphereContact center radius t2 { vertices, faces } =
    -- Check corners
    -- TODO: This check could be deferred. A vertex contact check might be
    -- more efficiently applied to specific vertices discovered in the most
    -- likely faces and edges as discovered in the later pass.
    sphereVertexContact center radius t2 vertices
        -- else check faces and face edges
        |> sphereAllFacesContact center radius t2 vertices faces


sphereVertexContact : Vec3 -> Float -> Transform -> Array Vec3 -> ( Maybe Vec3, Float )
sphereVertexContact center radius t2 vertices =
    vertices
        |> Array.foldl
            (\vertex (( _, maxPenetration ) as statusQuo) ->
                let
                    -- World position of corner
                    worldCorner =
                        Transform.pointToWorldFrame t2 vertex

                    penetration =
                        radius - Vec3.distance worldCorner center
                in
                    if penetration > maxPenetration then
                        ( Just worldCorner, penetration )
                    else
                        statusQuo
            )
            -- Initial state for (maybeContact, maxPenetration)
            ( Nothing, 0.0 )


sphereAllFacesContact : Vec3 -> Float -> Transform -> Array Vec3 -> Array Face -> ( Maybe Vec3, Float ) -> ( Maybe Vec3, Float )
sphereAllFacesContact center radius t2 vertices faces result =
    faces
        |> Array.foldl
            (sphereFaceContact
                center
                radius
                t2
                vertices
            )
            result


sphereFaceContact : Vec3 -> Float -> Transform -> Array Vec3 -> Face -> ( Maybe Vec3, Float ) -> ( Maybe Vec3, Float )
sphereFaceContact center radius t2 vertices { vertexIndices, normal } (( _, maxPenetration ) as statusQuo) =
    let
        -- Get world-transformed normal of the face
        worldFacePlaneNormal =
            Quaternion.rotate t2.quaternion normal

        -- Get an arbitrary world vertex from the face
        worldPoint =
            List.head vertexIndices
                |> Maybe.andThen (\i -> Array.get i vertices)
                |> Maybe.map
                    (Transform.pointToWorldFrame t2)

        penetration =
            worldPoint
                |> Maybe.map
                    (\point ->
                        worldFacePlaneNormal
                            |> Vec3.scale radius
                            |> Vec3.sub center
                            |> Vec3.sub point
                            |> Vec3.dot worldFacePlaneNormal
                    )
                |> Maybe.withDefault -1

        dot =
            worldPoint
                |> Maybe.map
                    (\point ->
                        Vec3.dot
                            (Vec3.sub center point)
                            worldFacePlaneNormal
                    )
                |> Maybe.withDefault -1

        worldVertices =
            if penetration > maxPenetration && dot > 0 then
                -- Sphere intersects the face plane.
                vertexIndices
                    |> List.map
                        (\index ->
                            Array.get index vertices
                                |> Maybe.map
                                    (\vertex ->
                                        ( (Transform.pointToWorldFrame t2 vertex)
                                        , True
                                        )
                                    )
                                |> Maybe.withDefault ( Const.zero3, False )
                        )
                    |> (\tuples ->
                            -- Check that all the world vertices are valid.
                            if
                                tuples
                                    |> List.foldl
                                        (\tuple valid ->
                                            if valid then
                                                (Tuple.second tuple)
                                            else
                                                False
                                        )
                                        True
                            then
                                -- Extract the world vertices
                                tuples
                                    |> List.map Tuple.first
                            else
                                []
                       )
            else
                []
    in
        -- If vertices are valid, Check if the sphere center is inside the
        -- normal projection of the face polygon.
        if pointInPolygon worldVertices worldFacePlaneNormal center then
            let
                worldContact =
                    worldFacePlaneNormal
                        |> Vec3.scale (penetration - radius)
                        |> Vec3.add center
            in
                ( Just worldContact, penetration )
        else
            -- Try the face's edges
            sphereFaceEdgesContact
                center
                radius
                (vertexIndices
                    |> List.filterMap
                        (\index ->
                            Array.get index vertices
                                |> Maybe.map
                                    (Transform.pointToWorldFrame t2)
                        )
                )
                statusQuo


sphereFaceEdgesContact : Vec3 -> Float -> List Vec3 -> ( Maybe Vec3, Float ) -> ( Maybe Vec3, Float )
sphereFaceEdgesContact center radius worldVertices statusQuo =
    worldVertices
        |> listRingFoldStaggeredPairs
            (\vertex prevVertex (( _, maxPenetration ) as statusQuo1) ->
                let
                    edge =
                        Vec3.sub vertex prevVertex

                    -- The normalized edge vector
                    edgeUnit =
                        Vec3.normalize edge

                    -- The potential contact is where the sphere center
                    -- projects onto the edge.
                    -- dot is the directed distance between the edge's
                    -- starting vertex and that projection. If it is not
                    -- between 0 and the edge's length, the projection
                    -- is invalid.
                    dot =
                        Vec3.dot (Vec3.sub center prevVertex) edgeUnit
                in
                    if
                        (dot > 0)
                            && (dot * dot < Vec3.lengthSquared edge)
                    then
                        let
                            worldContact =
                                Vec3.scale dot edgeUnit
                                    |> Vec3.add prevVertex

                            penetration =
                                radius - Vec3.distance worldContact center
                        in
                            -- Edge collision only occurs if the
                            -- projection is within the sphere.
                            if penetration > maxPenetration then
                                ( Just worldContact, penetration )
                            else
                                statusQuo1
                    else
                        -- TODO: A vertex contact check might be more efficient
                        -- here than in a prior pass
                        statusQuo1
            )
            statusQuo


pointInPolygon : List Vec3 -> Vec3 -> Vec3 -> Bool
pointInPolygon vertices normal position =
    if List.length vertices < 3 then
        False
    else
        vertices
            |> listRingFoldStaggeredPairs
                (\vertex prevVertex ( acc, precedent ) ->
                    if acc then
                        let
                            edge =
                                Vec3.sub vertex prevVertex

                            edge_x_normal =
                                Vec3.cross edge normal

                            vertex_to_p =
                                Vec3.sub position prevVertex

                            -- This dot product determines which side
                            -- of the edge the point is.
                            -- It must be consistent for all edges for the
                            -- point to be within the face.
                            side =
                                (Vec3.dot edge_x_normal vertex_to_p) > 0
                        in
                            case precedent of
                                Nothing ->
                                    ( True
                                    , side |> Just
                                    )

                                Just determinedPrecedent ->
                                    ( side == determinedPrecedent
                                    , precedent
                                    )
                    else
                        ( False, Nothing )
                )
                ( True, Nothing )
            |> Tuple.first


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
                    fn normal (Vec3.scale (1.0 / (toFloat vcount)) vsum) acc1
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


listLast : List a -> Maybe a
listLast list =
    list
        |> List.drop ((List.length list) - 1)
        |> List.head


{-| A generic List/Maybe-related utility.
For a "Just x" value add x to the list; for a "Nothing" value, do nothing.
Example:
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


{-| Map the function to pairs of consecutive elements in the ring list,
starting with the pair (first, last), then (second, first), and so on.
-}
listRingFoldStaggeredPairs : (a -> a -> b -> b) -> b -> List a -> b
listRingFoldStaggeredPairs fn resultSeed list =
    case listLast list of
        Nothing ->
            resultSeed

        Just last ->
            listFoldStaggeredPairs fn resultSeed last list


{-| Map the function to pairs of consecutive elements in the list,
starting with the pair (first, seed), then (second, first), and so on.
-}
listFoldStaggeredPairs : (a -> a -> b -> b) -> b -> a -> List a -> b
listFoldStaggeredPairs fn resultSeed seed list =
    list
        |> List.foldl
            (\current ( acc1, staggered1 ) ->
                case staggered1 of
                    prev :: tail ->
                        ( fn prev current acc1
                        , tail
                        )

                    _ ->
                        ( acc1, [] )
            )
            ( resultSeed, seed :: list )
        |> Tuple.first


arrayFoldWhileNothing : (a -> Maybe b) -> Maybe b -> Array a -> Maybe b
arrayFoldWhileNothing fn seed array =
    array
        |> Array.foldl
            (\element acc ->
                case acc of
                    Nothing ->
                        fn element

                    _ ->
                        acc
            )
            seed


arrayRecurseWhileNothing : (a -> Maybe b) -> Maybe b -> Array a -> Maybe b
arrayRecurseWhileNothing fn seed array =
    let
        recurse index =
            case Array.get index array of
                Nothing ->
                    Nothing

                Just element ->
                    case fn element of
                        Nothing ->
                            recurse (index + 1)

                        Just result ->
                            Just result
    in
        recurse 0


type Lazy
    = Now


arrayGetOrCrash : String -> Array a -> Int -> Maybe a
arrayGetOrCrash debugTag array i =
    Array.get i array
        |> maybeAndThenOrLazyCrash
            (\lazy ->
                "invalid index "
                    ++ (toString i)
                    ++ " into the "
                    ++ (toString (Array.length array))
                    ++ debugTag
                    ++ " array"
            )
            Just


defaultOrCrash : String -> a -> a
defaultOrCrash message default =
    -- enabled: Debug.crash message
    -- disabled:
    default


maybeAndThenOrLazyCrash : (Lazy -> String) -> (a -> Maybe b) -> Maybe a -> Maybe b
maybeAndThenOrLazyCrash messageFn fn maybe =
    {--enabled:
    case maybe of
        Just value ->
            fn value

        Nothing ->
            messageFn Now |> --Debug.crash
    --}
    -- disabled:
    Maybe.andThen fn maybe


maybeAndThenOrCrash : String -> (a -> Maybe b) -> Maybe a -> Maybe b
maybeAndThenOrCrash message fn maybe =
    {--enabled:
    case maybe of
        Just value ->
            fn value

        Nothing ->
            --Debug.crash message
    --}
    -- disabled:
    Maybe.andThen fn maybe


maybeMapOrCrash : String -> (a -> b) -> Maybe a -> Maybe b
maybeMapOrCrash message fn maybe =
    {--enabled:
    case maybe of
        Just value ->
            Just <| fn value

        Nothing ->
            --Debug.crash message
    --}
    -- disabled:
    Maybe.map fn maybe


maybeWithDefaultOrCrash : String -> a -> Maybe a -> a
maybeWithDefaultOrCrash message default maybe =
    {--enabled:
    case maybe of
        Just value ->
            value

        Nothing ->
            --Debug.crash message
    --}
    -- disabled:
    Maybe.withDefault default maybe
