module Physics.ConvexPolyhedron
    exposing
        ( ConvexPolyhedron
        , Face
        , findSeparatingAxis
        , clipAgainstHull
        , fromBox
        , expandBoundingSphereRadius
          -- exposed only for tests
        , testSepAxis
        , addFaceEdges
        , init
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
        faceEdgeLists =
            faceVertexLists
                |> List.indexedMap
                    (\index vertexList ->
                        vertexList
                            |> listRingFoldStaggeredPairs
                                (\vertex1 vertex2 acc ->
                                    ( index, ( vertex1, vertex2 ) ) :: acc
                                )
                                []
                    )

        edgeToFaceMap =
            faceEdgeLists
                |> List.foldl
                    (\edgeList acc ->
                        edgeList
                            |> List.foldl
                                (\( face, edge ) acc1 ->
                                    Dict.insert edge face acc1
                                )
                                acc
                    )
                    Dict.empty
    in
        faceEdgeLists
            |> List.map
                (List.map
                    (\( _, ( vertex1, vertex2 ) ) ->
                        -- adjacent faces will list a complementary
                        -- (reversed) edge
                        Dict.get ( vertex2, vertex1 ) edgeToFaceMap
                            |> Maybe.withDefault -1
                    )
                )


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


type Lazy
    = Now


maybeMapOrCrash : (a -> b) -> String -> Maybe a -> b
maybeMapOrCrash fn message maybe =
    case maybe of
        Just value ->
            fn value

        Nothing ->
            Debug.crash message


maybeMapOrLazyCrash : (a -> b) -> (Lazy -> String) -> Maybe a -> b
maybeMapOrLazyCrash fn messageFn maybe =
    case maybe of
        Just value ->
            fn value

        Nothing ->
            messageFn Now |> Debug.crash


maybeCrashOnNothing : String -> Maybe a -> a
maybeCrashOnNothing =
    maybeMapOrCrash identity


maybeLazyCrashOnNothing : (Lazy -> String) -> Maybe a -> a
maybeLazyCrashOnNothing =
    maybeMapOrLazyCrash identity


faceNormal : List Int -> Array Vec3 -> Vec3
faceNormal indices vertices =
    case indices of
        i1 :: i2 :: i3 :: _ ->
            Maybe.map3 computeNormal
                (Array.get i1 vertices)
                (Array.get i2 vertices)
                (Array.get i3 vertices)
                |> maybeCrashOnNothing
                    "Couldn't compute normal with invalid vertex index"

        _ ->
            Debug.crash "Couldn't compute normal with < 3 vertices"


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
        List.foldl
            (\member candidate ->
                Maybe.andThen (distinctOrNothing member) candidate
            )
            candidateEdge
            uniques
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
                (List.map
                    (\i ->
                        Array.get i hull2.vertices
                            |> Maybe.map (Transform.pointToWorldFrame t2)
                            -- Sorry
                            |> Maybe.withDefault Const.zero3
                    )
                    vertexIndices
                )
                minDist
                maxDist

        Nothing ->
            []


clipFaceAgainstHull : Transform -> ConvexPolyhedron -> Vec3 -> List Vec3 -> Float -> Float -> List ClipResult
clipFaceAgainstHull t1 hull1 separatingNormal worldVertsB minDist maxDist =
    case bestFace Nearest t1 hull1.faces separatingNormal of
        Just { vertexIndices, normal, adjacentFaces } ->
            let
                localPlaneEq =
                    -(List.head vertexIndices
                        |> Maybe.andThen (\i -> Array.get i hull1.vertices)
                        -- Sorry:
                        |> Maybe.withDefault Const.zero3
                        |> Vec3.dot normal
                     )

                planeNormalWS =
                    Quaternion.rotate t1.quaternion normal

                planeEqWS =
                    localPlaneEq - Vec3.dot planeNormalWS t1.position

                otherFaceDetails : Int -> ( Vec3, Vec3 )
                otherFaceDetails otherFaceIndex =
                    Array.get otherFaceIndex hull1.faces
                        {- Sorry: opting out of early strict check
                           |> maybeCrashOnNothing
                               "face index is invalid"
                        -}
                        |> Maybe.andThen
                            (\otherFace ->
                                otherFace.vertexIndices
                                    |> List.head
                                    {- Sorry: opting out of early strict check
                                       |> maybeMapOrCrash
                                           (\i -> Array.get i hull1.vertices)
                                           "vertexIndices is empty for face"
                                       |>  maybeMapOrLazyCrash
                                               (\lazy ->
                                                   " vertexIndices contains an invalid index into the "
                                                       ++ ( toString (Array.length hull1.vertices))
                                                       ++ " shape vertices "
                                               )
                                               ((,) otherFace.normal)
                                    -}
                                    |> Maybe.andThen (\i -> Array.get i hull1.vertices)
                                    |> Maybe.map ((,) otherFace.normal)
                             -- ... Sorry.
                            )
                        {- Sorry: opting out of early strict check
                           |> maybeCrashOnNothing
                               "face index is invalid"
                        -}
                        |> Maybe.withDefault
                            ( Const.zero3, Const.zero3 )

                -- ... Sorry.
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


foldFaceNormals : (Vec3 -> Vec3 -> a -> a) -> a -> ConvexPolyhedron -> a
foldFaceNormals fn acc { vertices, faces } =
    faces
        |> Array.foldl
            (\{ vertexIndices, normal } acc1 ->
                let
                    maybeResult =
                        vertexIndices
                            |> List.foldl
                                (\index acc2 ->
                                    Array.get index vertices
                                        |> Maybe.map2 Vec3.add acc2
                                )
                                (Just Const.zero3)

                    vcount =
                        List.length vertexIndices
                in
                    case maybeResult of
                        Just vsum ->
                            fn normal (Vec3.scale (1.0 / (toFloat vcount)) vsum) acc1

                        -- ignore ill-formed normal or vertex arrays
                        Nothing ->
                            acc1
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
