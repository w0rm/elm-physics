module Internal.Convex exposing
    ( AdjacentFace
    , Convex
    , Face
    , addFaceEdges
    , expandBoundingSphereRadius
    , faceAdjacency
    , foldFaceEdges
    , foldFaceNormals
    , foldUniqueEdges
    , fromBlock
    , init
    , initUniqueEdges
    , raycast
    )

import Array exposing (Array)
import Dict
import Direction3d
import Frame3d exposing (Frame3d)
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Length exposing (Meters)
import Physics.Coordinates exposing (ShapeCoordinates, WorldCoordinates)
import Point3d
import Set


type alias AdjacentFace =
    { point : Vec3
    , normal : Vec3
    }


type alias Face =
    { vertices : List Vec3
    , normal : Vec3
    , point : Vec3 -- the first point on the face
    , adjacentFaces : List AdjacentFace
    }


type alias Convex =
    { faces : List Face
    , vertices : List Vec3 -- cached for performance
    , uniqueEdges : List Vec3 -- unique edges
    , uniqueNormals : List Vec3 -- unique face normals
    }


init : List (List Int) -> Array Vec3 -> Convex
init faceVertexLists vertices =
    let
        faces =
            initFaces faceVertexLists vertices
    in
    { faces = initFaces faceVertexLists vertices
    , vertices = Array.toList vertices
    , uniqueEdges = initUniqueEdges faces
    , uniqueNormals = initUniqueNormals faces
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
                                    -- Shouldn’t happen
                                    ( Vec3.zero, Vec3.zero )
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


fromBlock : Float -> Float -> Float -> Convex
fromBlock x y z =
    let
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

        p0 =
            v3

        n0 =
            { x = 0, y = 0, z = -1 }

        p1 =
            v4

        n1 =
            Vec3.k

        p2 =
            v5

        n2 =
            { x = 0, y = -1, z = 0 }

        p3 =
            v2

        n3 =
            Vec3.j

        p4 =
            v0

        n4 =
            { x = -1, y = 0, z = 0 }

        p5 =
            v1

        n5 =
            Vec3.i
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
    , uniqueEdges = [ Vec3.i, Vec3.j, Vec3.k ]
    , uniqueNormals = [ Vec3.i, Vec3.j, Vec3.k ]
    }


initUniqueNormals : List Face -> List Vec3
initUniqueNormals faces =
    List.foldl (\{ normal } -> addNormalIfDistinct normal) [] faces


alreadyInTheSet : Vec3 -> List Vec3 -> Bool
alreadyInTheSet vec =
    List.any (Vec3.cross vec >> Vec3.almostZero)


addNormalIfDistinct : Vec3 -> List Vec3 -> List Vec3
addNormalIfDistinct currentNormal uniques =
    if alreadyInTheSet currentNormal uniques then
        uniques

    else
        currentNormal :: uniques


initUniqueEdges : List Face -> List Vec3
initUniqueEdges faces =
    List.foldl addFaceEdges [] faces


addFaceEdges : Face -> List Vec3 -> List Vec3
addFaceEdges { vertices } edges =
    foldFaceEdges
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
    in
    if alreadyInTheSet candidateEdge uniques then
        uniques

    else
        candidateEdge :: uniques


foldFaceNormals : (Vec3 -> Vec3 -> a -> a) -> a -> Convex -> a
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


foldUniqueEdges : (Vec3 -> Vec3 -> a -> a) -> a -> Convex -> a
foldUniqueEdges fn acc { vertices, uniqueEdges } =
    case vertices of
        vertex0 :: _ ->
            List.foldl (fn vertex0) acc uniqueEdges

        [] ->
            acc


expandBoundingSphereRadius : Frame3d Meters CenterOfMassCoordinates { defines : ShapeCoordinates } -> Convex -> Float -> Float
expandBoundingSphereRadius frame3d { vertices } boundingSphereRadius =
    vertices
        |> List.foldl
            (\vertex ->
                vertex
                    |> Point3d.fromMeters
                    |> Point3d.placeIn frame3d
                    |> Point3d.toMeters
                    |> Vec3.lengthSquared
                    |> max
            )
            (boundingSphereRadius * boundingSphereRadius)
        |> sqrt


raycast : { from : Vec3, direction : Vec3 } -> Frame3d Meters WorldCoordinates { defines : ShapeCoordinates } -> Convex -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { direction, from } frame3d convex =
    List.foldl
        (\face maybeHit ->
            let
                faceNormalWS =
                    Direction3d.unwrap (Direction3d.placeIn frame3d (Direction3d.unsafe face.normal))

                dot =
                    Vec3.dot direction faceNormalWS
            in
            if dot < 0 then
                let
                    pointOnFaceWS =
                        Point3d.toMeters (Point3d.placeIn frame3d (Point3d.fromMeters face.point))

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
                                        Point3d.toMeters (Point3d.placeIn frame3d (Point3d.fromMeters p)) :: acc1
                                    )
                                    []
                                |> foldFaceEdges
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
