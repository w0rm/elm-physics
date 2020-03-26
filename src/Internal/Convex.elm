module Internal.Convex exposing
    ( Convex
    , Face
    , addFaceEdges
    , expandBoundingSphereRadius
    , foldFaceEdges
    , fromBlock
    , init
    , initUniqueEdges
    , placeIn
    , raycast
    )

import Array exposing (Array)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias Face =
    { vertices : List Vec3
    , normal : Vec3
    }


type alias Convex =
    { faces : List Face
    , vertices : List Vec3 -- cached for performance
    , uniqueEdges : List Vec3 -- unique edges
    , uniqueNormals : List Vec3 -- unique face normals
    , position : Vec3
    , volume : Float
    }


placeIn : Transform3d coordinates defines -> Convex -> Convex
placeIn transform3d { faces, vertices, uniqueEdges, uniqueNormals, position, volume } =
    { faces = List.foldl (\face result -> facePlaceIn transform3d face :: result) [] faces
    , vertices = List.foldl (\vertex result -> Transform3d.pointPlaceIn transform3d vertex :: result) [] vertices
    , uniqueEdges = List.foldl (\edge result -> Transform3d.directionPlaceIn transform3d edge :: result) [] uniqueEdges
    , uniqueNormals = List.foldl (\normal result -> Transform3d.directionPlaceIn transform3d normal :: result) [] uniqueNormals
    , volume = volume
    , position = Transform3d.pointPlaceIn transform3d position
    }


{-| Places a face into the frame.
Note that this reverses face vertices list,
but it is reversed originally so this is fine
-}
facePlaceIn : Transform3d coordinates defines -> Face -> Face
facePlaceIn transform3d { vertices, normal } =
    { vertices = List.foldl (\point result -> Transform3d.pointPlaceIn transform3d point :: result) [] vertices
    , normal = Transform3d.directionPlaceIn transform3d normal
    }


init : List (List Int) -> Array Vec3 -> Convex
init faceVertexLists vertices =
    let
        faces =
            initFaces faceVertexLists vertices
    in
    { faces = faces
    , vertices = Array.toList vertices
    , uniqueEdges = initUniqueEdges faces
    , uniqueNormals = initUniqueNormals faces
    , position = Vec3.zero
    , volume = 0
    }


initFaces : List (List Int) -> Array Vec3 -> List Face
initFaces faceVertexLists convexVertices =
    List.map
        (\vertexIndices ->
            let
                vertices =
                    List.filterMap (\i -> Array.get i convexVertices) vertexIndices

                normal =
                    case vertices of
                        v1 :: v2 :: v3 :: _ ->
                            computeNormal v1 v2 v3

                        _ ->
                            -- Shouldn’t happen
                            Vec3.zero
            in
            { vertices = List.reverse vertices
            , normal = normal
            }
        )
        faceVertexLists


computeNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
computeNormal v1 v2 v3 =
    Vec3.normalize (Vec3.cross (Vec3.sub v3 v2) (Vec3.sub v1 v2))


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
    in
    { faces =
        -- faces vertices are reversed for local coordinates
        -- then they become correct after transformation
        -- this is needed for performance
        [ { vertices = List.reverse [ v3, v2, v1, v0 ]
          , normal = { x = 0, y = 0, z = -1 }
          }
        , { vertices = List.reverse [ v4, v5, v6, v7 ]
          , normal = Vec3.k
          }
        , { vertices = List.reverse [ v5, v4, v0, v1 ]
          , normal = { x = 0, y = -1, z = 0 }
          }
        , { vertices = List.reverse [ v2, v3, v7, v6 ]
          , normal = Vec3.j
          }
        , { vertices = List.reverse [ v0, v4, v7, v3 ]
          , normal = { x = -1, y = 0, z = 0 }
          }
        , { vertices = List.reverse [ v1, v2, v6, v5 ]
          , normal = Vec3.i
          }
        ]
    , vertices = [ v0, v1, v2, v3, v4, v5, v6, v7 ]
    , uniqueEdges = Vec3.basis
    , uniqueNormals = Vec3.basis
    , volume = x * y * z * 8
    , position = Vec3.zero
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
    List.foldl
        (\{ normal, vertices } maybeHit ->
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
                                                (Vec3.sub p2 p1)
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
