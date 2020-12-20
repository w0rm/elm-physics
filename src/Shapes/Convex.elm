module Shapes.Convex exposing
    ( Convex
    , Face
    , addDirectionIfDistinct
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
    { faces : List Face
    , vertices : List Vec3 -- cached for performance
    , uniqueEdges : List Vec3 -- unique edges
    , uniqueNormals : List Vec3 -- unique face normals
    , position : Vec3
    , inertia : Mat3
    , volume : Float
    }


placeIn : Transform3d coordinates defines -> Convex -> Convex
placeIn transform3d { faces, vertices, uniqueEdges, uniqueNormals, position, volume, inertia } =
    { faces = facesPlaceInHelp transform3d faces []
    , vertices = Transform3d.pointsPlaceIn transform3d vertices
    , uniqueEdges = Transform3d.directionsPlaceIn transform3d uniqueEdges
    , uniqueNormals = Transform3d.directionsPlaceIn transform3d uniqueNormals
    , volume = volume
    , position = Transform3d.pointPlaceIn transform3d position
    , inertia = Transform3d.inertiaRotateIn transform3d inertia
    }


{-| Places faces into the frame.
-}
facesPlaceInHelp : Transform3d coordinates defines -> List Face -> List Face -> List Face
facesPlaceInHelp transform3d faces result =
    case faces of
        { vertices, normal } :: remainingFaces ->
            facesPlaceInHelp
                transform3d
                remainingFaces
                ({ vertices = List.reverse (Transform3d.pointsPlaceIn transform3d vertices)
                 , normal = Transform3d.directionPlaceIn transform3d normal
                 }
                    :: result
                )

        [] ->
            result


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
    { faces = faces
    , vertices = allVertices
    , uniqueEdges =
        List.foldl
            (\face edges ->
                foldFaceEdges
                    (\v1 v2 ->
                        addDirectionIfDistinct (Vec3.direction v1 v2)
                    )
                    edges
                    face.vertices
            )
            []
            faces
    , uniqueNormals =
        List.foldl
            (\{ normal } ->
                addDirectionIfDistinct normal
            )
            []
            faces
    , position = position
    , volume = volume
    , inertia = inertia
    }


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
                    case indices of
                        ( i1, i2, i3 ) ->
                            ( i2, i1 ) :: ( i3, i2 ) :: ( i1, i3 ) :: res
                )
                []
                coplanar

        newFacesToCheck =
            nonCoplanar ++ facesToCheck

        newContour =
            List.foldl (\{ indices } -> extendContour indices) currentContour coplanar
    in
    if coplanar /= [] then
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

    else
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
                    if (ci1 == ti2) && (ci2 == ti1) then
                        -- insert ti3
                        List.reverse result ++ (ci1 :: ti3 :: rest1)

                    else if (ci1 == ti3) && (ci2 == ti2) then
                        -- insert ti1
                        List.reverse result ++ (ci1 :: ti1 :: rest1)

                    else if (ci1 == ti1) && (ci2 == ti3) then
                        -- insert ti2
                        List.reverse result ++ (ci1 :: ti2 :: rest1)

                    else
                        extendContourHelp triangle i1 rest1 (ci1 :: result)

                [] ->
                    if (ci1 == ti2) && (i1 == ti1) then
                        -- insert ti3
                        List.reverse (ti3 :: ci1 :: result)

                    else if (ci1 == ti3) && (i1 == ti2) then
                        -- insert ti1
                        List.reverse (ti1 :: ci1 :: result)

                    else if (ci1 == ti1) && (i1 == ti3) then
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
        [ { vertices = [ v3, v2, v1, v0 ], normal = Vec3.zNegative }
        , { vertices = [ v4, v5, v6, v7 ], normal = Vec3.zAxis }
        , { vertices = [ v5, v4, v0, v1 ], normal = Vec3.yNegative }
        , { vertices = [ v2, v3, v7, v6 ], normal = Vec3.yAxis }
        , { vertices = [ v0, v4, v7, v3 ], normal = Vec3.xNegative }
        , { vertices = [ v1, v2, v6, v5 ], normal = Vec3.xAxis }
        ]
    , vertices = [ v0, v1, v2, v3, v4, v5, v6, v7 ]
    , uniqueEdges = Vec3.basis
    , uniqueNormals = Vec3.basis
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

        uniqueSideNormals =
            List.map .normal <|
                if modBy 2 subdivisions == 0 then
                    List.take (subdivisions // 2) sides

                else
                    sides
    in
    { faces =
        topCap
            :: bottomCap
            :: List.map
                (\{ v0, v1, v2, v3, normal } ->
                    { vertices = [ v0, v1, v2, v3 ], normal = normal }
                )
                sides
    , vertices = topCap.vertices ++ bottomCap.vertices
    , uniqueEdges =
        Vec3.zAxis
            -- Rotate normals by 90 degrees to get edge directions
            :: List.map (\{ x, y } -> { x = -y, y = x, z = 0 }) uniqueSideNormals
    , uniqueNormals = Vec3.zAxis :: uniqueSideNormals
    , position = Vec3.zero
    , inertia = Mat3.cylinderInertia volume radius length
    , volume = volume
    }


{-| Add a candidate direction to a set if it is not a
near duplicate or near opposite of a direction already in the set.
-}
addDirectionIfDistinct : Vec3 -> List Vec3 -> List Vec3
addDirectionIfDistinct currentNormal uniques =
    if
        List.any
            (\direction ->
                Vec3.almostZero
                    (Vec3.cross
                        direction
                        currentNormal
                    )
            )
            uniques
    then
        uniques

    else
        currentNormal :: uniques


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
