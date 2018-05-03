module Physics.ConvexPolyhedron
    exposing
        ( ConvexPolyhedron
        , findSeparatingAxis
        , clipAgainstHull
        , fromBox
          -- only for tests
        , testSepAxis
        , project
        , clipFaceAgainstHull
        , clipFaceAgainstPlane
        )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)
import Set exposing (Set)
import Array.Hamt as Array exposing (Array)


zero3 : Vec3
zero3 =
    vec3 0 0 0


maxNumber : Float
maxNumber =
    3.40282347e38


precision : Float
precision =
    1.0e-6


almostZero : Vec3 -> Bool
almostZero vec =
    let
        { x, y, z } =
            Vec3.toRecord vec
    in
        (abs x <= precision)
            && (abs y <= precision)
            && (abs z <= precision)


type alias ConvexPolyhedron =
    { faces : Array (Array Int)
    , facesLength : Int
    , vertices : Array Vec3
    , normals : Array Vec3
    , edges : List Vec3
    }


connectedFaces : Int -> Array Int -> Array (Array Int) -> Array Int
connectedFaces currentFace currentIndices allFaces =
    Array.foldl
        (\index connectedFaces ->
            allFaces
                |> Array.toIndexedList
                |> List.filter
                    (\( testFace, testIndices ) ->
                        testFace
                            /= currentFace
                            && not (List.member testFace connectedFaces)
                            && (Array.filter ((==) index) testIndices /= Array.empty)
                    )
                |> List.head
                |> Maybe.map (\( connectedFace, _ ) -> connectedFace :: connectedFaces)
                |> Maybe.withDefault connectedFaces
        )
        []
        currentIndices
        |> List.reverse
        |> Array.fromList


fromBox : Vec3 -> ConvexPolyhedron
fromBox halfExtents =
    let
        { x, y, z } =
            Vec3.toRecord halfExtents
    in
        { faces = boxFaces
        , facesLength = 6
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
        , normals = boxFaceNormals -- faceNormals vertices faces
        , edges = boxUniqueEdges -- uniqueEdges vertices faces
        }


boxFaces : Array (Array Int)
boxFaces =
    Array.fromList
        [ Array.fromList [ 3, 2, 1, 0 ]
        , Array.fromList [ 4, 5, 6, 7 ]
        , Array.fromList [ 5, 4, 0, 1 ]
        , Array.fromList [ 2, 3, 7, 6 ]
        , Array.fromList [ 0, 4, 7, 3 ]
        , Array.fromList [ 1, 2, 6, 5 ]
        ]


boxFaceNormals : Array Vec3
boxFaceNormals =
    Array.fromList
        [ vec3 0 0 -1
        , vec3 0 0 1
        , vec3 0 -1 0
        , vec3 0 1 0
        , vec3 -1 0 0
        , vec3 1 0 0
        ]


boxUniqueEdges : List Vec3
boxUniqueEdges =
    [ vec3 -1 0 0
    , vec3 0 -1 0
    , vec3 0 0 -1
    , vec3 0 0 1
    , vec3 0 1 0
    , vec3 1 0 0
    ]


faceNormals : Array Vec3 -> Array (Array Int) -> Array Vec3
faceNormals vertices =
    Array.map
        (\indices ->
            case
                Maybe.map3
                    (,,)
                    (Maybe.andThen (\i -> Array.get i vertices) (Array.get 0 indices))
                    (Maybe.andThen (\i -> Array.get i vertices) (Array.get 1 indices))
                    (Maybe.andThen (\i -> Array.get i vertices) (Array.get 2 indices))
            of
                Just ( v1, v2, v3 ) ->
                    normal v1 v2 v3

                _ ->
                    Debug.crash "Couldn't compute normal"
        )


uniqueEdges : Array Vec3 -> Array (Array Int) -> List Vec3
uniqueEdges vertices faces =
    faces
        |> Array.foldl
            (\indices ->
                edgesInFaceHelp 0 (Array.length indices) vertices indices
            )
            Set.empty
        |> Set.toList
        |> List.map Vec3.fromTuple


edgesInFaceHelp : Int -> Int -> Array Vec3 -> Array Int -> Set ( Float, Float, Float ) -> Set ( Float, Float, Float )
edgesInFaceHelp current length vertices indices edges =
    if current == length then
        edges
    else
        case
            Maybe.map2
                edge
                (Maybe.andThen (\i -> Array.get i vertices) (Array.get current indices))
                (Maybe.andThen (\i -> Array.get i vertices) (Array.get ((current + 1) % length) indices))
        of
            Just vector ->
                edgesInFaceHelp
                    (current + 1)
                    length
                    vertices
                    indices
                    (Set.insert (Vec3.toTuple vector) edges)

            Nothing ->
                edges


normal : Vec3 -> Vec3 -> Vec3 -> Vec3
normal v0 v1 v2 =
    Vec3.cross (Vec3.sub v2 v1) (Vec3.sub v0 v1)
        |> Vec3.normalize


edge : Vec3 -> Vec3 -> Vec3
edge v1 v2 =
    Vec3.normalize (Vec3.sub v2 v1)


type alias ClipResult =
    { point : Vec3
    , normal : Vec3
    , depth : Float
    }


clipAgainstHull : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Vec3 -> Float -> Float -> List ClipResult
clipAgainstHull t1 hull1 t2 hull2 separatingNormal minDist maxDist =
    case closestFaceHelp (>) 0 t2 hull2 separatingNormal -maxNumber Nothing of
        Just { indices } ->
            clipFaceAgainstHull
                t1
                hull1
                separatingNormal
                (List.map
                    (\i ->
                        Array.get i hull2.vertices
                            |> Maybe.map (Transform.pointToWorldFrame t2)
                            -- Sorry
                            |> Maybe.withDefault zero3
                    )
                    (Array.toList indices)
                )
                minDist
                maxDist

        Nothing ->
            []


clipFaceAgainstHull : Transform -> ConvexPolyhedron -> Vec3 -> List Vec3 -> Float -> Float -> List ClipResult
clipFaceAgainstHull t1 hull1 separatingNormal worldVertsB minDist maxDist =
    case closestFaceHelp (<) 0 t1 hull1 separatingNormal maxNumber Nothing of
        Just closest ->
            let
                localPlaneEq =
                    -(Array.get 0 closest.indices
                        |> Maybe.andThen (\i -> Array.get i hull1.vertices)
                        -- Sorry:
                        |> Maybe.withDefault zero3
                        |> Vec3.dot closest.normal
                     )

                planeNormalWS =
                    Quaternion.rotate t1.quaternion closest.normal

                planeEqWS =
                    localPlaneEq - Vec3.dot planeNormalWS t1.position
            in
                connectedFaces closest.index closest.indices hull1.faces
                    |> Array.foldl
                        (\otherFaceIndex ->
                            let
                                otherFaceVertex =
                                    Array.get otherFaceIndex hull1.faces
                                        |> Maybe.andThen (Array.get 0)
                                        |> Maybe.andThen (\i -> Array.get i hull1.vertices)
                                        -- Sorry:
                                        |> Maybe.withDefault zero3

                                otherFaceNormal =
                                    Array.get otherFaceIndex hull1.normals
                                        -- Sorry:
                                        |> Maybe.withDefault zero3

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


type alias ClosestFaceResult =
    { indices : Array Int
    , normal : Vec3
    , index : Int -- index in the faces array (maybe this is enough?)
    }


closestFaceHelp : (Float -> Float -> Bool) -> Int -> Transform -> ConvexPolyhedron -> Vec3 -> Float -> Maybe ClosestFaceResult -> Maybe ClosestFaceResult
closestFaceHelp compareFunc index transform hull separatingNormal dCurrent result =
    if index == hull.facesLength then
        result
    else
        let
            faceNormal =
                hull.normals
                    |> Array.get index
                    -- Sorry:
                    |> Maybe.withDefault zero3

            faceIndices =
                hull.faces
                    |> Array.get index
                    -- Sorry:
                    |> Maybe.withDefault Array.empty

            d =
                faceNormal
                    |> Quaternion.rotate transform.quaternion
                    |> Vec3.dot separatingNormal
        in
            if compareFunc d dCurrent then
                closestFaceHelp
                    compareFunc
                    (index + 1)
                    transform
                    hull
                    separatingNormal
                    d
                    (Just
                        { indices = faceIndices
                        , normal = faceNormal
                        , index = index
                        }
                    )
            else
                closestFaceHelp
                    compareFunc
                    (index + 1)
                    transform
                    hull
                    separatingNormal
                    dCurrent
                    result


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
        -- TODO: maybe move this to the top level
        testFaceNormals : Vec4 -> List Vec3 -> { target : Vec3, dmin : Float } -> Maybe { target : Vec3, dmin : Float }
        testFaceNormals quat faceNormals context =
            case faceNormals of
                [] ->
                    Just context

                normal :: restNormals ->
                    let
                        rotatedNormal =
                            Quaternion.rotate quat normal
                    in
                        case testSepAxis t1 hull1 t2 hull2 rotatedNormal of
                            Nothing ->
                                Nothing

                            Just d ->
                                if d < context.dmin then
                                    testFaceNormals
                                        quat
                                        restNormals
                                        { dmin = d, target = rotatedNormal }
                                else
                                    testFaceNormals
                                        quat
                                        restNormals
                                        context
    in
        { dmin = maxNumber, target = zero3 }
            |> testFaceNormals t1.quaternion (Array.toList hull1.normals)
            |> Maybe.andThen (testFaceNormals t2.quaternion (Array.toList hull2.normals))
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
            zero3
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
            ( -maxNumber, maxNumber )
            vertices
            |> (\( maxVal, minVal ) ->
                    ( maxVal - add
                    , minVal - add
                    )
               )
