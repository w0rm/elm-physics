module Physics.ConvexPolyhedron exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)


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
    { faces : List (List Vec3)
    , vertices : List Vec3
    , normals : List Vec3
    , uniqueEdges : List Vec3
    }


fromBox : Vec3 -> ConvexPolyhedron
fromBox halfExtents =
    let
        { x, y, z } =
            Vec3.toRecord halfExtents

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
    in
        { faces =
            -- TODO: maybe use indices?
            [ [ v3, v2, v1, v0 ]
            , [ v4, v5, v6, v7 ]
            , [ v5, v4, v0, v1 ]
            , [ v2, v3, v7, v6 ]
            , [ v0, v4, v7, v3 ]
            , [ v1, v2, v6, v5 ]
            ]
        , vertices =
            [ v0, v1, v2, v3, v4, v5, v6, v7 ]
        , normals =
            [ normal v3 v2 v1
            , normal v4 v5 v6
            , normal v5 v4 v0
            , normal v2 v3 v7
            , normal v0 v4 v7
            , normal v1 v2 v6
            ]
        , uniqueEdges =
            [ vec3 -1 0 0
            , vec3 0 1 0
            , vec3 1 0 0
            , vec3 0 -1 0
            , vec3 0 0 1
            , vec3 0 0 -1
            ]
        }


normal : Vec3 -> Vec3 -> Vec3 -> Vec3
normal v0 v1 v2 =
    Vec3.cross (Vec3.sub v2 v1) (Vec3.sub v1 v0)
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
    case closestFaceBHelp t2 separatingNormal hull2.normals hull2.faces -maxNumber Nothing of
        Just { vertices } ->
            clipFaceAgainstHull
                t1
                hull1
                separatingNormal
                (List.map (Quaternion.rotate t1.quaternion >> Vec3.add t1.position) vertices)
                minDist
                maxDist

        Nothing ->
            []


closestFaceBHelp : Transform -> Vec3 -> List Vec3 -> List (List Vec3) -> Float -> Maybe ClosestFaceResult -> Maybe ClosestFaceResult
closestFaceBHelp transform separatingNormal normals faces dmax result =
    case ( normals, faces ) of
        ( normal :: restNormals, face :: restFaces ) ->
            let
                d =
                    Vec3.dot (Quaternion.rotate transform.quaternion normal) separatingNormal
            in
                if d > dmax then
                    closestFaceBHelp transform
                        separatingNormal
                        restNormals
                        restFaces
                        d
                        (Just
                            { vertices = face
                            , normal = normal
                            }
                        )
                else
                    closestFaceBHelp transform separatingNormal restNormals restFaces dmax result

        _ ->
            result


clipFaceAgainstHull : Transform -> ConvexPolyhedron -> Vec3 -> List Vec3 -> Float -> Float -> List ClipResult
clipFaceAgainstHull t1 hull1 separatingNormal worldVertsB1 minDist maxDist =
    let
        clipFaceAgainstHullHelpAdd closest a b result =
            let
                worldEdge =
                    Vec3.sub a b
                        |> Quaternion.rotate t1.quaternion
                        |> Vec3.add t1.position

                planeNormalWS =
                    closest.normal
                        |> Quaternion.rotate t1.quaternion
                        |> Vec3.add t1.position
                        |> Vec3.cross worldEdge
                        |> Vec3.negate

                worldA =
                    a
                        |> Quaternion.rotate t1.quaternion
                        |> Vec3.add t1.position

                planeEqWS =
                    -(Vec3.dot worldA planeNormalWS)
            in
                clipFaceAgainstPlane planeNormalWS planeEqWS result

        clipFaceAgainstHullHelp closest first vertices result =
            case vertices of
                [] ->
                    result

                fst :: snd :: remaining ->
                    clipFaceAgainstHullHelp closest
                        first
                        (snd :: remaining)
                        (clipFaceAgainstHullHelpAdd closest fst snd result)

                last :: [] ->
                    clipFaceAgainstHullHelp closest
                        first
                        []
                        (clipFaceAgainstHullHelpAdd closest last first result)

        clipFaceAgainstHullHelpStart closest =
            case closest.vertices of
                -- guarantee at least two
                fst :: snd :: rest ->
                    clipFaceAgainstHullHelp closest fst closest.vertices worldVertsB1

                _ ->
                    []
    in
        case closestFaceAHelp t1 separatingNormal hull1.normals hull1.faces maxNumber Nothing of
            Just closest ->
                let
                    localPlaneEq =
                        -(List.head closest.vertices
                            |> Maybe.withDefault zero3
                            |> Vec3.dot closest.normal
                         )

                    planeNormalWS =
                        closest.normal
                            |> Quaternion.rotate t1.quaternion

                    planeEqWS =
                        localPlaneEq - Vec3.dot planeNormalWS t1.position
                in
                    clipFaceAgainstHullHelpStart closest
                        |> List.foldl
                            (\point result ->
                                let
                                    depth =
                                        max minDist (Vec3.dot planeNormalWS point)
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
    { vertices : List Vec3
    , normal : Vec3
    }


closestFaceAHelp : Transform -> Vec3 -> List Vec3 -> List (List Vec3) -> Float -> Maybe ClosestFaceResult -> Maybe ClosestFaceResult
closestFaceAHelp transform separatingNormal normals faces dmin result =
    case ( normals, faces ) of
        ( normal :: restNormals, face :: restFaces ) ->
            let
                d =
                    Vec3.dot (Quaternion.rotate transform.quaternion normal) separatingNormal
            in
                if d < dmin then
                    closestFaceAHelp transform
                        separatingNormal
                        restNormals
                        restFaces
                        d
                        (Just
                            { vertices = face
                            , normal = normal
                            }
                        )
                else
                    closestFaceAHelp transform separatingNormal restNormals restFaces dmin result

        _ ->
            result


clipFaceAgainstPlane : Vec3 -> Float -> List Vec3 -> List Vec3
clipFaceAgainstPlane planeNormal planeConstant vertices =
    case vertices of
        -- guarantee at least two
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
                (clipFaceAgainstPlaneAdd
                    planeNormal
                    planeConstant
                    fst
                    snd
                    result
                )

        last :: [] ->
            clipFaceAgainstPlaneHelp
                planeNormal
                planeConstant
                first
                []
                (clipFaceAgainstPlaneAdd
                    planeNormal
                    planeConstant
                    last
                    first
                    result
                )


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
            (lerp (nDotPrev / (nDotPrev - nDotNext)) prev next)
                :: next
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
                    case testSepAxis t1 hull1 t2 hull2 (Quaternion.rotate quat normal) of
                        Nothing ->
                            Nothing

                        Just d ->
                            if d < context.dmin then
                                testFaceNormals
                                    quat
                                    restNormals
                                    { dmin = d, target = normal }
                            else
                                testFaceNormals
                                    quat
                                    restNormals
                                    context
    in
        { dmin = maxNumber, target = zero3 }
            |> testFaceNormals t1.quaternion hull1.normals
            |> Maybe.andThen (testFaceNormals t2.quaternion hull2.normals)
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
                    hull2.uniqueEdges
        )
        (Just context)
        hull1.uniqueEdges


testSepAxis : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> Vec3 -> Maybe Float
testSepAxis t1 hull1 t2 hull2 axis =
    let
        ( min1, max1 ) =
            project t1 hull1 axis

        ( min2, max2 ) =
            project t2 hull2 axis
    in
        if max1 < min2 || max2 < min1 then
            Nothing
        else
            Just (min (max1 - min2) (max2 - min1))


{-| Get min and max dot product of a convex hull at Transform projected onto an axis.
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
        List.foldl
            (\vec ( minVal, maxVal ) ->
                let
                    val =
                        Vec3.dot vec localAxis
                in
                    ( min minVal val, max maxVal val )
            )
            ( maxNumber, -maxNumber )
            vertices
            |> (\( minVal, maxVal ) ->
                    ( minVal - add
                    , maxVal - add
                    )
               )
