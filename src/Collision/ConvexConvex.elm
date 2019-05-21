module Collision.ConvexConvex exposing
    ( addContacts
    , clipAgainstHull
    , clipFaceAgainstHull
    , clipFaceAgainstPlane
    , findSeparatingAxis
    , project
    , testSepAxis
    )

import Internal.Const as Const
import Internal.Contact as Contact exposing (Contact)
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron, Face)
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> List Contact -> List Contact
addContacts shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 contacts =
    case findSeparatingAxis shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 of
        Just sepAxis ->
            clipAgainstHull shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 sepAxis -100 100
                |> List.foldl
                    (\{ point, normal, depth } currentContacts ->
                        { ni = Vec3.negate sepAxis
                        , pi = Vec3.sub point (Vec3.scale depth normal)
                        , pj = point
                        }
                            :: currentContacts
                    )
                    contacts

        Nothing ->
            contacts


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
        Just { point, normal, adjacentFaces } ->
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
    ConvexPolyhedron.foldFaceEdges
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
            { t1 = t1, hull1 = hull1, t2 = t2, hull2 = hull2 }

        worldNormals =
            List.foldl (Quaternion.rotate t1.orientation >> (::))
                (List.foldl (Quaternion.rotate t2.orientation >> (::)) [] hull2.uniqueNormals)
                hull1.uniqueNormals
    in
    case testUniqueNormals ctx worldNormals Vec3.zero Const.maxNumber of
        Nothing ->
            Nothing

        Just { target, dmin } ->
            let
                worldEdges1 =
                    List.foldl (Quaternion.rotate ctx.t1.orientation >> (::)) [] ctx.hull1.uniqueEdges

                worldEdges2 =
                    List.foldl (Quaternion.rotate ctx.t2.orientation >> (::)) [] ctx.hull2.uniqueEdges
            in
            testUniqueEdges ctx worldEdges2 worldEdges1 worldEdges2 target dmin


type alias TestContext =
    { t1 : Transform
    , hull1 : ConvexPolyhedron
    , t2 : Transform
    , hull2 : ConvexPolyhedron
    }


testUniqueNormals : TestContext -> List Vec3 -> Vec3 -> Float -> Maybe { target : Vec3, dmin : Float }
testUniqueNormals ctx normals target dmin =
    case normals of
        [] ->
            Just { target = target, dmin = dmin }

        normal :: restNormals ->
            case testSepAxis ctx normal of
                Nothing ->
                    Nothing

                Just d ->
                    if d - dmin < 0 then
                        testUniqueNormals ctx restNormals normal d

                    else
                        testUniqueNormals ctx restNormals target dmin


testUniqueEdges : TestContext -> List Vec3 -> List Vec3 -> List Vec3 -> Vec3 -> Float -> Maybe Vec3
testUniqueEdges ctx initEdges2 edges1 edges2 target dmin =
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
                    testUniqueEdges ctx initEdges2 remainingEdges1 initEdges2 target dmin

                worldEdge2 :: remainingEdges2 ->
                    let
                        cross =
                            Vec3.cross worldEdge1 worldEdge2
                    in
                    if Vec3.almostZero cross then
                        -- continue because edges are parallel
                        testUniqueEdges ctx initEdges2 edges1 remainingEdges2 target dmin

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
                                    testUniqueEdges ctx initEdges2 edges1 remainingEdges2 normalizedCross dist

                                else
                                    -- continue
                                    testUniqueEdges ctx initEdges2 edges1 remainingEdges2 target dmin


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
