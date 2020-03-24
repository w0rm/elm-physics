module Collision.ConvexConvex exposing
    ( addContacts
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Convex as Convex exposing (AdjacentFace, Convex, Face)
import Internal.Vector3 as Vec3 exposing (Vec3)


minDist : Float
minDist =
    -100


maxDist : Float
maxDist =
    100


addContacts : Convex -> Convex -> List Contact -> List Contact
addContacts convex1 convex2 contacts =
    case findSeparatingAxis convex1 convex2 of
        Just separatingAxis ->
            let
                reversedSeparatingAxis =
                    Vec3.negate separatingAxis
            in
            case bestFace convex1.faces separatingAxis of
                Just face1 ->
                    case bestFace convex2.faces reversedSeparatingAxis of
                        Just face2 ->
                            clipTwoFaces face1 face2 reversedSeparatingAxis contacts

                        Nothing ->
                            contacts

                Nothing ->
                    contacts

        Nothing ->
            contacts


clipTwoFaces : Face -> Face -> Vec3 -> List Contact -> List Contact
clipTwoFaces { point, normal, adjacentFaces } { vertices } separatingAxis contacts =
    let
        worldPlaneConstant =
            -(Vec3.dot normal point)
    in
    List.foldl
        (\vertex result ->
            let
                depth =
                    max minDist (Vec3.dot normal vertex + worldPlaneConstant)
            in
            if depth <= maxDist && depth <= 0 then
                { ni = separatingAxis
                , pi = Vec3.sub vertex (Vec3.scale depth normal)
                , pj = vertex
                }
                    :: result

            else
                result
        )
        contacts
        (clipAgainstAdjacentFaces adjacentFaces vertices)


bestFace : List Face -> Vec3 -> Maybe Face
bestFace faces separatingAxis =
    case faces of
        face :: restFaces ->
            Just
                (bestFaceHelp
                    separatingAxis
                    restFaces
                    face
                    (Vec3.dot face.normal separatingAxis)
                )

        [] ->
            Nothing


bestFaceHelp : Vec3 -> List Face -> Face -> Float -> Face
bestFaceHelp separatingAxis faces currentBestFace currentBestDistance =
    case faces of
        face :: remainingFaces ->
            let
                faceDistance =
                    Vec3.dot face.normal separatingAxis
            in
            if currentBestDistance - faceDistance > 0 then
                bestFaceHelp
                    separatingAxis
                    remainingFaces
                    face
                    faceDistance

            else
                bestFaceHelp
                    separatingAxis
                    remainingFaces
                    currentBestFace
                    currentBestDistance

        [] ->
            currentBestFace


clipAgainstAdjacentFaces : List AdjacentFace -> List Vec3 -> List Vec3
clipAgainstAdjacentFaces adjacentFaces worldVertices =
    case adjacentFaces of
        { point, normal } :: remainingFaces ->
            let
                worldPlaneConstant =
                    -(Vec3.dot point normal)

                vertices =
                    Convex.foldFaceEdges
                        (clipFaceAgainstPlaneAdd normal worldPlaneConstant)
                        []
                        worldVertices
            in
            clipAgainstAdjacentFaces remainingFaces vertices

        [] ->
            worldVertices


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


findSeparatingAxis : Convex -> Convex -> Maybe Vec3
findSeparatingAxis convex1 convex2 =
    let
        -- group to reduce the number of arguments
        ctx =
            { convex1 = convex1
            , convex2 = convex2
            }

        -- normals from both convexes converted in the world coordinates
        worldNormals =
            convex1.uniqueNormals ++ convex2.uniqueNormals
    in
    case testUniqueNormals ctx worldNormals Vec3.zero Const.maxNumber of
        Just { target, dmin } ->
            let
                worldEdges1 =
                    ctx.convex1.uniqueEdges

                worldEdges2 =
                    ctx.convex2.uniqueEdges
            in
            testUniqueEdges ctx worldEdges2 worldEdges1 worldEdges2 target dmin

        _ ->
            Nothing


type alias TestContext =
    { convex1 : Convex
    , convex2 : Convex
    }


testUniqueNormals : TestContext -> List Vec3 -> Vec3 -> Float -> Maybe { target : Vec3, dmin : Float }
testUniqueNormals ctx normals target dmin =
    case normals of
        [] ->
            Just { target = target, dmin = dmin }

        normal :: restNormals ->
            case testSeparatingAxis ctx normal of
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
            if Vec3.dot (Vec3.sub ctx.convex2.position ctx.convex1.position) target > 0 then
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
                        case testSeparatingAxis ctx normalizedCross of
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


{-| If projections of two convexes don’t overlap, then they don’t collide.
-}
testSeparatingAxis : TestContext -> Vec3 -> Maybe Float
testSeparatingAxis { convex1, convex2 } separatingAxis =
    let
        p1 =
            project convex1.vertices separatingAxis

        p2 =
            project convex2.vertices separatingAxis

        d1 =
            p1.max - p2.min

        d2 =
            p2.max - p1.min
    in
    if d1 < 0 || d2 < 0 then
        Nothing

    else if d1 - d2 > 0 then
        Just d2

    else
        Just d1


{-| Get max and min dot product of a convex hull at ShapeWorldTransform3d projected onto an axis.
-}
project : List Vec3 -> Vec3 -> { min : Float, max : Float }
project vertices axis =
    projectHelp axis Const.maxNumber -Const.maxNumber vertices


projectHelp : Vec3 -> Float -> Float -> List Vec3 -> { min : Float, max : Float }
projectHelp localAxis minVal maxVal currentVertices =
    case currentVertices of
        [] ->
            { min = minVal, max = maxVal }

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
                (if minVal - val > 0 then
                    val

                 else
                    minVal
                )
                (if maxVal - val > 0 then
                    maxVal

                 else
                    val
                )
                remainingVertices
