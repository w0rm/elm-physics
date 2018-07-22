module Physics.NarrowPhase exposing (getContacts)

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Body as Body exposing (Body, BodyId)
import Physics.Const as Const
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Physics.Quaternion as Quaternion
import Physics.Shape as Shape exposing (Shape(..))
import Physics.Transform as Transform exposing (Transform)
import Physics.World as World exposing (World)
import Set exposing (Set)


getContacts : World -> List ContactEquation
getContacts world =
    Set.foldl
        (\( bodyId1, bodyId2 ) ->
            Maybe.map2
                (getBodyContacts world bodyId1 bodyId2)
                (Dict.get bodyId1 world.bodies)
                (Dict.get bodyId2 world.bodies)
                |> Maybe.withDefault identity
        )
        []
        (World.getPairs world)


getBodyContacts : World -> BodyId -> BodyId -> Body -> Body -> List ContactEquation -> List ContactEquation
getBodyContacts world bodyId1 bodyId2 body1 body2 contactEquations =
    Dict.foldl
        (\shapeId1 shape1 acc1 ->
            Dict.foldl
                (\shapeId2 shape2 acc2 ->
                    getShapeContacts
                        (Body.shapeWorldTransform shapeId1 body1)
                        shape1
                        bodyId1
                        body1
                        (Body.shapeWorldTransform shapeId2 body2)
                        shape2
                        bodyId2
                        body2
                        acc2
                )
                acc1
                body2.shapes
        )
        contactEquations
        body1.shapes


getShapeContacts : Transform -> Shape -> BodyId -> Body -> Transform -> Shape -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getShapeContacts shapeTransform1 shape1 bodyId1 body1 shapeTransform2 shape2 bodyId2 body2 =
    case ( shape1, shape2 ) of
        ( Plane, Plane ) ->
            -- don't collide two planes
            identity

        ( Plane, Convex convexPolyhedron ) ->
            getPlaneConvexContacts
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                convexPolyhedron
                bodyId2
                body2

        ( Plane, Sphere radius ) ->
            foldPlaneSphereContacts
                addContact
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                radius
                bodyId2
                body2

        ( Convex convexPolyhedron, Plane ) ->
            getPlaneConvexContacts
                shapeTransform2
                bodyId2
                body2
                shapeTransform1
                convexPolyhedron
                bodyId1
                body1

        ( Convex convexPolyhedron1, Convex convexPolyhedron2 ) ->
            getConvexConvexContacts
                shapeTransform1
                convexPolyhedron1
                bodyId1
                body1
                shapeTransform2
                convexPolyhedron2
                bodyId2
                body2

        ( Convex convexPolyhedron, Sphere radius ) ->
            foldSphereConvexContacts
                addContact
                shapeTransform2
                radius
                bodyId2
                body2
                shapeTransform1
                convexPolyhedron
                bodyId1
                body1

        ( Sphere radius, Plane ) ->
            foldPlaneSphereContacts
                addContact
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                radius
                bodyId2
                body2

        ( Sphere radius, Convex convexPolyhedron ) ->
            foldSphereConvexContacts
                addContact
                shapeTransform2
                radius
                bodyId2
                body2
                shapeTransform1
                convexPolyhedron
                bodyId1
                body1

        ( Sphere radius1, Sphere radius2 ) ->
            foldSphereSphereContacts
                addContact
                shapeTransform1
                radius1
                bodyId1
                body1
                shapeTransform2
                radius2
                bodyId2
                body2


addContact : ContactEquation -> a -> a -> List ContactEquation -> List ContactEquation
addContact contactEquation _ _ acc =
    contactEquation :: acc


getPlaneConvexContacts : Transform -> BodyId -> Body -> Transform -> ConvexPolyhedron -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getPlaneConvexContacts planeTransform planeBodyId planeBody convexTransform convexPolyhedron convexBodyId convexBody contactEquations =
    let
        worldNormal =
            Quaternion.rotate planeTransform.quaternion Vec3.k
    in
        Array.foldl
            (\vertex ->
                let
                    worldVertex =
                        Transform.pointToWorldFrame convexTransform vertex

                    dot =
                        planeTransform.position
                            |> Vec3.sub worldVertex
                            |> Vec3.dot worldNormal
                in
                    if dot <= 0 then
                        (::)
                            { bodyId1 = planeBodyId
                            , bodyId2 = convexBodyId
                            , ni = worldNormal
                            , ri =
                                worldVertex
                                    |> Vec3.add (Vec3.negate (Vec3.scale dot worldNormal))
                                    |> Vec3.add (Vec3.negate planeBody.position)
                            , rj = Vec3.sub worldVertex convexBody.position
                            , restitution = 0
                            }
                    else
                        identity
            )
            contactEquations
            convexPolyhedron.vertices


getConvexConvexContacts : Transform -> ConvexPolyhedron -> BodyId -> Body -> Transform -> ConvexPolyhedron -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getConvexConvexContacts shapeTransform1 convexPolyhedron1 bodyId1 body1 shapeTransform2 convexPolyhedron2 bodyId2 body2 contactEquations =
    case ConvexPolyhedron.findSeparatingAxis shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 of
        Just sepAxis ->
            ConvexPolyhedron.clipAgainstHull shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 sepAxis -100 100
                |> List.foldl
                    (\{ point, normal, depth } ->
                        let
                            q =
                                normal
                                    |> Vec3.negate
                                    |> Vec3.scale depth

                            ri =
                                Vec3.add point q
                                    |> Vec3.add (Vec3.negate body1.position)

                            rj =
                                point
                                    |> Vec3.add (Vec3.negate body2.position)
                        in
                            (::)
                                { bodyId1 = bodyId1
                                , bodyId2 = bodyId2
                                , ni = Vec3.negate sepAxis
                                , ri = ri
                                , rj = rj
                                , restitution = 0
                                }
                    )
                    contactEquations

        Nothing ->
            contactEquations


foldPlaneSphereContacts : (ContactEquation -> Body -> Body -> a -> a) -> Transform -> BodyId -> Body -> Transform -> Float -> BodyId -> Body -> a -> a
foldPlaneSphereContacts fn planeTransform bodyId1 body1 t2 radius bodyId2 body2 seed =
    let
        worldPlaneNormal =
            Quaternion.rotate planeTransform.quaternion Vec3.k

        worldVertex =
            worldPlaneNormal
                |> Vec3.scale radius
                |> Vec3.sub t2.position

        dot =
            planeTransform.position
                |> Vec3.sub worldVertex
                |> Vec3.dot worldPlaneNormal
    in
        if dot <= 0 then
            fn
                { bodyId1 = bodyId1
                , bodyId2 = bodyId2
                , ni = worldPlaneNormal
                , ri =
                    Vec3.sub
                        (worldPlaneNormal
                            |> Vec3.scale dot
                            |> Vec3.sub worldVertex
                        )
                        body1.position
                , rj = Vec3.sub worldVertex body2.position
                , restitution = 0
                }
                body1
                body2
                seed
        else
            seed


foldSphereConvexContacts : (ContactEquation -> Body -> Body -> a -> a) -> Transform -> Float -> BodyId -> Body -> Transform -> ConvexPolyhedron -> BodyId -> Body -> a -> a
foldSphereConvexContacts fn t1 radius bodyId1 body1 t2 { vertices, faces, normals } bodyId2 body2 seed =
    let
        contactEq : Vec3 -> Float -> Vec3 -> ContactEquation
        contactEq vectorToContact radius worldContact2 =
            let
                sphereNormal =
                    Vec3.normalize vectorToContact
            in
                { bodyId1 = bodyId1
                , bodyId2 = bodyId2
                , ni = sphereNormal
                , ri = Vec3.scale radius sphereNormal
                , rj = Vec3.sub worldContact2 t2.position
                , restitution = 0
                }

        center =
            t1.position
    in
        -- Check corners
        vertices
            |> arrayFoldWhileNothing
                (\vertex ->
                    let
                        -- World position of corner
                        worldCorner =
                            Transform.pointToWorldFrame t2 vertex

                        centerToCorner =
                            Vec3.sub worldCorner center
                    in
                        if Vec3.lengthSquared centerToCorner > radius * radius then
                            Nothing
                        else
                            contactEq centerToCorner radius worldCorner
                                |> Just
                )
                Nothing
            |> (\oneFound ->
                    case oneFound of
                        Just _ ->
                            oneFound

                        Nothing ->
                            faces
                                |> arrayIndexedFoldWhileNothing
                                    (\face index ->
                                        Array.get index normals
                                            |> Maybe.andThen
                                                (\normal ->
                                                    foldSphereFaceContact contactEq center radius t2 face vertices normal
                                                )
                                    )
                                    Nothing
               )
            |> (\oneFound ->
                    case oneFound of
                        Just found ->
                            fn found body1 body2 seed

                        Nothing ->
                            seed
               )


foldSphereFaceContact : (Vec3 -> Float -> Vec3 -> ContactEquation) -> Vec3 -> Float -> Transform -> List Int -> Array Vec3 -> Vec3 -> Maybe ContactEquation
foldSphereFaceContact contactEqFn center radius t2 face vertices normal =
    let
        -- Get world-transformed normal of the face
        worldFacePlaneNormal =
            Quaternion.rotate t2.quaternion normal

        -- Get an arbitrary world vertex from the face
        worldPoint =
            List.head face
                |> Maybe.andThen (\i -> Array.get i vertices)
                |> Maybe.map
                    (Transform.pointToWorldFrame t2)

        vectorToContact =
            Vec3.scale (-radius) worldFacePlaneNormal

        penetration =
            worldPoint
                |> Maybe.map
                    (\point ->
                        Vec3.dot
                            (Vec3.sub (Vec3.add center vectorToContact) point)
                            worldFacePlaneNormal
                    )
                |> Maybe.withDefault 1

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
            if penetration < 0 && dot > 0 then
                -- Sphere intersects the face plane.
                face
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
                    vectorToContact
                        |> Vec3.scale ((radius + penetration) / radius)
                        |> Vec3.add center
            in
                contactEqFn vectorToContact radius worldContact
                    |> Just
        else
            foldSphereEdgeContact
                contactEqFn
                center
                radius
                (face
                    |> List.map
                        (\index ->
                            Array.get index vertices
                                |> Maybe.map
                                    (Transform.pointToWorldFrame t2)
                        )
                )


foldSphereEdgeContact : (Vec3 -> Float -> Vec3 -> ContactEquation) -> Vec3 -> Float -> List (Maybe Vec3) -> Maybe ContactEquation
foldSphereEdgeContact contactEqFn center radius worldVertices =
    worldVertices
        |> listRingFoldStaggeredPairs
            (\current prev oneFound ->
                case ( current, prev, oneFound ) of
                    ( _, _, Just _ ) ->
                        oneFound

                    ( Just vertex, Just prevVertex, Nothing ) ->
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

                                    -- The vector from the center to its
                                    -- projection.
                                    centerToContact =
                                        Vec3.sub worldContact center

                                    distanceSquared =
                                        Vec3.lengthSquared centerToContact
                                in
                                    -- Edge collision only occurs if the
                                    -- projection is within the sphere.
                                    if distanceSquared < radius * radius then
                                        contactEqFn centerToContact radius worldContact
                                            |> Just
                                    else
                                        Nothing
                            else
                                Nothing

                    _ ->
                        Nothing
            )
            Nothing


{-| Map the function to pairs of consecutive elements in the ring array,
starting with the pair (first, last), then (second, first), and so on.
-}
listRingFoldStaggeredPairs : (a -> a -> b -> b) -> b -> List a -> b
listRingFoldStaggeredPairs fn acc list =
    case
        List.drop (List.length list - 1) list
            |> List.head
    of
        Nothing ->
            acc

        Just last ->
            listFoldStaggeredPairs fn last acc list


{-| Map the function to pairs of consecutive elements in the array,
starting with the pair (first, seed), then (second, first), and so on.
-}
listFoldStaggeredPairs : (a -> a -> b -> b) -> a -> b -> List a -> b
listFoldStaggeredPairs fn seed acc list =
    list
        |> List.foldl
            (\current ( acc1, staggered1 ) ->
                case staggered1 of
                    prev :: tail ->
                        ( fn current prev acc1
                        , tail
                        )

                    _ ->
                        -- impossible
                        ( acc1
                        , []
                        )
            )
            ( acc, seed :: list )
        |> Tuple.first


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


foldSphereSphereContacts : (ContactEquation -> Body -> Body -> a -> a) -> Transform -> Float -> BodyId -> Body -> Transform -> Float -> BodyId -> Body -> a -> a
foldSphereSphereContacts fn t1 radius1 bodyId1 body1 t2 radius2 bodyId2 body2 seed =
    let
        center1 =
            Transform.pointToWorldFrame t1 Const.zero3

        center2 =
            Transform.pointToWorldFrame t2 Const.zero3

        distance =
            Vec3.distance center2 center1
                - radius1
                - radius2

        normal =
            Vec3.direction center2 center1
    in
        if distance > 0 then
            seed
        else
            fn
                { bodyId1 = bodyId1
                , bodyId2 = bodyId2
                , ni = normal
                , ri = Vec3.scale radius1 normal
                , rj = Vec3.scale -radius2 normal
                , restitution = 0
                }
                body1
                body2
                seed


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


arrayIndexedFoldWhileNothing : (a -> Int -> Maybe b) -> Maybe b -> Array a -> Maybe b
arrayIndexedFoldWhileNothing fn seed array =
    array
        |> Array.foldl
            (\element ( acc, index ) ->
                ( case acc of
                    Nothing ->
                        fn element index

                    _ ->
                        acc
                , index + 1
                )
            )
            ( seed, 0 )
        |> Tuple.first
