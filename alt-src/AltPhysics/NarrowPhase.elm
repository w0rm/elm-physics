module AltPhysics.NarrowPhase exposing
    ( addSphereConvexContacts
    ,  getContacts
       -- exposed only for tests

    )

import AltMath.Vector3 as Vec3 exposing (Vec3)
import AltPhysics.Body as Body exposing (Body, BodyId)
import AltPhysics.Const as Const
import AltPhysics.ContactEquation as ContactEquation exposing (ContactEquation)
import AltPhysics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import AltPhysics.Quaternion as Quaternion
import AltPhysics.Shape as Shape exposing (Shape(..))
import AltPhysics.Transform as Transform exposing (Transform)
import AltPhysics.World as World exposing (World)
import Array exposing (Array)
import Dict exposing (Dict)
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
        (\shapeId1 shape1 currentContactEquations1 ->
            Dict.foldl
                (\shapeId2 shape2 currentContactEquations2 ->
                    getShapeContacts
                        (Body.shapeWorldTransform shapeId1 body1)
                        shape1
                        bodyId1
                        body1
                        (Body.shapeWorldTransform shapeId2 body2)
                        shape2
                        bodyId2
                        body2
                        currentContactEquations2
                )
                currentContactEquations1
                body2.shapes
        )
        contactEquations
        body1.shapes


getShapeContacts : Transform -> Shape -> BodyId -> Body -> Transform -> Shape -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getShapeContacts shapeTransform1 shape1 bodyId1 body1 shapeTransform2 shape2 bodyId2 body2 contactEquations =
    case ( shape1, shape2 ) of
        ( Plane, Plane ) ->
            -- don't collide two planes
            contactEquations

        ( Plane, Convex convexPolyhedron ) ->
            addPlaneConvexContacts
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                convexPolyhedron
                bodyId2
                body2
                contactEquations

        ( Plane, Sphere radius ) ->
            addPlaneSphereContacts
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                radius
                bodyId2
                body2
                contactEquations

        ( Convex convexPolyhedron, Plane ) ->
            addPlaneConvexContacts
                shapeTransform2
                bodyId2
                body2
                shapeTransform1
                convexPolyhedron
                bodyId1
                body1
                contactEquations

        ( Convex convexPolyhedron1, Convex convexPolyhedron2 ) ->
            addConvexConvexContacts
                shapeTransform1
                convexPolyhedron1
                bodyId1
                body1
                shapeTransform2
                convexPolyhedron2
                bodyId2
                body2
                contactEquations

        ( Convex convexPolyhedron, Sphere radius ) ->
            addSphereConvexContacts
                shapeTransform2
                radius
                bodyId2
                shapeTransform1
                convexPolyhedron
                bodyId1
                contactEquations

        ( Sphere radius, Plane ) ->
            addPlaneSphereContacts
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                radius
                bodyId2
                body2
                contactEquations

        ( Sphere radius, Convex convexPolyhedron ) ->
            addSphereConvexContacts
                shapeTransform1
                radius
                bodyId1
                shapeTransform2
                convexPolyhedron
                bodyId2
                contactEquations

        ( Sphere radius1, Sphere radius2 ) ->
            addSphereSphereContacts
                shapeTransform1
                radius1
                bodyId1
                body1
                shapeTransform2
                radius2
                bodyId2
                body2
                contactEquations


addPlaneConvexContacts : Transform -> BodyId -> Body -> Transform -> ConvexPolyhedron -> BodyId -> Body -> List ContactEquation -> List ContactEquation
addPlaneConvexContacts planeTransform planeBodyId planeBody convexTransform convexPolyhedron convexBodyId convexBody contactEquations =
    let
        worldNormal =
            Quaternion.rotate planeTransform.quaternion Vec3.k
    in
    Array.foldl
        (\vertex currentContactEquations ->
            let
                worldVertex =
                    Transform.pointToWorldFrame convexTransform vertex

                dot =
                    planeTransform.position
                        |> Vec3.sub worldVertex
                        |> Vec3.dot worldNormal
            in
            if dot <= 0 then
                { bodyId1 = planeBodyId
                , bodyId2 = convexBodyId
                , ni = worldNormal
                , ri =
                    Vec3.sub
                        (worldNormal
                            |> Vec3.scale dot
                            |> Vec3.sub worldVertex
                        )
                        planeBody.position
                , rj = Vec3.sub worldVertex convexBody.position
                , restitution = 0
                }
                    :: currentContactEquations

            else
                currentContactEquations
        )
        contactEquations
        convexPolyhedron.vertices


addConvexConvexContacts : Transform -> ConvexPolyhedron -> BodyId -> Body -> Transform -> ConvexPolyhedron -> BodyId -> Body -> List ContactEquation -> List ContactEquation
addConvexConvexContacts shapeTransform1 convexPolyhedron1 bodyId1 body1 shapeTransform2 convexPolyhedron2 bodyId2 body2 contactEquations =
    case ConvexPolyhedron.findSeparatingAxis shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 of
        Just sepAxis ->
            ConvexPolyhedron.clipAgainstHull shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 sepAxis -100 100
                |> List.foldl
                    (\{ point, normal, depth } currentContactEquations ->
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
                        { bodyId1 = bodyId1
                        , bodyId2 = bodyId2
                        , ni = Vec3.negate sepAxis
                        , ri = ri
                        , rj = rj
                        , restitution = 0
                        }
                            :: currentContactEquations
                    )
                    contactEquations

        Nothing ->
            contactEquations


addPlaneSphereContacts : Transform -> BodyId -> Body -> Transform -> Float -> BodyId -> Body -> List ContactEquation -> List ContactEquation
addPlaneSphereContacts planeTransform bodyId1 body1 t2 radius bodyId2 body2 contactEquations =
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
            :: contactEquations

    else
        contactEquations


addSphereConvexContacts : Transform -> Float -> BodyId -> Transform -> ConvexPolyhedron -> BodyId -> List ContactEquation -> List ContactEquation
addSphereConvexContacts { position } radius bodyId1 t2 hull2 bodyId2 contactEquations =
    let
        ( maybeWorldContact, penetration ) =
            ConvexPolyhedron.sphereContact position radius t2 hull2
    in
    case maybeWorldContact of
        Just worldContact2 ->
            let
                worldNormal =
                    Vec3.direction worldContact2 position
            in
            { bodyId1 = bodyId1
            , bodyId2 = bodyId2
            , ni = worldNormal
            , ri = Vec3.scale radius worldNormal
            , rj = Vec3.sub worldContact2 t2.position
            , restitution = 0
            }
                :: contactEquations

        Nothing ->
            contactEquations


addSphereSphereContacts : Transform -> Float -> BodyId -> Body -> Transform -> Float -> BodyId -> Body -> List ContactEquation -> List ContactEquation
addSphereSphereContacts t1 radius1 bodyId1 body1 t2 radius2 bodyId2 body2 contactEquations =
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
        contactEquations

    else
        { bodyId1 = bodyId1
        , bodyId2 = bodyId2
        , ni = normal
        , ri = Vec3.scale radius1 normal
        , rj = Vec3.scale -radius2 normal
        , restitution = 0
        }
            :: contactEquations
