module Physics.NarrowPhase exposing (getContacts)

import Physics.World as World exposing (..)
import Physics.Body as Body exposing (..)
import Physics.Shape as Shape exposing (..)
import Physics.ConvexPolyhedron as ConvexPolyhedron
import Physics.Quaternion as Quaternion
import Set exposing (Set)
import Math.Vector3 as Vec3 exposing (Vec3)
import Dict exposing (Dict)
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.Transform as Transform exposing (Transform)


getContacts : World -> List ContactEquation
getContacts world =
    Set.foldl
        (\( bodyId1, bodyId2 ) ->
            Maybe.map2
                (\body1 body2 ->
                    getBodyContacts world bodyId1 body1 bodyId2 body2
                )
                (Dict.get bodyId1 world.bodies)
                (Dict.get bodyId2 world.bodies)
                |> Maybe.withDefault identity
        )
        []
        (World.getPairs world)


getBodyContacts : World -> BodyId -> Body -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getBodyContacts world bodyId1 body1 bodyId2 body2 contactEquations =
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

        ( Plane, Box halfExtents ) ->
            getPlaneBoxContacts
                shapeTransform1
                bodyId1
                body1
                shapeTransform2
                halfExtents
                bodyId2
                body2

        ( Box halfExtents, Plane ) ->
            getPlaneBoxContacts
                shapeTransform2
                bodyId2
                body2
                shapeTransform1
                halfExtents
                bodyId1
                body1

        ( Box halfExtents1, Box halfExtents2 ) ->
            getBoxBoxContacts
                shapeTransform1
                halfExtents1
                bodyId1
                body1
                shapeTransform2
                halfExtents2
                bodyId2
                body2


getPlaneBoxContacts : Transform -> BodyId -> Body -> Transform -> Vec3 -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getPlaneBoxContacts planeTransform planeBodyId planeBody boxTransform boxHalfExtents boxBodyId boxBody contactEquations =
    let
        worldNormal =
            Quaternion.rotate planeTransform.quaternion Vec3.k

        convexPolyhedron =
            ConvexPolyhedron.fromBox boxHalfExtents
    in
        List.foldl
            (\vertex ->
                let
                    worldVertex =
                        Transform.pointToWorldFrame boxTransform vertex

                    dot =
                        planeTransform.position
                            |> Vec3.sub worldVertex
                            |> Vec3.dot worldNormal
                in
                    if dot <= 0 then
                        (::)
                            { bodyId1 = planeBodyId
                            , bodyId2 = boxBodyId
                            , ni = worldNormal
                            , ri =
                                worldVertex
                                    |> Vec3.add (Vec3.negate (Vec3.scale dot worldNormal))
                                    |> Vec3.add (Vec3.negate planeBody.position)
                            , rj = Vec3.sub worldVertex boxBody.position
                            , restitution = 0
                            }
                    else
                        identity
            )
            contactEquations
            convexPolyhedron.vertices


getBoxBoxContacts : Transform -> Vec3 -> BodyId -> Body -> Transform -> Vec3 -> BodyId -> Body -> List ContactEquation -> List ContactEquation
getBoxBoxContacts shapeTransform1 halfExtents1 bodyId1 body1 shapeTransform2 halfExtents2 bodyId2 body2 contactEquations =
    let
        convexPolyhedron1 =
            ConvexPolyhedron.fromBox halfExtents1

        convexPolyhedron2 =
            ConvexPolyhedron.fromBox halfExtents2
    in
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
