module Physics.NarrowPhase exposing (getContacts)

import Array.Hamt as Array
import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Body as Body exposing (Body, BodyId)
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
