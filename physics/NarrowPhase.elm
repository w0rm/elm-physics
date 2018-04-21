module Physics.NarrowPhase exposing (getContacts, NarrowPhaseResult)

import Physics.World as World exposing (..)
import Physics.Body as Body exposing (..)
import Physics.Shape as Shape exposing (..)
import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (..)
import Physics.Quaternion as Quaternion
import Set exposing (Set)
import Math.Vector3 as Vec3 exposing (Vec3)
import Dict exposing (Dict)
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)


type alias NarrowPhaseResult =
    { contactEquations : List ContactEquation
    }


narrowPhaseResult : NarrowPhaseResult
narrowPhaseResult =
    { contactEquations = []
    }


getContacts : World -> NarrowPhaseResult
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
        narrowPhaseResult
        (World.getPairs world)


getBodyContacts : World -> BodyId -> Body -> BodyId -> Body -> NarrowPhaseResult -> NarrowPhaseResult
getBodyContacts world bodyId1 body1 bodyId2 body2 narrowPhaseResult =
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
        narrowPhaseResult
        body1.shapes


getShapeContacts : ShapeTransform -> Shape -> BodyId -> Body -> ShapeTransform -> Shape -> BodyId -> Body -> NarrowPhaseResult -> NarrowPhaseResult
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
                shapeTransform2
                halfExtents2
                bodyId2


getPlaneBoxContacts : ShapeTransform -> BodyId -> Body -> ShapeTransform -> Vec3 -> BodyId -> Body -> NarrowPhaseResult -> NarrowPhaseResult
getPlaneBoxContacts planeTransform planeBodyId planeBody boxTransform boxHalfExtents boxBodyId boxBody narrowPhaseResult =
    let
        worldNormal =
            Quaternion.rotate planeTransform.quaternion Vec3.k

        convexPolyhedron =
            ConvexPolyhedron.fromBox boxHalfExtents
    in
        List.foldl
            (\vertex acc ->
                let
                    worldVertex =
                        vertex
                            |> Quaternion.rotate boxTransform.quaternion
                            |> Vec3.add boxTransform.position

                    dot =
                        planeTransform.position
                            |> Vec3.sub worldVertex
                            |> Vec3.dot worldNormal
                in
                    if dot <= 0 then
                        { acc
                            | contactEquations =
                                { bodyId1 = planeBodyId
                                , bodyId2 = boxBodyId
                                , ni = worldNormal
                                , ri =
                                    worldVertex
                                        |> Vec3.add (Vec3.negate (Vec3.scale dot worldNormal))
                                        |> Vec3.add (Vec3.negate planeBody.position)
                                , rj =
                                    Vec3.sub worldVertex boxBody.position
                                }
                                    :: acc.contactEquations
                        }
                    else
                        acc
            )
            narrowPhaseResult
            convexPolyhedron.vertices


getBoxBoxContacts : ShapeTransform -> Vec3 -> BodyId -> ShapeTransform -> Vec3 -> BodyId -> NarrowPhaseResult -> NarrowPhaseResult
getBoxBoxContacts shapeTransform1 halfExtents1 bodyId1 shapeTransform2 halfExtents2 bodyId2 =
    identity
