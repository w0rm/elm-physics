module Physics.Debug exposing
    ( getContacts, getCenterOfMass
    , FaceNormal, getFaceNormals
    , UniqueEdge, getUniqueEdges
    )

{-| A list of utilities that may be useful for
debugging issues with elm-physics itself.

@docs getContacts, getCenterOfMass

@docs FaceNormal, getFaceNormals

@docs UniqueEdge, getUniqueEdges

-}

import Direction3d exposing (Direction3d)
import Internal.Body as InternalBody
import Internal.BroadPhase as BroadPhase
import Internal.Convex as Convex
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Shape exposing (Kind(..), Shape)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.World exposing (Protected(..))
import Length exposing (Meters)
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World exposing (World)
import Point3d exposing (Point3d)


{-| Get the contact points in the world.
-}
getContacts : World data -> List (Point3d Meters WorldCoordinates)
getContacts (Protected world) =
    List.foldl
        (\{ contacts } acc1 ->
            List.foldl
                (\{ pi, pj } acc2 -> Point3d.fromMeters pi :: Point3d.fromMeters pj :: acc2)
                acc1
                contacts
        )
        []
        (BroadPhase.getContacts world)


{-| Get the center of mass of the body.
-}
getCenterOfMass : Body data -> Point3d Meters BodyCoordinates
getCenterOfMass (InternalBody.Protected { centerOfMassTransform3d }) =
    Point3d.fromMeters (Transform3d.originPoint centerOfMassTransform3d)


{-| A face normal consists of a normal vector for a face
and a reference point on the face.

These are both expressed within the local body coordinate system.

-}
type alias FaceNormal =
    { normal : Direction3d BodyCoordinates
    , point : Point3d Meters BodyCoordinates
    }


{-| Get the face normals of the body.
-}
getFaceNormals : Body data -> List FaceNormal
getFaceNormals (InternalBody.Protected { shapes, centerOfMassTransform3d }) =
    List.foldl (addFaceNormals centerOfMassTransform3d) [] shapes


addFaceNormals : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates } -> Shape CenterOfMassCoordinates -> List FaceNormal -> List FaceNormal
addFaceNormals centerOfMassTransform3d { kind, transform3d } normals =
    case kind of
        Convex convex ->
            Convex.foldFaceNormals
                (\normal point ->
                    (::)
                        { normal = Direction3d.unsafe (Transform3d.directionPlaceIn (Transform3d.placeIn centerOfMassTransform3d transform3d) normal)
                        , point = Point3d.fromMeters (Transform3d.pointPlaceIn (Transform3d.placeIn centerOfMassTransform3d transform3d) point)
                        }
                )
                normals
                convex

        _ ->
            normals


{-| A unique edge consists of a unit direction vector
that runs parallel to an edge of a face.

A vertex point of the body is also provided for context.

These are both expressed within the local body coordinate system.

-}
type alias UniqueEdge =
    { direction : Direction3d BodyCoordinates
    , point : Point3d Meters BodyCoordinates
    }


{-| Get the unique edges of the body.
-}
getUniqueEdges : Body data -> List UniqueEdge
getUniqueEdges (InternalBody.Protected { shapes, centerOfMassTransform3d }) =
    List.foldl (addUniqueEdges centerOfMassTransform3d) [] shapes


addUniqueEdges : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates } -> Shape CenterOfMassCoordinates -> List UniqueEdge -> List UniqueEdge
addUniqueEdges centerOfMassTransform3d { kind, transform3d } edges =
    case kind of
        Convex convex ->
            Convex.foldUniqueEdges
                (\point direction ->
                    (::)
                        { direction = Direction3d.unsafe (Transform3d.directionPlaceIn (Transform3d.placeIn centerOfMassTransform3d transform3d) direction)
                        , point = Point3d.fromMeters (Transform3d.pointPlaceIn (Transform3d.placeIn centerOfMassTransform3d transform3d) point)
                        }
                )
                edges
                convex

        _ ->
            edges
