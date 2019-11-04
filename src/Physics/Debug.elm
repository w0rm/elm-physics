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
import Frame3d exposing (Frame3d)
import Internal.Body as InternalBody
import Internal.BroadPhase as BroadPhase
import Internal.Convex as Convex
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Shape exposing (Kind(..), Shape)
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
getCenterOfMass (InternalBody.Protected { centerOfMassFrame3d }) =
    Frame3d.originPoint centerOfMassFrame3d


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
getFaceNormals (InternalBody.Protected { shapes, centerOfMassFrame3d }) =
    List.foldl (addFaceNormals centerOfMassFrame3d) [] shapes


addFaceNormals : Frame3d Meters BodyCoordinates { defines : CenterOfMassCoordinates } -> Shape CenterOfMassCoordinates -> List FaceNormal -> List FaceNormal
addFaceNormals centerOfMassFrame3d { kind, frame3d } normals =
    case kind of
        Convex convex ->
            Convex.foldFaceNormals
                (\normal point ->
                    (::)
                        { normal = Direction3d.placeIn (Frame3d.placeIn centerOfMassFrame3d frame3d) (Direction3d.unsafe normal)
                        , point = Point3d.placeIn (Frame3d.placeIn centerOfMassFrame3d frame3d) (Point3d.fromMeters point)
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
getUniqueEdges (InternalBody.Protected { shapes, centerOfMassFrame3d }) =
    List.foldl (addUniqueEdges centerOfMassFrame3d) [] shapes


addUniqueEdges : Frame3d Meters BodyCoordinates { defines : CenterOfMassCoordinates } -> Shape CenterOfMassCoordinates -> List UniqueEdge -> List UniqueEdge
addUniqueEdges centerOfMassFrame3d { kind, frame3d } edges =
    case kind of
        Convex convex ->
            Convex.foldUniqueEdges
                (\point direction ->
                    (::)
                        { direction = Direction3d.placeIn (Frame3d.placeIn centerOfMassFrame3d frame3d) (Direction3d.unsafe direction)
                        , point = Point3d.placeIn (Frame3d.placeIn centerOfMassFrame3d frame3d) (Point3d.fromMeters point)
                        }
                )
                edges
                convex

        _ ->
            edges
