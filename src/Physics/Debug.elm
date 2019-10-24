module Physics.Debug exposing
    ( getContacts
    , FaceNormal, getFaceNormals
    , UniqueEdge, getUniqueEdges
    )

{-| A list of utilities that may be useful for debugging.

@docs getContacts

@docs FaceNormal, getFaceNormals

@docs UniqueEdge, getUniqueEdges

-}

import Direction3d exposing (Direction3d)
import Internal.Body as InternalBody
import Internal.BroadPhase as BroadPhase
import Internal.Convex as Convex
import Internal.Coordinates exposing (BodyLocalCoordinates)
import Internal.Shape exposing (Kind(..), Shape)
import Internal.World exposing (Protected(..))
import Length exposing (Meters)
import Physics.Body exposing (Body)
import Physics.World exposing (World)
import Point3d exposing (Point3d)


{-| Get the contact points in the world.
-}
getContacts : World data -> List { x : Float, y : Float, z : Float }
getContacts (Protected world) =
    List.foldl
        (\{ contacts } acc1 ->
            List.foldl
                (\{ pi, pj } acc2 -> pi :: pj :: acc2)
                acc1
                contacts
        )
        []
        (BroadPhase.getContacts world)


{-| A face normal consists of a normal vector for a face
and a reference point on the face.

These are both expressed within the local body coordinate system.

-}
type alias FaceNormal =
    { normal : Direction3d BodyLocalCoordinates
    , point : Point3d Meters BodyLocalCoordinates
    }


{-| Get the face normals of the body.
-}
getFaceNormals : Body data -> List FaceNormal
getFaceNormals (InternalBody.Protected { shapes }) =
    List.foldl addFaceNormals [] shapes


addFaceNormals : Shape -> List FaceNormal -> List FaceNormal
addFaceNormals { kind, frame3d } normals =
    case kind of
        Convex convex ->
            Convex.foldFaceNormals
                (\normal point ->
                    (::)
                        { normal = Direction3d.placeIn frame3d (Direction3d.unsafe normal)
                        , point = Point3d.placeIn frame3d (Point3d.fromMeters point)
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
    { direction : Direction3d BodyLocalCoordinates
    , point : Point3d Meters BodyLocalCoordinates
    }


{-| Get the unique edges of the body.
-}
getUniqueEdges : Body data -> List UniqueEdge
getUniqueEdges (InternalBody.Protected { shapes }) =
    List.foldl addUniqueEdges [] shapes


addUniqueEdges : Shape -> List UniqueEdge -> List UniqueEdge
addUniqueEdges { kind, frame3d } edges =
    case kind of
        Convex convex ->
            Convex.foldUniqueEdges
                (\point direction ->
                    (::)
                        { direction = Direction3d.placeIn frame3d (Direction3d.unsafe direction)
                        , point = Point3d.placeIn frame3d (Point3d.fromMeters point)
                        }
                )
                edges
                convex

        _ ->
            edges
