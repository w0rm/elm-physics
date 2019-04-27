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

import Internal.Body as InternalBody
import Internal.ConvexPolyhedron as ConvexPolyhedron
import Internal.NarrowPhase as NarrowPhase
import Internal.Quaternion as Quaternion
import Internal.Shape exposing (Kind(..), Shape)
import Internal.Vector3 as Vec3
import Internal.World exposing (Protected(..))
import Physics.Body exposing (Body)
import Physics.World exposing (World)


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
        -- TODO: maybe cache the previous contacts in the world
        (NarrowPhase.getContacts world)


{-| A face normal consists of a normal vector for a face
and a reference point on the face.

These are both expressed within the local body coordinate system.

-}
type alias FaceNormal =
    { normal : { x : Float, y : Float, z : Float }
    , point : { x : Float, y : Float, z : Float }
    }


{-| Get the face normals of the body.
-}
getFaceNormals : Body data -> List FaceNormal
getFaceNormals (InternalBody.Protected { shapes }) =
    List.foldl addFaceNormals [] shapes


addFaceNormals : Shape -> List FaceNormal -> List FaceNormal
addFaceNormals { kind, position, orientation } normals =
    case kind of
        Convex convex ->
            ConvexPolyhedron.foldFaceNormals
                (\normal point ->
                    (::)
                        { normal = Quaternion.rotate orientation normal
                        , point =
                            point
                                |> Quaternion.rotate orientation
                                |> Vec3.add position
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
    { direction : { x : Float, y : Float, z : Float }
    , point : { x : Float, y : Float, z : Float }
    }


{-| Get the unique edges of the body.
-}
getUniqueEdges : Body data -> List UniqueEdge
getUniqueEdges (InternalBody.Protected { shapes }) =
    List.foldl addUniqueEdges [] shapes


addUniqueEdges : Shape -> List UniqueEdge -> List UniqueEdge
addUniqueEdges { kind, position, orientation } edges =
    case kind of
        Convex convex ->
            ConvexPolyhedron.foldUniqueEdges
                (\point direction ->
                    (::)
                        { direction = Quaternion.rotate orientation direction
                        , point =
                            point
                                |> Quaternion.rotate orientation
                                |> Vec3.add position
                        }
                )
                edges
                convex

        _ ->
            edges
