module Physics.Debug exposing (getContacts, getFaceNormals, getUniqueEdges)

{-| List of utilities that may be useful for debugging.

@docs getContacts, getFaceNormals, getUniqueEdges

-}

import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.Body as InternalBody
import Internal.ConvexPolyhedron as ConvexPolyhedron
import Internal.NarrowPhase as NarrowPhase
import Internal.Quaternion as Quaternion
import Internal.Shape exposing (Kind(..), Shape)
import Internal.World exposing (Protected(..))
import Physics.Body exposing (Body)
import Physics.World exposing (World)


{-| Get the contact points in the world.
-}
getContacts : World data -> List Vec3
getContacts (Protected world) =
    List.foldl
        (\{ body1, body2, ri, rj } acc ->
            Vec3.add body1.position ri :: Vec3.add body2.position rj :: acc
        )
        []
        -- TODO: maybe cache the previous contacts in the world
        (NarrowPhase.getContacts world)


{-| Get the face normals of the body, where each face normal consists
of a normal vector for a face and a reference point within the face.

These are both expressed within the local body coordinate system.

-}
getFaceNormals : Body data -> List { normal : Vec3, point : Vec3 }
getFaceNormals (InternalBody.Protected { shapes }) =
    List.foldl addFaceNormals [] shapes


addFaceNormals : Shape -> List { normal : Vec3, point : Vec3 } -> List { normal : Vec3, point : Vec3 }
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


{-| Get the unique edges of the body, where each unique edge consists
of a unit direction vector that runs parallel to an edge of a face.

A vertex point of the body is also provided for context.

These are both expressed within the local body coordinate system.

-}
getUniqueEdges : Body data -> List { direction : Vec3, point : Vec3 }
getUniqueEdges (InternalBody.Protected { shapes }) =
    List.foldl addUniqueEdges [] shapes


addUniqueEdges : Shape -> List { direction : Vec3, point : Vec3 } -> List { direction : Vec3, point : Vec3 }
addUniqueEdges { kind, position, orientation } edges =
    case kind of
        Convex convex ->
            ConvexPolyhedron.foldUniqueEdges
                (\direction point ->
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
