module Physics.Debug exposing (getContacts, getFaceNormals, getUniqueEdges)

{-| List of utilities that may be useful for debugging.

@docs getContacts, getFaceNormals, getUniqueEdges

-}

import AltMath.Vector3 as Vec3 exposing (Vec3)
import Physics.Body exposing (Body)
import Physics.World exposing (World)


{-| Get the contact points in the world.
-}
getContacts : World data -> List Vec3
getContacts _ =
    []


{-| Get the face normals of the body, where each face normal consists
of a normal vector for a face and a reference point within the face.

These are both expressed within the local body coordinate system.

-}
getFaceNormals : Body data -> List ( Vec3, Vec3 )
getFaceNormals _ =
    []


{-| Get the unique edges of the body, where each unique edge consists
of a unit direction vector that runs parallel to an edge of a face.

A vertex point of the body is also provided for context.

These are both expressed within the local body coordinate system.

-}
getUniqueEdges : Body data -> List ( Vec3, Vec3 )
getUniqueEdges _ =
    []
