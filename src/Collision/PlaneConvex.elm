module Collision.PlaneConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Manifold as Manifold
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex as Convex exposing (Convex)
import Shapes.Plane exposing (Plane)


addContacts : Int -> (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts shapeKey orderContact plane convex contacts =
    case convex.obb of
        Convex.Box ax ay az he ->
            -- Eight corners tested inline as scalars; only a penetrating one
            -- allocates. emit captures the invariants so each call is A5, not a
            -- 14-arg curry.
            let
                center =
                    convex.position

                planePosition =
                    plane.position

                planeNormal =
                    plane.normal

                emit sx sy sz vertexId acc =
                    let
                        vx =
                            center.x + sx * he.x * ax.x + sy * he.y * ay.x + sz * he.z * az.x

                        vy =
                            center.y + sx * he.x * ax.y + sy * he.y * ay.y + sz * he.z * az.y

                        vz =
                            center.z + sx * he.x * ax.z + sy * he.y * ay.z + sz * he.z * az.z

                        dot =
                            ((vx - planePosition.x) * planeNormal.x)
                                + ((vy - planePosition.y) * planeNormal.y)
                                + ((vz - planePosition.z) * planeNormal.z)
                    in
                    if dot - Const.contactBreakingThreshold < 0 then
                        orderContact
                            { shapeKey = shapeKey
                            , featureKey = ContactId.planeVertex vertexId
                            , ni = planeNormal
                            , pi =
                                { x = vx - dot * planeNormal.x
                                , y = vy - dot * planeNormal.y
                                , z = vz - dot * planeNormal.z
                                }
                            , pj = { x = vx, y = vy, z = vz }
                            }
                            :: acc

                    else
                        acc
            in
            contacts
                |> emit 1 1 1 1
                |> emit 1 1 -1 2
                |> emit 1 -1 1 3
                |> emit 1 -1 -1 4
                |> emit -1 1 1 5
                |> emit -1 1 -1 6
                |> emit -1 -1 1 7
                |> emit -1 -1 -1 8

        Convex.NotBox vertices _ _ _ ->
            -- A many-sided face (cylinder cap) dips >4 vertices; cull like the clip.
            emitPlaneContacts shapeKey
                orderContact
                plane.position
                plane.normal
                (Manifold.reduce plane.normal
                    -(Vec3.dot plane.normal plane.position)
                    (List.indexedMap (\i v -> ( i + 1, v )) vertices)
                )
                contacts


{-| A plane contact per culled vertex: project onto the plane for `pi`, vertex as `pj`.
-}
emitPlaneContacts : Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> List ( Int, Vec3 ) -> List Contact -> List Contact
emitPlaneContacts shapeKey orderContact planePosition planeNormal points contacts =
    case points of
        ( vertexId, vertex ) :: rest ->
            let
                dot =
                    ((vertex.x - planePosition.x) * planeNormal.x)
                        + ((vertex.y - planePosition.y) * planeNormal.y)
                        + ((vertex.z - planePosition.z) * planeNormal.z)
            in
            emitPlaneContacts shapeKey
                orderContact
                planePosition
                planeNormal
                rest
                (orderContact
                    { shapeKey = shapeKey
                    , featureKey = ContactId.planeVertex vertexId
                    , ni = planeNormal
                    , pi =
                        { x = vertex.x - dot * planeNormal.x
                        , y = vertex.y - dot * planeNormal.y
                        , z = vertex.z - dot * planeNormal.z
                        }
                    , pj = vertex
                    }
                    :: contacts
                )

        [] ->
            contacts
