module Collision.PlaneConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 exposing (Vec3)
import Shapes.Convex as Convex exposing (Convex)
import Shapes.Plane exposing (Plane)


addContacts : Int -> (Contact -> Contact) -> Plane -> Convex -> List Contact -> List Contact
addContacts shapeKey orderContact plane convex contacts =
    case convex.obb of
        Convex.Box ax ay az he ->
            -- Test the eight corners inline: each is computed as scalars (no
            -- intermediate `Vec3` list), and only a penetrating corner allocates
            -- a contact. Corner order/ids match `boxCorners`.
            let
                emit sx sy sz vertexId acc =
                    boxCornerContact shapeKey orderContact plane.position plane.normal convex.position ax ay az he sx sy sz vertexId acc
            in
            contacts
                |> emit 1 1 1 1
                |> emit 1 1 (-1) 2
                |> emit 1 (-1) 1 3
                |> emit 1 (-1) (-1) 4
                |> emit (-1) 1 1 5
                |> emit (-1) 1 (-1) 6
                |> emit (-1) (-1) 1 7
                |> emit (-1) (-1) (-1) 8

        Convex.NotBox vertices _ _ _ ->
            addContactsHelp shapeKey
                orderContact
                plane.position
                plane.normal
                0
                vertices
                contacts


{-| One box corner (`centre + sx·he.x·ax + sy·he.y·ay + sz·he.z·az`) against the
plane. Computes the corner as scalars and only builds a contact when it
penetrates — same arithmetic as the materialised-list path, no allocation
otherwise.
-}
boxCornerContact : Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Int -> List Contact -> List Contact
boxCornerContact shapeKey orderContact planePosition planeNormal center ax ay az he sx sy sz vertexId contacts =
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
            :: contacts

    else
        contacts


addContactsHelp : Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> Int -> List Vec3 -> List Contact -> List Contact
addContactsHelp shapeKey orderContact planePosition planeNormal vertexId vertices contacts =
    case vertices of
        vertex :: remainingVertices ->
            let
                dot =
                    ((vertex.x - planePosition.x) * planeNormal.x)
                        + ((vertex.y - planePosition.y) * planeNormal.y)
                        + ((vertex.z - planePosition.z) * planeNormal.z)
            in
            if dot - Const.contactBreakingThreshold < 0 then
                addContactsHelp shapeKey
                    orderContact
                    planePosition
                    planeNormal
                    (vertexId + 1)
                    remainingVertices
                    (orderContact
                        { shapeKey = shapeKey
                        , featureKey = ContactId.planeVertex (vertexId + 1)
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

            else
                addContactsHelp shapeKey
                    orderContact
                    planePosition
                    planeNormal
                    (vertexId + 1)
                    remainingVertices
                    contacts

        [] ->
            contacts
