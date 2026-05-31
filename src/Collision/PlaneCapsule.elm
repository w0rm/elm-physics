module Collision.PlaneCapsule exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 exposing (Vec3)
import Shapes.Capsule exposing (Capsule)
import Shapes.Plane exposing (Plane)


addContacts : Int -> (Contact -> Contact) -> Plane -> Capsule -> List Contact -> List Contact
addContacts shapeKey orderContact { normal, position } capsule contacts =
    let
        ep1 =
            { x = capsule.position.x - capsule.halfLength * capsule.axis.x
            , y = capsule.position.y - capsule.halfLength * capsule.axis.y
            , z = capsule.position.z - capsule.halfLength * capsule.axis.z
            }

        ep2 =
            { x = capsule.position.x + capsule.halfLength * capsule.axis.x
            , y = capsule.position.y + capsule.halfLength * capsule.axis.y
            , z = capsule.position.z + capsule.halfLength * capsule.axis.z
            }

        contacts1 =
            addCapContact shapeKey (ContactId.planeCapEnd 1) orderContact normal position capsule.radius ep1 contacts
    in
    addCapContact shapeKey (ContactId.planeCapEnd 2) orderContact normal position capsule.radius ep2 contacts1


addCapContact : Int -> Int -> (Contact -> Contact) -> Vec3 -> Vec3 -> Float -> Vec3 -> List Contact -> List Contact
addCapContact shapeKey featureKey orderContact normal planePosition radius ep contacts =
    let
        vertex =
            { x = ep.x - radius * normal.x
            , y = ep.y - radius * normal.y
            , z = ep.z - radius * normal.z
            }

        dot =
            ((vertex.x - planePosition.x) * normal.x)
                + ((vertex.y - planePosition.y) * normal.y)
                + ((vertex.z - planePosition.z) * normal.z)
    in
    if dot - Const.contactBreakingThreshold < 0 then
        orderContact
            { shapeKey = shapeKey
            , featureKey = featureKey
            , ni = normal
            , pi =
                { x = vertex.x - dot * normal.x
                , y = vertex.y - dot * normal.y
                , z = vertex.z - dot * normal.z
                }
            , pj = vertex
            }
            :: contacts

    else
        contacts
