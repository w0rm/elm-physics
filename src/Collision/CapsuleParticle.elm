module Collision.CapsuleParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Capsule exposing (Capsule)


addContacts : Int -> (Contact -> Contact) -> Capsule -> Vec3 -> List Contact -> List Contact
addContacts shapeKey orderContact capsule particlePosition contacts =
    let
        t =
            max -capsule.halfLength
                (min capsule.halfLength
                    (Vec3.dot (Vec3.sub particlePosition capsule.position) capsule.axis)
                )

        closestPoint =
            Vec3.add capsule.position (Vec3.scale t capsule.axis)

        distance =
            Vec3.distance particlePosition closestPoint - capsule.radius

        normal =
            Vec3.direction particlePosition closestPoint
    in
    if distance > 0 then
        contacts

    else
        orderContact
            { shapeKey = shapeKey
            , featureKey = ContactId.simple
            , ni = normal
            , pi = Vec3.add closestPoint (Vec3.scale capsule.radius normal)
            , pj = particlePosition
            }
            :: contacts
