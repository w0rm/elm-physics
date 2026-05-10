module Collision.CapsuleParticle exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Capsule exposing (Capsule)


addContacts : String -> (Contact -> Contact) -> Capsule -> Vec3 -> List Contact -> List Contact
addContacts idPrefix orderContact capsule particlePosition contacts =
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
            { id = idPrefix
            , ni = normal
            , pi = Vec3.add closestPoint (Vec3.scale capsule.radius normal)
            , pj = particlePosition
            }
            :: contacts
