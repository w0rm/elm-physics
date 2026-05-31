module Collision.CapsuleSphere exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 as Vec3
import Shapes.Capsule exposing (Capsule)
import Shapes.Sphere exposing (Sphere)


addContacts : Int -> (Contact -> Contact) -> Capsule -> Sphere -> List Contact -> List Contact
addContacts shapeKey orderContact capsule sphere contacts =
    let
        t =
            max -capsule.halfLength
                (min capsule.halfLength
                    (Vec3.dot (Vec3.sub sphere.position capsule.position) capsule.axis)
                )

        closestPoint =
            Vec3.add capsule.position (Vec3.scale t capsule.axis)

        distance =
            Vec3.distance sphere.position closestPoint - capsule.radius - sphere.radius

        normal =
            Vec3.direction sphere.position closestPoint
    in
    if distance > 0 then
        contacts

    else
        orderContact
            { shapeKey = shapeKey
            , featureKey = ContactId.simple
            , ni = normal
            , pi = Vec3.add closestPoint (Vec3.scale capsule.radius normal)
            , pj = Vec3.sub sphere.position (Vec3.scale sphere.radius normal)
            }
            :: contacts
