module Collision.PlaneSphere exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Plane exposing (Plane)
import Internal.Sphere exposing (Sphere)
import Internal.Vector3 as Vec3


addContacts : (Contact -> Contact) -> Plane -> Sphere -> List Contact -> List Contact
addContacts orderContact plane sphere contacts =
    let
        worldVertex =
            plane.normal
                |> Vec3.scale sphere.radius
                |> Vec3.sub sphere.position

        dot =
            plane.position
                |> Vec3.sub worldVertex
                |> Vec3.dot plane.normal
    in
    if dot <= 0 then
        orderContact
            { ni = plane.normal
            , pi = Vec3.sub worldVertex (Vec3.scale dot plane.normal)
            , pj = worldVertex
            }
            :: contacts

    else
        contacts
