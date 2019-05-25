module Collision.PlaneConvex exposing (addContacts)

import Internal.Contact as Contact exposing (Contact)
import Internal.Convex as Convex exposing (Convex)
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : (Contact -> Contact) -> Transform -> Transform -> Convex -> List Contact -> List Contact
addContacts orderContact planeTransform convexTransform { vertices } contacts =
    let
        worldNormal =
            Quaternion.rotate planeTransform.orientation Vec3.k
    in
    List.foldl
        (\vertex currentContacts ->
            let
                worldVertex =
                    Transform.pointToWorldFrame convexTransform vertex

                dot =
                    planeTransform.position
                        |> Vec3.sub worldVertex
                        |> Vec3.dot worldNormal
            in
            if dot <= 0 then
                orderContact
                    { ni = worldNormal
                    , pi =
                        worldNormal
                            |> Vec3.scale dot
                            |> Vec3.sub worldVertex
                    , pj = worldVertex
                    }
                    :: currentContacts

            else
                currentContacts
        )
        contacts
        vertices
