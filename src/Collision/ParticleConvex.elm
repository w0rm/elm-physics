module Collision.ParticleConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex, Face)
import Internal.Quaternion as Quaternion
import Internal.Const as Const
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : (Contact -> Contact) -> Transform -> Transform -> Convex -> List Contact -> List Contact
addContacts orderContact { position } t2 { faces } contacts =
    case convexContact position faces t2 Const.maxNumber Nothing of
        Just contact ->
            orderContact contact :: contacts
        Nothing ->
            contacts


convexContact : Vec3 -> List Face -> Transform -> Float -> Maybe Contact -> Maybe Contact
convexContact particlePosition faces convexTransform bestDepth bestContact =
    case faces of
        [] ->
            bestContact

        { point, normal } :: remainingFaces ->
            let
                worldFaceNormal =
                    Quaternion.rotate convexTransform.orientation normal

                worldFacePoint =
                    Transform.pointToWorldFrame convexTransform point

                dot =
                    Vec3.dot
                        worldFaceNormal
                        (Vec3.sub worldFacePoint particlePosition)
            in
            if dot >= 0 then
                if dot < bestDepth then
                    convexContact
                        particlePosition
                        remainingFaces
                        convexTransform
                        dot
                        (Just
                            { ni = Vec3.negate worldFaceNormal
                            , pi = particlePosition
                            , pj = Vec3.add particlePosition (Vec3.scale dot worldFaceNormal)
                            }
                        )
                else
                    convexContact
                        particlePosition
                        remainingFaces
                        convexTransform
                        bestDepth
                        bestContact

            else
                Nothing


