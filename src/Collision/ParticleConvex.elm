module Collision.ParticleConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex exposing (Convex, Face)


addContacts : (Contact -> Contact) -> Vec3 -> Convex -> List Contact -> List Contact
addContacts orderContact particlePosition { faces } contacts =
    case convexContact particlePosition faces Const.maxNumber Nothing of
        Just contact ->
            orderContact contact :: contacts

        Nothing ->
            contacts


convexContact : Vec3 -> List Face -> Float -> Maybe Contact -> Maybe Contact
convexContact particlePosition faces bestDepth bestContact =
    case faces of
        [] ->
            bestContact

        { vertices, normal } :: remainingFaces ->
            let
                point =
                    case vertices of
                        first :: _ ->
                            first

                        [] ->
                            Vec3.zero

                dot =
                    Vec3.dot
                        normal
                        (Vec3.sub point particlePosition)
            in
            if dot >= 0 then
                if dot < bestDepth then
                    convexContact
                        particlePosition
                        remainingFaces
                        dot
                        (Just
                            { ni = Vec3.negate normal
                            , pi = particlePosition
                            , pj = Vec3.add particlePosition (Vec3.scale dot normal)
                            }
                        )

                else
                    convexContact
                        particlePosition
                        remainingFaces
                        bestDepth
                        bestContact

            else
                Nothing
