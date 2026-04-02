module Collision.ParticleConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex exposing (Convex, Face)


addContacts : String -> (Contact -> Contact) -> Vec3 -> Convex -> List Contact -> List Contact
addContacts idPrefix orderContact particlePosition { faces } contacts =
    case convexContact idPrefix particlePosition faces Const.maxNumber Nothing of
        Just contact ->
            orderContact contact :: contacts

        Nothing ->
            contacts


convexContact : String -> Vec3 -> List Face -> Float -> Maybe Contact -> Maybe Contact
convexContact idPrefix particlePosition faces bestDepth bestContact =
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
                if dot - bestDepth < 0 then
                    convexContact idPrefix
                        particlePosition
                        remainingFaces
                        dot
                        (Just
                            { id = idPrefix
                            , ni = Vec3.negate normal
                            , pi = particlePosition
                            , pj = Vec3.add particlePosition (Vec3.scale dot normal)
                            }
                        )

                else
                    convexContact idPrefix
                        particlePosition
                        remainingFaces
                        bestDepth
                        bestContact

            else
                Nothing
