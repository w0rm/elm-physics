module Collision.ParticleConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Convex exposing (Convex, Face)
import Internal.Coordinates exposing (ShapeWorldTransform3d)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


addContacts : (Contact -> Contact) -> ShapeWorldTransform3d -> ShapeWorldTransform3d -> Convex -> List Contact -> List Contact
addContacts orderContact convexFrame3d t2 { faces } contacts =
    case convexContact (Transform3d.originPoint convexFrame3d) faces t2 Const.maxNumber Nothing of
        Just contact ->
            orderContact contact :: contacts

        Nothing ->
            contacts


convexContact : Vec3 -> List Face -> ShapeWorldTransform3d -> Float -> Maybe Contact -> Maybe Contact
convexContact particlePosition faces convexFrame3d bestDepth bestContact =
    case faces of
        [] ->
            bestContact

        { point, normal } :: remainingFaces ->
            let
                worldFaceNormal =
                    Transform3d.directionPlaceIn convexFrame3d normal

                worldFacePoint =
                    Transform3d.pointPlaceIn convexFrame3d point

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
                        convexFrame3d
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
                        convexFrame3d
                        bestDepth
                        bestContact

            else
                Nothing
