module Collision.ParticleConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.VertexBuffer as VertexBuffer exposing (VertexBuffer)
import Shapes.Convex exposing (Convex, Face, FaceGroup(..))


addContacts : Int -> (Contact -> Contact) -> Vec3 -> Convex -> List Contact -> List Contact
addContacts shapeKey orderContact particlePosition { faces, vertexBuffer } contacts =
    case faces of
        (OneSidedFace n i _ _ _ _) :: rest ->
            convexContact shapeKey orderContact vertexBuffer particlePosition { normal = n, vertices = i } Nothing rest Const.maxNumber Vec3.zero contacts

        (TwoSidedFace n1 i1 _ n2 i2 _) :: rest ->
            convexContact shapeKey orderContact vertexBuffer particlePosition { normal = n1, vertices = i1 } (Just { normal = n2, vertices = i2 }) rest Const.maxNumber Vec3.zero contacts

        [] ->
            contacts


{-| Tracks (bestDepth, bestNormal) and builds the Contact once when faces
are exhausted (sentinel `bestDepth = Const.maxNumber`). Bails on the first
face whose half-space excludes the particle.
-}
convexContact : Int -> (Contact -> Contact) -> VertexBuffer -> Vec3 -> Face -> Maybe Face -> List FaceGroup -> Float -> Vec3 -> List Contact -> List Contact
convexContact shapeKey orderContact buffer particlePosition currentFace nextFace queuedGroups bestDepth bestNormal contacts =
    let
        point =
            case currentFace.vertices of
                first :: _ ->
                    VertexBuffer.get first buffer

                [] ->
                    Vec3.zero

        dot =
            Vec3.dot
                currentFace.normal
                (Vec3.sub point particlePosition)
    in
    if dot >= 0 then
        let
            ( newDepth, newNormal ) =
                if dot - bestDepth < 0 then
                    ( dot, currentFace.normal )

                else
                    ( bestDepth, bestNormal )
        in
        case nextFace of
            Just face ->
                convexContact shapeKey orderContact buffer particlePosition face Nothing queuedGroups newDepth newNormal contacts

            Nothing ->
                case queuedGroups of
                    (OneSidedFace n i _ _ _ _) :: restGroups ->
                        convexContact shapeKey orderContact buffer particlePosition { normal = n, vertices = i } Nothing restGroups newDepth newNormal contacts

                    (TwoSidedFace n1 i1 _ n2 i2 _) :: restGroups ->
                        convexContact shapeKey orderContact buffer particlePosition { normal = n1, vertices = i1 } (Just { normal = n2, vertices = i2 }) restGroups newDepth newNormal contacts

                    [] ->
                        if newDepth - Const.maxNumber < 0 then
                            orderContact
                                { shapeKey = shapeKey
                                , featureKey = ContactId.simple
                                , ni = Vec3.negate newNormal
                                , pi = particlePosition
                                , pj = Vec3.add particlePosition (Vec3.scale newDepth newNormal)
                                }
                                :: contacts

                        else
                            contacts

    else
        contacts
