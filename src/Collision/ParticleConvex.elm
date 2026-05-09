module Collision.ParticleConvex exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex exposing (Convex, Face)


addContacts : String -> (Contact -> Contact) -> Vec3 -> Convex -> List Contact -> List Contact
addContacts idPrefix orderContact particlePosition { faces } contacts =
    case faces of
        ( primary, partner ) :: rest ->
            convexContact idPrefix orderContact particlePosition primary partner rest Const.maxNumber Vec3.zero contacts

        [] ->
            contacts


{-| Defers the `Contact` construction to the end of the recursion: tracks
(`bestDepth`, `bestNormal`) as separate args (sentinel
`bestDepth = Const.maxNumber` means "not yet found") and constructs the
contact once when faces are exhausted. The previous version allocated a
fresh `Contact` for every face that improved the best — for an N-face
convex that's up to N allocations per particle, of which only the last
is used.

Walks the grouped face structure with a single tail-recursive loop that
threads `currentFace : Face` plus the partner waiting in this group
(`nextFace : Maybe Face`) and `queuedGroups : List ( Face, Maybe Face )`
for the rest. Bails on the first face whose half-space excludes the
particle.

-}
convexContact : String -> (Contact -> Contact) -> Vec3 -> Face -> Maybe Face -> List ( Face, Maybe Face ) -> Float -> Vec3 -> List Contact -> List Contact
convexContact idPrefix orderContact particlePosition currentFace nextFace queuedGroups bestDepth bestNormal contacts =
    let
        point =
            case currentFace.vertices of
                first :: _ ->
                    first

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
                convexContact idPrefix orderContact particlePosition face Nothing queuedGroups newDepth newNormal contacts

            Nothing ->
                case queuedGroups of
                    ( primary, partner ) :: restGroups ->
                        convexContact idPrefix orderContact particlePosition primary partner restGroups newDepth newNormal contacts

                    [] ->
                        if newDepth - Const.maxNumber < 0 then
                            orderContact
                                { id = idPrefix
                                , ni = Vec3.negate newNormal
                                , pi = particlePosition
                                , pj = Vec3.add particlePosition (Vec3.scale newDepth newNormal)
                                }
                                :: contacts

                        else
                            contacts

    else
        contacts
