module Common.Contacts exposing (Labeled, worldPoints)

{-| Pull per-contact ids out of `Physics.Contacts` for the debug
overlay. The `id` field is the SAT axis label set by the collision
routine ("cap-on-edge", "face-+x", etc.) and is what makes the
`debugContactIds` toggle useful in collision-tester demos.

The transform replicates `Physics.contactPoints`'s post-sim
re-projection so points line up with the rendered body positions.

-}

import Array
import Internal.Body as InternalBody
import Internal.Contact as InternalContact
import Internal.ContactId as ContactId
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Physics exposing (WorldCoordinates)
import Physics.Types as Types
import Point3d exposing (Point3d)


type alias Labeled =
    { id : String
    , point : Point3d Meters WorldCoordinates
    }


worldPoints : Physics.Contacts id -> List Labeled
worldPoints (Types.Contacts c) =
    helper c.pairGroups c.bodies []


helper :
    List InternalContact.PairGroup
    -> Array.Array ( id, InternalBody.Body )
    -> List Labeled
    -> List Labeled
helper pairGroups bodies acc =
    case pairGroups of
        [] ->
            acc

        pairGroup :: rest ->
            case pairGroup.contacts of
                [] ->
                    helper rest bodies acc

                _ ->
                    case Array.get pairGroup.body1.id bodies of
                        Just ( _, body1 ) ->
                            let
                                -- body1 is already integrated by the solver
                                transform =
                                    Transform3d.atOrigin
                                        |> Transform3d.relativeTo pairGroup.body1.transform3d
                                        |> Transform3d.placeIn body1.transform3d

                                entries =
                                    List.map
                                        (\{ contact } ->
                                            { id = ContactId.toString contact.shapeKey contact.featureKey
                                            , point =
                                                Point3d.fromMeters
                                                    (Transform3d.pointPlaceIn transform contact.pi)
                                            }
                                        )
                                        pairGroup.contacts
                            in
                            helper rest bodies (entries ++ acc)

                        Nothing ->
                            helper rest bodies acc
