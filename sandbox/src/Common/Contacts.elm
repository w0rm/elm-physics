module Common.Contacts exposing (Labeled, worldPoints)

{-| Pull per-contact ids out of `Physics.Contacts` for the debug
overlay. The `id` field is the SAT axis label set by the collision
routine ("cap-on-edge", "face-+x", etc.) and is what makes the
`debugContactIds` toggle useful in collision-tester demos.

The transform replicates `Physics.contactPoints`'s post-sim
re-projection so points line up with the rendered body positions.

-}

import Array
import Internal.Contact as InternalContact
import Internal.SolverBody as SolverBody
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
    helper c.dt c.gravity c.pairGroups c.solverBodies []


helper :
    Float
    -> { x : Float, y : Float, z : Float }
    -> List InternalContact.PairGroup
    -> Array.Array (SolverBody.SolverBody id)
    -> List Labeled
    -> List Labeled
helper dt gravity pairGroups solverBodies acc =
    case pairGroups of
        [] ->
            acc

        pairGroup :: rest ->
            case pairGroup.contacts of
                [] ->
                    helper dt gravity rest solverBodies acc

                _ ->
                    case Array.get pairGroup.body1.id solverBodies of
                        Just solverBody1 ->
                            let
                                newBody1 =
                                    SolverBody.toBody dt gravity solverBody1

                                transform =
                                    Transform3d.atOrigin
                                        |> Transform3d.relativeTo pairGroup.body1.transform3d
                                        |> Transform3d.placeIn newBody1.transform3d

                                entries =
                                    List.map
                                        (\{ contact } ->
                                            { id = contact.id
                                            , point =
                                                Point3d.fromMeters
                                                    (Transform3d.pointPlaceIn transform contact.pi)
                                            }
                                        )
                                        pairGroup.contacts
                            in
                            helper dt gravity rest solverBodies (entries ++ acc)

                        Nothing ->
                            helper dt gravity rest solverBodies acc
