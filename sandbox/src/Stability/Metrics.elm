module Stability.Metrics exposing (Metrics, compute)

{-| Stability metrics computed from a list of bodies after N simulation frames.

Only dynamic bodies (those with non-zero mass) are included.
Static bodies like ground planes are excluded.

-}

import Physics
import Speed
import Vector3d


type alias Metrics =
    { maxSpeed : Float
    , avgSpeed : Float
    , dynamicBodyCount : Int
    }


{-| Compute stability metrics from the current body state.
-}
compute : List ( id, Physics.Body ) -> Metrics
compute bodies =
    let
        dynamicBodies =
            List.filterMap
                (\( _, body ) ->
                    case Physics.mass body of
                        Nothing ->
                            Nothing

                        Just _ ->
                            Just body
                )
                bodies

        speeds =
            List.map
                (\body ->
                    Speed.inMetersPerSecond
                        (Vector3d.length (Physics.velocity body))
                )
                dynamicBodies

        count =
            List.length dynamicBodies
    in
    { maxSpeed = Maybe.withDefault 0 (List.maximum speeds)
    , avgSpeed =
        if count == 0 then
            0

        else
            List.sum speeds / toFloat count
    , dynamicBodyCount = count
    }


