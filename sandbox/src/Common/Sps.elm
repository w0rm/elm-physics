module Common.Sps exposing
    ( Sps
    , init
    , update
    , view
    )

{-| Tracks how often physics actually advances when the simulation is
gated by `Timestep`. The display FPS lies about physics
work — at 120Hz display with 60Hz physics target, the FPS meter shows
120 but physics runs at 60. This counter shows that "60".

If physics SPS falls below the target step rate, the machine can't
keep up and the simulation is in slow-motion.

-}

import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes exposing (style)


type Sps
    = Sps (List Sample)


type alias Sample =
    { dt : Float
    , substeps : Int
    }


init : Sps
init =
    Sps []


update : Duration -> Int -> Sps -> Sps
update dt substeps (Sps samples) =
    Sps (List.take 50 ({ dt = Duration.inMilliseconds dt, substeps = substeps } :: samples))


view : Sps -> Html a
view (Sps samples) =
    let
        totalMs =
            List.foldl (\s acc -> acc + s.dt) 0 samples

        fires =
            List.foldl (\s acc -> acc + s.substeps) 0 samples

        sps =
            if totalMs > 0 then
                round (1000 * toFloat fires / totalMs)

            else
                0
    in
    Html.div
        [ style "position" "fixed"
        , style "font-family" "monospaced"
        , style "right" "250px"
        , style "top" "60px"
        , style "color" "white"
        ]
        [ Html.span [ style "font" "50px sans-serif" ]
            [ Html.text (String.fromInt sps) ]
        , Html.text " sps"
        ]
