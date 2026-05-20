module Common.Sps exposing (Sps, init, sps, update)

{-| Tracks how often physics actually advances when the simulation is
gated by `Timestep`. The display FPS lies about physics work — at 120Hz
display with 60Hz physics target, the FPS meter shows 120 but physics
runs at 60. This counter shows that "60".

If physics SPS falls below the target step rate, the machine can't keep
up and the simulation is in slow-motion.

-}

import Duration exposing (Duration)


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


sps : Sps -> Int
sps (Sps samples) =
    let
        totalMs =
            List.foldl (\s acc -> acc + s.dt) 0 samples

        fires =
            List.foldl (\s acc -> acc + s.substeps) 0 samples
    in
    if totalMs > 0 then
        round (1000 * toFloat fires / totalMs)

    else
        0
