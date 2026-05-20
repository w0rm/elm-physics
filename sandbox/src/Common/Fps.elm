module Common.Fps exposing (fps, update)

{-| Tracks frame deltas to compute a weighted-average FPS. The display
itself is rendered by `Common.Demo` so all stats share one panel.
-}

import Duration exposing (Duration)


update : Duration -> List Float -> List Float
update dt frames =
    List.take 50 (Duration.inMilliseconds dt :: frames)


fps : List Float -> Float
fps frames =
    let
        average currentWeight sumOfWeights weightedSum list =
            case list of
                [] ->
                    if sumOfWeights > 0 then
                        weightedSum / sumOfWeights

                    else
                        0

                el :: rest ->
                    average
                        (currentWeight * 0.9)
                        (currentWeight + sumOfWeights)
                        (el * currentWeight + weightedSum)
                        rest

        avgMs =
            average 1 0 0 frames
    in
    if avgMs > 0 then
        1000 / avgMs

    else
        0
