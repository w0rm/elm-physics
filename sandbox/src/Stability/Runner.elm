module Stability.Runner exposing (consecutiveStableFrames, runN)

{-| Deterministic simulation runner for stability benchmarks.

Runs `Physics.simulate` for exactly `n` frames with a fixed timestep,
feeding each frame's output as input to the next.

-}

import Physics
import Stability.Metrics as Metrics


{-| Run the simulation for `n` fixed-timestep frames, returning the final body state.
-}
runN : Int -> Physics.Config id -> List ( id, Physics.Body ) -> List ( id, Physics.Body )
runN n config bodies =
    if n <= 0 then
        bodies

    else
        let
            ( next, _ ) =
                Physics.simulate config bodies
        in
        runN (n - 1) config next


{-| Run for up to `maxFrames` and return the number of consecutive frames
(from frame 1) where maxSpeed stays below `threshold`.

Stops at the first frame that exceeds the threshold, so the result is the
frame at which the stack first becomes unstable. Bodies should start at rest.

Higher = more stable. Warm starting should raise this above the baseline.

-}
consecutiveStableFrames : Float -> Int -> Physics.Config id -> List ( id, Physics.Body ) -> Int
consecutiveStableFrames threshold maxFrames config bodies =
    consecutiveHelper threshold maxFrames config bodies 0


consecutiveHelper : Float -> Int -> Physics.Config id -> List ( id, Physics.Body ) -> Int -> Int
consecutiveHelper threshold remaining config bodies count =
    if remaining <= 0 then
        count

    else
        let
            ( next, _ ) =
                Physics.simulate config bodies

            s =
                (Metrics.compute next).maxSpeed
        in
        if s >= threshold then
            count

        else
            consecutiveHelper threshold (remaining - 1) config next (count + 1)
