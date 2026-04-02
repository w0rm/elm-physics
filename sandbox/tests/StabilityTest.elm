module StabilityTest exposing (stability)

{-| Regression test for physics solver stability.

── What the scenario tests ──────────────────────────────────────────────────

  stackOf5
    Tests convex-convex contact stability under sustained load.
    Boxes start at exact resting positions (no drop, no initial velocity).
    One warmup frame lets the solver establish contacts before counting begins.
    consecutiveStableFrames counts consecutive frames where maxSpeed < 0.05 m/s,
    stopping at the first frame that exceeds it — the frame the stack collapses.

── Baseline (current implementation, no warm starting) ──────────────────────

  stackOf5  (up to 2000 frames):  443 consecutive stable frames (maxSpeed < 0.05 m/s)

-}

import Expect
import Physics
import Stability.Runner as Runner exposing (runN)
import Stability.Scenarios as Scenarios
import Test exposing (Test, describe, test)


stability : Test
stability =
    describe "Stability benchmarks"
        [ describe "stack of 5 boxes"
            -- Convex-convex contact stability under sustained load.
            -- Boxes at exact resting positions; maxSpeed drifts up as solver errors
            -- accumulate until the stack collapses. consecutiveStableFrames gives
            -- the frame at which that first happens.
            -- Warm starting should push this significantly higher than the baseline.
            [ test "stack stays below 0.05 m/s for at least 443 frames" <|
                \_ ->
                    Runner.consecutiveStableFrames 0.05 2000 Physics.onEarth (runN 1 Physics.onEarth Scenarios.stackOf5.bodies)
                        |> Expect.atLeast 443
            ]
        ]
