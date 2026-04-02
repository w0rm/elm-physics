module StabilityTest exposing (stability)

{-| Regression test for physics solver stability.

stackOf5: 5 boxes at exact resting positions, no initial velocity.
Simulation must stay stable (maxSpeed < 0.05) for 100000 frames.

-}

import Expect
import Physics exposing (onEarth)
import Stability.Metrics as Metrics
import Stability.Scenarios as Scenarios
import Test exposing (Test, describe, test)


stableFrames : Float -> Int -> Physics.Config id -> List ( id, Physics.Body ) -> Int
stableFrames threshold remaining config bodies =
    stableFramesHelp threshold remaining config bodies 0


stableFramesHelp : Float -> Int -> Physics.Config id -> List ( id, Physics.Body ) -> Int -> Int
stableFramesHelp threshold remaining config bodies count =
    if remaining <= 0 then
        count

    else
        let
            ( next, newContacts ) =
                Physics.simulate config bodies
        in
        if (Metrics.compute next).maxSpeed >= threshold then
            count

        else
            stableFramesHelp threshold (remaining - 1) { config | contacts = newContacts } next (count + 1)


coldStableFrames : Float -> Int -> Physics.Config id -> List ( id, Physics.Body ) -> Int
coldStableFrames threshold remaining config bodies =
    coldStableFramesHelp threshold remaining config bodies 0


coldStableFramesHelp : Float -> Int -> Physics.Config id -> List ( id, Physics.Body ) -> Int -> Int
coldStableFramesHelp threshold remaining config bodies count =
    if remaining <= 0 then
        count

    else
        let
            ( next, _ ) =
                Physics.simulate config bodies
        in
        if (Metrics.compute next).maxSpeed >= threshold then
            count

        else
            coldStableFramesHelp threshold (remaining - 1) config next (count + 1)


stability : Test
stability =
    describe "Stability benchmarks"
        [ test "stack of 5 boxes with contacts at 13 iterations: 100000 frames" <|
            \_ ->
                let
                    config =
                        { onEarth | solverIterations = 13 }

                    ( bodies, newContacts ) =
                        Physics.simulate config Scenarios.stackOf5.bodies
                in
                stableFrames 0.05 100000 { config | contacts = newContacts } bodies
                    |> Expect.equal 100000
        , test "stack of 5 boxes without contacts at 25 iterations: 100000 frames" <|
            \_ ->
                let
                    config =
                        { onEarth | solverIterations = 25 }

                    ( bodies, _ ) =
                        Physics.simulate config Scenarios.stackOf5.bodies
                in
                coldStableFrames 0.05 100000 config bodies
                    |> Expect.equal 100000
        ]
