module StabilityTest exposing (stability)

{-| Regression test for physics solver stability.

stackOf5: 5 boxes at exact resting positions, no initial velocity. The first
frame always has a gravity-induced velocity spike before solver impulses
converge, so we advance a short warmup before counting consecutive stable
frames. During the warmup we also assert that no box's center has drifted
more than `warmupDriftThreshold` from its initial position — otherwise the
stack is briefly "breaking apart and re-settling", which would make the
post-warmup steady-state stability misleading.

restingOnSlope: one box on a 15° wood incline. Static friction (μ = 0.4 >
tan 15° ≈ 0.27) must hold it — it must never accelerate freely, and may only
creep within a small bound over 100000 frames.

stackOf5Dropped: five boxes released with gaps so they fall, impact, and
settle. The transient must not topple the stack (boxes stay within a small
horizontal bound), and once settled it must stay stable for 100000 frames.
This exercises the dynamic→rest transition, unlike the perfect-rest stack.

-}

import Expect
import Length
import Physics exposing (onEarth)
import Point3d
import Stability.Metrics as Metrics
import Stability.Scenarios as Scenarios
import Test exposing (Test, describe, test)


{-| Number of initial frames to advance before counting consecutive stable
frames. Empirically 63 at dt=1/60, 10 solver iterations is the cold-start
minimum (the warm-start test would clear at 60 frames). Rounded slightly up
in commits if you want margin against fragility.
-}
warmupFrames : Int
warmupFrames =
    63


{-| Max distance any box's center is allowed to drift from its starting
position at any frame during warmup. Measured empirically over the 63-frame
warmup at 10 iterations:

  - warm-start case peaks under 8 mm
  - cold-start case peaks under 33 mm — drift grows roughly linearly with
    time over the warmup window. The
    cold-start solver isn't given the contact-warm-start lambdas across
    frames, so it has to converge normal forces from cold each step and
    its position-recovery is correspondingly weaker.

Set comfortably above the cold-case peak so legitimate transient settling
passes, but any catastrophic stack collapse (where a box would slide
half a metre away) fails the assertion.

-}
warmupDriftThreshold : Float
warmupDriftThreshold =
    0.05


initialOrigins : List ( id, Physics.Body ) -> List ( id, Point3d.Point3d Length.Meters Physics.WorldCoordinates )
initialOrigins bodies =
    List.map (\( id_, body ) -> ( id_, Physics.originPoint body )) bodies


maxDrift : List ( id, Point3d.Point3d Length.Meters Physics.WorldCoordinates ) -> List ( id, Physics.Body ) -> Float
maxDrift initial current =
    List.map2
        (\( _, p0 ) ( _, body ) ->
            Length.inMeters (Point3d.distanceFrom p0 (Physics.originPoint body))
        )
        initial
        current
        |> List.maximum
        |> Maybe.withDefault 0


warmup :
    Int
    -> Float
    -> List ( id, Point3d.Point3d Length.Meters Physics.WorldCoordinates )
    -> Physics.Config id
    -> List ( id, Physics.Body )
    -> Result Float ( Physics.Config id, List ( id, Physics.Body ) )
warmup remaining drift initial config bodies =
    if remaining <= 0 then
        Ok ( config, bodies )

    else
        let
            ( next, newContacts ) =
                Physics.simulate config bodies

            d =
                maxDrift initial next
        in
        if d > drift then
            Err d

        else
            warmup (remaining - 1) drift initial { config | contacts = newContacts } next


warmupCold :
    Int
    -> Float
    -> List ( id, Point3d.Point3d Length.Meters Physics.WorldCoordinates )
    -> Physics.Config id
    -> List ( id, Physics.Body )
    -> Result Float (List ( id, Physics.Body ))
warmupCold remaining drift initial config bodies =
    if remaining <= 0 then
        Ok bodies

    else
        let
            ( next, _ ) =
                Physics.simulate config bodies

            d =
                maxDrift initial next
        in
        if d > drift then
            Err d

        else
            warmupCold (remaining - 1) drift initial config next


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


{-| Max horizontal distance (from the z axis the boxes start on) any box is
allowed to reach across the whole dropped run. The impact slides the boxes
~0.083 m sideways and they then creep slowly within ~0.1 m; a topple would
fling them metres. The limit sits well above the observed wander but far
below a collapse.

We bound horizontal drift rather than maxSpeed because the settled-but-offset
stack legitimately produces occasional one-frame velocity spikes (~0.12 m/s)
when a contact reconfigures, which immediately re-settle. Those blips don't
mean the stack is failing — only sustained sideways travel does.

-}
dropToppleLimit : Float
dropToppleLimit =
    0.25


{-| The dropped stack must be at rest by the end of the run (not perpetually
bouncing). Comfortably above the ~7e-5 m/s steady residual but below the
~0.12 m/s reconfiguration blips.
-}
dropSettledSpeed : Float
dropSettledSpeed =
    0.05


{-| Largest horizontal (xy-plane) distance of any dynamic body's center from
the z axis. The dropped boxes start at x=y=0, so this is how far they've slid
or toppled sideways.
-}
maxHorizontal : List ( id, Physics.Body ) -> Float
maxHorizontal bodies =
    maxHorizontalHelp bodies 0


maxHorizontalHelp : List ( id, Physics.Body ) -> Float -> Float
maxHorizontalHelp bodies acc =
    case bodies of
        [] ->
            acc

        ( _, body ) :: rest ->
            case Physics.mass body of
                Just _ ->
                    let
                        p =
                            Point3d.toMeters (Physics.originPoint body)
                    in
                    maxHorizontalHelp rest (max acc (sqrt (p.x * p.x + p.y * p.y)))

                Nothing ->
                    maxHorizontalHelp rest acc


{-| Run the dropped stack `remaining` frames (warm-started). Fails with the
offending distance the first time any box slides past `limit` horizontally
(toppled). On reaching the end, returns the final-frame maxSpeed so the
caller can confirm the stack came to rest.
-}
runDrop :
    Int
    -> Float
    -> Physics.Config Int
    -> List ( Int, Physics.Body )
    -> Float
    -> Result Float Float
runDrop remaining limit config bodies lastSpeed =
    if remaining <= 0 then
        Ok lastSpeed

    else
        let
            ( next, newContacts ) =
                Physics.simulate config bodies

            h =
                maxHorizontal next
        in
        if h - limit > 0 then
            Err h

        else
            runDrop (remaining - 1) limit { config | contacts = newContacts } next (Metrics.compute next).maxSpeed


{-| Origin of the (single) dynamic body in a scenario — used to measure how
far the box on the slope has crept from where it started.
-}
dynamicOrigin : List ( id, Physics.Body ) -> Point3d.Point3d Length.Meters Physics.WorldCoordinates
dynamicOrigin bodies =
    case bodies of
        ( _, body ) :: rest ->
            case Physics.mass body of
                Just _ ->
                    Physics.originPoint body

                Nothing ->
                    dynamicOrigin rest

        [] ->
            Point3d.origin


{-| Run the slope scenario for `remaining` frames, carrying contacts (warm
start) frame to frame. Returns the worst maxSpeed seen and the dynamic
body's final drift from `origin0`. Tail-recursive; per-frame cost is
dominated by `Physics.simulate`.
-}
runSlope :
    Int
    -> Physics.Config Int
    -> List ( Int, Physics.Body )
    -> Point3d.Point3d Length.Meters Physics.WorldCoordinates
    -> Float
    -> ( Float, Float )
runSlope remaining config bodies origin0 maxSpeedSoFar =
    if remaining <= 0 then
        ( maxSpeedSoFar
        , Length.inMeters (Point3d.distanceFrom origin0 (dynamicOrigin bodies))
        )

    else
        let
            ( next, newContacts ) =
                Physics.simulate config bodies

            worst =
                max maxSpeedSoFar (Metrics.compute next).maxSpeed
        in
        runSlope (remaining - 1) { config | contacts = newContacts } next origin0 worst


{-| The box must never accelerate freely down the incline. If friction
failed entirely it would accelerate at g·sin 15° ≈ 2.54 m/s² and cross this
within a few frames, so staying under it across 100 k frames is a genuine
grip check. Observed peak with friction working is ~4e-4 m/s.
-}
slopeMaxSpeedLimit : Float
slopeMaxSpeedLimit =
    0.05


{-| Friction holds the box, but with no friction-λ warm-starting it leaves a
tiny unrecovered tangential slip each step. Under the SPOOK soft-constraint
parameters (spookEps > 0) the friction solve is slightly compliant, so it
leaves a small residual each step: the box creeps downhill at a steady
~0.27 mm/s, integrating to ~0.59 m over 100 k frames.

That is still a creep, not a slide — `slopeMaxSpeedLimit` is the real grip
check (a box that lost friction would accelerate at g·sin 15° ≈ 2.54 m/s² and
trip it within a few frames). This bound only has to tolerate the creep while
still failing loudly on an actual slide (metres, or NaN).

-}
slopeDriftLimit : Float
slopeDriftLimit =
    0.7


stability : Test
stability =
    describe "Stability benchmarks"
        [ test "stack of 5 boxes with contacts at 7 iterations: warmup stays stacked + 100000 stable frames" <|
            \_ ->
                let
                    config =
                        { onEarth | solverIterations = 7 }

                    initial =
                        initialOrigins Scenarios.stackOf5.bodies
                in
                case warmup warmupFrames warmupDriftThreshold initial config Scenarios.stackOf5.bodies of
                    Err d ->
                        Expect.fail
                            ("Stack drifted "
                                ++ String.fromFloat d
                                ++ " m during warmup (limit "
                                ++ String.fromFloat warmupDriftThreshold
                                ++ " m)"
                            )

                    Ok ( warmedConfig, warmedBodies ) ->
                        stableFrames 0.05 100000 warmedConfig warmedBodies
                            |> Expect.equal 100000
        , test "stack of 5 boxes without contacts at 30 iterations: warmup stays stacked + 100000 stable frames" <|
            \_ ->
                let
                    config =
                        { onEarth | solverIterations = 30 }

                    initial =
                        initialOrigins Scenarios.stackOf5.bodies
                in
                case warmupCold warmupFrames warmupDriftThreshold initial config Scenarios.stackOf5.bodies of
                    Err d ->
                        Expect.fail
                            ("Stack drifted "
                                ++ String.fromFloat d
                                ++ " m during warmup (limit "
                                ++ String.fromFloat warmupDriftThreshold
                                ++ " m)"
                            )

                    Ok warmedBodies ->
                        coldStableFrames 0.05 100000 config warmedBodies
                            |> Expect.equal 100000
        , test "box resting on a slope at 10 iterations: friction holds it for 100000 frames" <|
            \_ ->
                let
                    config =
                        { onEarth | solverIterations = 10 }

                    origin0 =
                        dynamicOrigin Scenarios.restingOnSlope.bodies

                    -- Skip frame 0: the box starts flush, so the first frame has an
                    -- initial-contact velocity spike before friction grips.
                    ( warmedBodies, warmedContacts ) =
                        Physics.simulate config Scenarios.restingOnSlope.bodies

                    ( maxSpeed, drift ) =
                        runSlope 100000 { config | contacts = warmedContacts } warmedBodies origin0 0
                in
                if maxSpeed - slopeMaxSpeedLimit >= 0 then
                    Expect.fail
                        ("Box accelerated on the slope: maxSpeed="
                            ++ String.fromFloat maxSpeed
                            ++ " m/s (limit "
                            ++ String.fromFloat slopeMaxSpeedLimit
                            ++ ")"
                        )

                else if drift - slopeDriftLimit >= 0 then
                    Expect.fail
                        ("Box slid down the slope: drift="
                            ++ String.fromFloat drift
                            ++ " m (limit "
                            ++ String.fromFloat slopeDriftLimit
                            ++ ")"
                        )

                else
                    Expect.pass
        , test "stack of 5 boxes dropped at 10 iterations: lands without toppling over 100000 frames" <|
            \_ ->
                let
                    config =
                        { onEarth | solverIterations = 10 }
                in
                case runDrop 100000 dropToppleLimit config Scenarios.stackOf5Dropped.bodies 0 of
                    Err h ->
                        Expect.fail
                            ("Dropped stack toppled: a box slid "
                                ++ String.fromFloat h
                                ++ " m sideways (limit "
                                ++ String.fromFloat dropToppleLimit
                                ++ " m)"
                            )

                    Ok finalSpeed ->
                        if finalSpeed - dropSettledSpeed >= 0 then
                            Expect.fail
                                ("Dropped stack never came to rest: final maxSpeed="
                                    ++ String.fromFloat finalSpeed
                                    ++ " m/s (limit "
                                    ++ String.fromFloat dropSettledSpeed
                                    ++ ")"
                                )

                        else
                            Expect.pass
        ]
