module SleepTest exposing (suite)

{-| End-to-end tests for body sleeping (per-island deactivation).

These guard the silent edge cases — a sleeping body must hold its pose exactly,
must wake on an applied force, must stay island-coherent across a constraint
(waking one constrained body wakes its partner), and must not be left frozen
under a kinematic platform that starts moving.
-}

import AngularSpeed
import Block3d
import Duration
import Expect
import Force
import Frame3d
import Length
import Physics exposing (Body, onEarth)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape
import Physics.Types as Types
import Plane3d
import Point3d
import Quantity
import Test exposing (Test, describe, test)
import Torque
import Vector3d


{-| The body is "asleep" once its rest-frame counter reaches the limit
(`Internal.SolverBody.sleepFrameLimit`, 60). 0 means fully awake.
-}
sleepFrames : Body -> Int
sleepFrames (Types.Body body) =
    body.sleepFrames


asleepLimit : Int
asleepLimit =
    60


unitBox : Body
unitBox =
    Physics.block
        (Block3d.centeredOn Frame3d.atOrigin
            ( Length.meters 1, Length.meters 1, Length.meters 1 )
        )
        Material.wood


floorBody : Body
floorBody =
    Physics.plane Plane3d.xy Material.wood


{-| Thread contacts through `steps` simulation calls with a plain
gravity-only config (no constraints).
-}
run : Int -> List ( Int, Body ) -> List ( Int, Body )
run steps bodies =
    runWith steps (\_ -> Nothing) bodies


runWith : Int -> (Int -> Maybe (Int -> List Constraint)) -> List ( Int, Body ) -> List ( Int, Body )
runWith steps constrain bodies =
    loop steps constrain Physics.emptyContacts bodies


loop : Int -> (Int -> Maybe (Int -> List Constraint)) -> Physics.Contacts Int -> List ( Int, Body ) -> List ( Int, Body )
loop steps constrain contacts bodies =
    if steps <= 0 then
        bodies

    else
        let
            ( nextBodies, nextContacts ) =
                Physics.simulate { onEarth | contacts = contacts, constrain = constrain } bodies
        in
        loop (steps - 1) constrain nextContacts nextBodies


lookup : Int -> List ( Int, Body ) -> Maybe Body
lookup wanted bodies =
    bodies
        |> List.filter (\( id, _ ) -> id == wanted)
        |> List.head
        |> Maybe.map Tuple.second


x : Body -> Float
x body =
    .x (Point3d.toMeters (Physics.originPoint body))


z : Body -> Float
z body =
    .z (Point3d.toMeters (Physics.originPoint body))


{-| A body resting on the floor, simulated long enough to fall asleep, then
poked with `mutate` and stepped once. The mutator must wake it: any wake
mutator resets `sleepFrames` to 0, and a body at 0 keeps its island awake (so
it cannot be frozen), so after one step it must read below the limit. If the
mutator failed to wake it, the island stays asleep and `sleepFrames` holds at
the limit.
-}
wakesWith : String -> (Body -> Body) -> Test
wakesWith name mutate =
    test name <|
        \_ ->
            let
                sleeping =
                    run 150
                        [ ( 0, floorBody )
                        , ( 1, unitBox |> Physics.moveTo (Point3d.meters 0 0 0.5) )
                        ]

                poked =
                    sleeping
                        |> List.map
                            (\( id, body ) ->
                                if id == 1 then
                                    ( id, mutate body )

                                else
                                    ( id, body )
                            )

                after =
                    run 1 poked
            in
            case ( lookup 1 sleeping, lookup 1 after ) of
                ( Just before, Just awake ) ->
                    Expect.all
                        [ \_ -> sleepFrames before |> Expect.equal asleepLimit
                        , \_ -> sleepFrames awake |> Expect.lessThan asleepLimit
                        ]
                        ()

                _ ->
                    Expect.fail "box missing from simulation output"


suite : Test
suite =
    describe "Body sleeping"
        [ test "a body resting on the floor reaches the sleep limit and then holds its pose exactly" <|
            \_ ->
                let
                    scene =
                        run 150
                            [ ( 0, floorBody )
                            , ( 1, unitBox |> Physics.moveTo (Point3d.meters 0 0 0.5) )
                            ]

                    -- one more step: an asleep body's pose is byte-identical
                    -- (it reuses its transform), an awake one micro-drifts.
                    sceneNext =
                        run 1 scene
                in
                case ( lookup 1 scene, lookup 1 sceneNext ) of
                    ( Just box, Just boxNext ) ->
                        Expect.all
                            [ \_ -> sleepFrames box |> Expect.equal asleepLimit
                            , \_ -> x boxNext |> Expect.within (Expect.Absolute 0) (x box)
                            ]
                            ()

                    _ ->
                        Expect.fail "box missing from simulation output"
        , test "an applied force wakes a sleeping body" <|
            \_ ->
                let
                    sleeping =
                        run 150
                            [ ( 0, floorBody )
                            , ( 1, unitBox |> Physics.moveTo (Point3d.meters 0 0 0.5) )
                            ]

                    pushed =
                        sleeping
                            |> List.map
                                (\( id, body ) ->
                                    if id == 1 then
                                        ( id
                                        , Physics.applyForce
                                            (Vector3d.fromTuple Force.newtons ( 5000, 0, 0 ))
                                            (Physics.originPoint body)
                                            body
                                        )

                                    else
                                        ( id, body )
                                )

                    woken =
                        run 1 pushed
                in
                case ( lookup 1 sleeping, lookup 1 woken ) of
                    ( Just before, Just after ) ->
                        Expect.all
                            [ \_ -> sleepFrames before |> Expect.equal asleepLimit
                            , \_ -> sleepFrames after |> Expect.lessThan asleepLimit
                            , \_ -> x after |> Expect.greaterThan (x before)
                            ]
                            ()

                    _ ->
                        Expect.fail "box missing from simulation output"
        , test "waking one body of a constrained pair wakes its partner (island coherence)" <|
            \_ ->
                let
                    -- two boxes 2 m apart, held at that distance by a constraint
                    constrain id1 =
                        if id1 == 1 then
                            Just
                                (\id2 ->
                                    if id2 == 2 then
                                        [ Constraint.distance (Length.meters 2) ]

                                    else
                                        []
                                )

                        else
                            Nothing

                    scene =
                        [ ( 0, floorBody )
                        , ( 1, unitBox |> Physics.moveTo (Point3d.meters 0 0 0.5) )
                        , ( 2, unitBox |> Physics.moveTo (Point3d.meters 2 0 0.5) )
                        ]

                    sleeping =
                        runWith 150 constrain scene

                    -- drive only box 1 *away* from box 2, so the distance
                    -- constraint goes taut and yanks box 2 along
                    pushed =
                        sleeping
                            |> List.map
                                (\( id, body ) ->
                                    if id == 1 then
                                        ( id
                                        , Physics.setVelocityTo
                                            (Vector3d.metersPerSecond -5 0 0)
                                            body
                                        )

                                    else
                                        ( id, body )
                                )

                    woken =
                        runWith 15 constrain pushed
                in
                case ( lookup 2 sleeping, lookup 2 woken ) of
                    ( Just box2Before, Just box2After ) ->
                        Expect.all
                            -- box 2 was asleep...
                            [ \_ -> sleepFrames box2Before |> Expect.equal asleepLimit

                            -- ...and driving only box 1 dragged box 2 along the
                            -- constraint, so box 2 was simulated, not left frozen.
                            -- (Were the constrained pair in separate islands, box
                            -- 2's island would stay asleep and its x unchanged.)
                            , \_ -> x box2After |> Expect.lessThan (x box2Before - 0.1)
                            ]
                            ()

                    _ ->
                        Expect.fail "boxes missing from simulation output"
        , test "a kinematic platform that starts moving wakes the body asleep on it" <|
            \_ ->
                let
                    platform =
                        Physics.kinematic
                            [ ( Shape.block
                                    (Block3d.centeredOn Frame3d.atOrigin
                                        ( Length.meters 6, Length.meters 6, Length.meters 1 )
                                    )
                              , Material.wood
                              )
                            ]
                            |> Physics.moveTo (Point3d.meters 0 0 -0.5)

                    scene =
                        [ ( 0, platform )
                        , ( 1, unitBox |> Physics.moveTo (Point3d.meters 0 0 0.5) )
                        ]

                    -- let the box settle and sleep on the parked platform
                    sleeping =
                        run 150 scene

                    -- now set the platform moving
                    nudged =
                        sleeping
                            |> List.map
                                (\( id, body ) ->
                                    if id == 0 then
                                        ( id
                                        , Physics.setVelocityTo
                                            (Vector3d.metersPerSecond 1 0 0)
                                            body
                                        )

                                    else
                                        ( id, body )
                                )

                    dragged =
                        run 15 nudged
                in
                case ( lookup 1 sleeping, lookup 1 dragged ) of
                    ( Just box, Just boxDragged ) ->
                        Expect.all
                            [ \_ -> sleepFrames box |> Expect.equal asleepLimit
                            , \_ -> sleepFrames boxDragged |> Expect.lessThan asleepLimit
                            , \_ -> x boxDragged |> Expect.greaterThan (x box)
                            ]
                            ()

                    _ ->
                        Expect.fail "box missing from simulation output"
        , wakesWith "an applied impulse wakes a sleeping body" <|
            \body ->
                Physics.applyImpulse
                    (Vector3d.xyz
                        (Quantity.times (Duration.seconds 1) (Force.newtons 500))
                        Quantity.zero
                        Quantity.zero
                    )
                    (Physics.originPoint body)
                    body
        , wakesWith "an applied torque wakes a sleeping body" <|
            \body ->
                Physics.applyTorque
                    (Vector3d.fromTuple Torque.newtonMeters ( 0, 0, 5000 ))
                    body
        , wakesWith "an applied angular impulse wakes a sleeping body" <|
            \body ->
                Physics.applyAngularImpulse
                    (Vector3d.xyz Quantity.zero
                        Quantity.zero
                        (Quantity.times (Duration.seconds 1) (Torque.newtonMeters 5000))
                    )
                    body
        , wakesWith "a set angular velocity wakes a sleeping body" <|
            \body ->
                Physics.setAngularVelocityTo
                    (Vector3d.fromTuple AngularSpeed.radiansPerSecond ( 0, 0, 5 ))
                    body
        , test "dragging a sleeping body with a moved static anchor + pointToPoint wakes it (the Lack-demo pattern)" <|
            \_ ->
                let
                    -- mirrors examples/Lack.elm: pin a `static` anchor to the
                    -- body with a pointToPoint constraint and move the anchor
                    constrain anchorId =
                        if anchorId == 3 then
                            Just
                                (\bodyId ->
                                    if bodyId == 1 then
                                        [ Constraint.pointToPoint Point3d.origin Point3d.origin ]

                                    else
                                        []
                                )

                        else
                            Nothing

                    sleeping =
                        run 150
                            [ ( 0, floorBody )
                            , ( 1, unitBox |> Physics.moveTo (Point3d.meters 0 0 0.5) )
                            ]

                    -- add a static drag anchor up at z = 2 and pin it to the box
                    withAnchor =
                        ( 3, Physics.static [] |> Physics.moveTo (Point3d.meters 0 0 2) )
                            :: sleeping

                    dragged =
                        runWith 15 constrain withAnchor
                in
                case ( lookup 1 sleeping, lookup 1 dragged ) of
                    ( Just box, Just boxDragged ) ->
                        Expect.all
                            [ \_ -> sleepFrames box |> Expect.equal asleepLimit
                            , \_ -> sleepFrames boxDragged |> Expect.lessThan asleepLimit
                            , \_ -> z boxDragged |> Expect.greaterThan (z box + 0.1)
                            ]
                            ()

                    _ ->
                        Expect.fail "box missing from simulation output"
        ]
