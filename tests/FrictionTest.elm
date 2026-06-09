module FrictionTest exposing (suite)

{-| Real-world Coulomb friction validation, end-to-end through the public
`Physics` API. A box rests on a floor of the same material; combined friction is
the geometric mean √(f·f) = f.

Two classic experiments:

  - **Flat floor, horizontal push.** Max friction is Fmax = μ·m·g. A push below
    Fmax leaves the box at rest; a push above it accelerates the box at
    a = F/m − μg (Newton's second law with kinetic friction).

  - **Inclined contact** (modelled by tilting gravity by θ over a flat floor,
    which is physically identical to a ramp). The box holds iff tanθ < μ, i.e.
    below the critical angle arctan(μ). Above it, the box slides down at
    a = g(sinθ − μcosθ).

These are sensitive to the friction solver coupling friction to the _actual_
normal impulse: on an incline N = mg·cosθ, not mg, so a regression that bases
friction on gravity alone would break the critical-angle and slide-acceleration
checks.

-}

import Acceleration
import Block3d
import Expect
import Frame3d
import Length
import Mass
import Physics exposing (Body, onEarth)
import Physics.Material as Material exposing (Material)
import Physics.Types exposing (Material(..))
import Plane3d
import Point3d
import Test exposing (Test, describe, test)
import Vector3d


g : Float
g =
    9.80665


boxMass : Float
boxMass =
    10


{-| Scene: a 1×1×1 box resting on a flat floor, both of the given material.
-}
floorOf : Material any -> ( Int, Body )
floorOf material =
    ( 0, Physics.plane Plane3d.xy material )


boxOf : Material Material.Dense -> ( Int, Body )
boxOf material =
    ( 1
    , Physics.block
        (Block3d.centeredOn Frame3d.atOrigin
            ( Length.meters 1, Length.meters 1, Length.meters 1 )
        )
        material
        |> Physics.scaleMassTo (Mass.kilograms boxMass)
        -- half-height 0.5, so the bottom face rests on z = 0
        |> Physics.moveTo (Point3d.meters 0 0 0.5)
    )


scene : Material Material.Dense -> List ( Int, Body )
scene material =
    [ floorOf material, boxOf material ]


{-| Combined friction for two surfaces of the same material: √(f·f) = f.
-}
frictionOf : Material kind -> Float
frictionOf (Material m) =
    m.friction



-- Simulation helpers


{-| Run `n` steps with the given gravity, re-applying force `( fx, fz )`
(newtons, world x and z) at the box's center of mass every frame so it produces
no torque. Threads the contact cache for solver stability.
-}
run :
    Vector3d.Vector3d Acceleration.MetersPerSecondSquared Physics.WorldCoordinates
    -> ( Float, Float )
    -> Int
    -> List ( Int, Body )
    -> Physics.Contacts Int
    -> List ( Int, Body )
run gravity (( fx, fz ) as force) n bodies contacts =
    if n <= 0 then
        bodies

    else
        let
            pushed =
                List.map
                    (\( id, body ) ->
                        if id == 1 && force /= ( 0, 0 ) then
                            ( id
                            , Physics.applyForce
                                (Vector3d.newtons fx 0 fz)
                                (Physics.originPoint body)
                                body
                            )

                        else
                            ( id, body )
                    )
                    bodies

            ( newBodies, newContacts ) =
                Physics.simulate
                    { onEarth | gravity = gravity, contacts = contacts }
                    pushed
        in
        run gravity force (n - 1) newBodies newContacts


earth : Vector3d.Vector3d Acceleration.MetersPerSecondSquared Physics.WorldCoordinates
earth =
    Vector3d.gees 0 0 -1


{-| Gravity of magnitude 1 gee, tilted `θ` degrees from straight down in the x–z
plane — equivalent to standing the box on a ramp of slope θ.
-}
tilted : Float -> Vector3d.Vector3d Acceleration.MetersPerSecondSquared Physics.WorldCoordinates
tilted degs =
    let
        r =
            degrees degs
    in
    Vector3d.gees (sin r) 0 -(cos r)


boxVx : List ( Int, Body ) -> Float
boxVx bodies =
    bodies
        |> List.filter (\( id, _ ) -> id == 1)
        |> List.head
        |> Maybe.map (\( _, b ) -> (Vector3d.unwrap (Physics.velocity b)).x)
        -- NaN if the box is missing, so a silent drop fails the test loudly
        |> Maybe.withDefault (0 / 0)


{-| Final +x velocity after pushing a settled wood box on flat ground for half a
second (30 frames) with horizontal force `fx` and vertical force `fz` (newtons).
-}
pushVxFz : Float -> Float -> Float
pushVxFz fx fz =
    scene Material.wood
        |> (\s -> run earth ( 0, 0 ) 120 s Physics.emptyContacts)
        |> (\s -> run earth ( fx, fz ) 30 s Physics.emptyContacts)
        |> boxVx


{-| Final +x velocity after a purely horizontal push of `fx` newtons.
-}
pushVx : Float -> Float
pushVx fx =
    pushVxFz fx 0


{-| Final +x velocity of a box on an incline of `degs`, after `frames`.
-}
slideVx : Material Material.Dense -> Float -> Int -> Float
slideVx material degs frames =
    scene material
        |> (\s -> run (tilted degs) ( 0, 0 ) frames s Physics.emptyContacts)
        |> boxVx


{-| Flat-floor tests: horizontal push against gravity-only normal load.
-}
flat : Test
flat =
    let
        fMax =
            frictionOf Material.wood * boxMass * g
    in
    describe "flat floor, horizontal push (wood on wood, μ = 0.4)"
        [ test "settles to rest under gravity alone" <|
            \_ ->
                let
                    vx =
                        pushVx 0
                in
                (abs vx < 0.005)
                    |> Expect.equal True
                    |> Expect.onFail ("settled box drifted: vx = " ++ String.fromFloat vx)
        , test "push at half Fmax: static friction holds it put" <|
            \_ ->
                let
                    vx =
                        pushVx (0.5 * fMax)
                in
                (abs vx < 0.005)
                    |> Expect.equal True
                    |> Expect.onFail ("box crept under sub-friction push: vx = " ++ String.fromFloat vx)
        , test "push at twice Fmax: accelerates at a = F/m − μg" <|
            \_ ->
                let
                    force =
                        2 * fMax

                    vx =
                        pushVx force

                    -- a = (F − μmg)/m, integrated over 30 frames = 0.5 s
                    expected =
                        (force / boxMass - frictionOf Material.wood * g) * 0.5
                in
                Expect.within (Expect.Absolute (0.05 * expected)) expected vx
        ]


{-| The friction clip is μ·N where N is the actual normal impulse — which the
solver builds from the projection of _all_ external forces onto the contact
normal (gravity AND applyForce), not gravity alone. So pressing the box harder
into the floor must increase its grip. We push horizontally at 1.5·Fmax (enough
to slide it under gravity alone) and vary an applied vertical force.
-}
normalLoad : Test
normalLoad =
    let
        fMax =
            frictionOf Material.wood * boxMass * g

        slidePush =
            1.5 * fMax
    in
    describe "applied force feeds the friction clip, not just gravity"
        [ test "horizontal 1.5·Fmax alone: box slides" <|
            \_ ->
                let
                    vx =
                        pushVxFz slidePush 0
                in
                (vx > 0.3)
                    |> Expect.equal True
                    |> Expect.onFail ("box did not slide under gravity-only normal: vx = " ++ String.fromFloat vx)
        , test "same push, but pressed down hard: extra normal force holds it put" <|
            \_ ->
                let
                    -- +100 N down raises Fmax to 0.4·(98+100) ≈ 79 N > 58.8 N push
                    vx =
                        pushVxFz slidePush -100
                in
                (abs vx < 0.01)
                    |> Expect.equal True
                    |> Expect.onFail ("down-force did not raise the friction clip: vx = " ++ String.fromFloat vx)
        , test "same push, but lifted: reduced normal force slides it faster" <|
            \_ ->
                let
                    -- +100 N up nearly unloads the contact, collapsing the clip
                    vx =
                        pushVxFz slidePush 100
                in
                (vx > pushVxFz slidePush 0)
                    |> Expect.equal True
                    |> Expect.onFail ("up-force did not lower the friction clip: vx = " ++ String.fromFloat vx)
        ]


{-| Inclined-contact tests: critical angle and kinetic slide on a ramp.
-}
incline : Test
incline =
    let
        -- arctan(0.4) ≈ 21.8°
        critical =
            atan (frictionOf Material.wood) * 180 / pi
    in
    describe "inclined contact (wood on wood, critical angle ≈ 21.8°)"
        [ test "below the critical angle (15°): box holds" <|
            \_ ->
                let
                    vx =
                        slideVx Material.wood 15 200
                in
                (abs vx < 0.005)
                    |> Expect.equal True
                    |> Expect.onFail ("box slid below critical angle: vx = " ++ String.fromFloat vx)
        , test "above the critical angle (24°): box slides down" <|
            \_ ->
                let
                    vx =
                        slideVx Material.wood 24 200
                in
                (vx > 0.5)
                    |> Expect.equal True
                    |> Expect.onFail ("box failed to slide above critical angle: vx = " ++ String.fromFloat vx)
        , test "the measured critical angle matches arctan(μ) within 1°" <|
            \_ ->
                let
                    -- search the smallest whole-ish angle at which it breaks free
                    holds degs =
                        abs (slideVx Material.wood degs 200) < 0.01

                    breakaway =
                        List.filter (\d -> not (holds d))
                            (List.map (\i -> 18 + toFloat i * 0.5) (List.range 0 12))
                            |> List.minimum
                            |> Maybe.withDefault (0 / 0)
                in
                Expect.within (Expect.Absolute 1) critical breakaway
        , test "kinetic slide at 30° matches a = g(sinθ − μcosθ)" <|
            \_ ->
                let
                    r =
                        degrees 30

                    -- differential acceleration over frames 100→200, past the
                    -- breakaway transient
                    v1 =
                        slideVx Material.wood 30 100

                    v2 =
                        slideVx Material.wood 30 200

                    measured =
                        (v2 - v1) / (100 / 60)

                    expected =
                        g * (sin r - frictionOf Material.wood * cos r)
                in
                Expect.within (Expect.Absolute (0.05 * expected)) expected measured
        ]


{-| Material comparison: the combined-friction geometric mean in action.
-}
materials : Test
materials =
    test "at 10°, an ice box slides where a wood box holds" <|
        \_ ->
            let
                -- wood: critical 21.8°, so holds at 10°
                woodVx =
                    slideVx Material.wood 10 200

                -- ice: μ = 0.03, critical arctan(0.03) ≈ 1.7°, so slides at 10°
                iceVx =
                    slideVx Material.ice 10 200
            in
            Expect.all
                [ \_ ->
                    (abs woodVx < 0.005)
                        |> Expect.equal True
                        |> Expect.onFail ("wood box slid at 10°: vx = " ++ String.fromFloat woodVx)
                , \_ ->
                    (iceVx > 1)
                        |> Expect.equal True
                        |> Expect.onFail ("ice box did not slide at 10°: vx = " ++ String.fromFloat iceVx)
                ]
                ()


suite : Test
suite =
    describe "Coulomb friction validation"
        [ flat
        , normalLoad
        , incline
        , materials
        ]
