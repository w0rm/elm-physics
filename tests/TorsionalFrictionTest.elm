module TorsionalFrictionTest exposing (suite)

{-| Central-friction validation, end-to-end through the public `Physics` API:

  - **Torsional friction.** A body spinning about the contact normal is braked
    by a torque of at most μ·N·r̄, where r̄ is the normal-load-weighted lever arm
    of the contact points around the friction center. For a flat-resting cube
    all four corners share r̄, so the spin decelerates uniformly at
    α = μ·m·g·r̄/Iz until it stops. A sphere's point contact has r̄ = 0, so it
    spins on undisturbed.

  - **Circular Coulomb cone.** The tangent friction cap is |F| ≤ μ·N regardless
    of direction. A per-axis clamp would hold a diagonal push up to √2·μ·N;
    the circular cone must let it break away just above μ·N, and kinetic slide
    speed must be direction-independent.

-}

import AngularSpeed
import Block3d
import Expect
import Frame3d
import Length
import Mass
import Physics exposing (Body, onEarth)
import Physics.Material as Material
import Plane3d
import Point3d
import Sphere3d
import Test exposing (Test, describe, test)
import Vector3d


g : Float
g =
    9.80665


bodyMass : Float
bodyMass =
    10


{-| Combined wood-on-wood friction: √(0.4·0.4).
-}
mu : Float
mu =
    0.4


floorBody : ( Int, Body )
floorBody =
    ( 0, Physics.plane Plane3d.xy Material.wood )


{-| A 1×1×1 cube with its bottom face on z = 0.
-}
cube : ( Int, Body )
cube =
    ( 1
    , Physics.block
        (Block3d.centeredOn Frame3d.atOrigin
            ( Length.meters 1, Length.meters 1, Length.meters 1 )
        )
        Material.wood
        |> Physics.scaleMassTo (Mass.kilograms bodyMass)
        |> Physics.moveTo (Point3d.meters 0 0 0.5)
    )


ball : ( Int, Body )
ball =
    ( 1
    , Physics.sphere
        (Sphere3d.atPoint (Point3d.meters 0 0 0.5) (Length.meters 0.5))
        Material.wood
        |> Physics.scaleMassTo (Mass.kilograms bodyMass)
    )



-- Simulation helpers


{-| Run `n` steps, re-applying a horizontal force `( fx, fy )` (newtons) at the
body's center of mass every frame. Threads the contact cache.
-}
run : ( Float, Float ) -> Int -> ( List ( Int, Body ), Physics.Contacts Int ) -> ( List ( Int, Body ), Physics.Contacts Int )
run (( fx, fy ) as force) n ( bodies, contacts ) =
    if n <= 0 then
        ( bodies, contacts )

    else
        let
            pushed =
                List.map
                    (\( id, body ) ->
                        if id == 1 && force /= ( 0, 0 ) then
                            ( id
                            , Physics.applyForce
                                (Vector3d.newtons fx fy 0)
                                (Physics.originPoint body)
                                body
                            )

                        else
                            ( id, body )
                    )
                    bodies
        in
        run force (n - 1) (Physics.simulate { onEarth | contacts = contacts } (wakeAll pushed))


{-| Wake everything before each step so sleeping can't freeze the scene
mid-measurement.
-}
wakeAll : List ( id, Body ) -> List ( id, Body )
wakeAll =
    List.map (Tuple.mapSecond Physics.wake)


settle : ( Int, Body ) -> ( List ( Int, Body ), Physics.Contacts Int )
settle movingBody =
    run ( 0, 0 ) 120 ( [ floorBody, movingBody ], Physics.emptyContacts )


setSpin : Float -> List ( Int, Body ) -> List ( Int, Body )
setSpin wz =
    List.map
        (\( id, body ) ->
            if id == 1 then
                ( id
                , Physics.setAngularVelocityTo
                    (Vector3d.xyz
                        (AngularSpeed.radiansPerSecond 0)
                        (AngularSpeed.radiansPerSecond 0)
                        (AngularSpeed.radiansPerSecond wz)
                    )
                    body
                )

            else
                ( id, body )
        )


mover : List ( Int, Body ) -> Maybe Body
mover bodies =
    bodies
        |> List.filter (\( id, _ ) -> id == 1)
        |> List.head
        |> Maybe.map Tuple.second


{-| Spin about z after settling, then `frames` more steps. NaN if the body is
missing, so a silent drop fails the test loudly.
-}
spunOmegaZ : ( Int, Body ) -> Int -> Float
spunOmegaZ body frames =
    let
        ( settled, contacts ) =
            settle body
    in
    run ( 0, 0 ) frames ( setSpin 10 settled, contacts )
        |> Tuple.first
        |> mover
        |> Maybe.map (\b -> (Vector3d.unwrap (Physics.angularVelocity b)).z)
        |> Maybe.withDefault (0 / 0)


{-| Horizontal speed after pushing the settled cube with `( fx, fy )` newtons
for half a second (30 frames).
-}
pushSpeed : ( Float, Float ) -> Float
pushSpeed force =
    settle cube
        |> run force 30
        |> Tuple.first
        |> mover
        |> Maybe.map
            (\b ->
                let
                    v =
                        Vector3d.unwrap (Physics.velocity b)
                in
                sqrt (v.x * v.x + v.y * v.y)
            )
        |> Maybe.withDefault (0 / 0)


{-| Max static friction force on the flat floor.
-}
fMax : Float
fMax =
    mu * bodyMass * g


twist : Test
twist =
    let
        -- corner contacts of the bottom face, r̄ = √2/2 from the center
        leverArm =
            sqrt 2 / 2

        -- Iz = m(w² + d²)/12
        inertiaZ =
            bodyMass * 2 / 12

        alpha =
            mu * bodyMass * g * leverArm / inertiaZ
    in
    describe "torsional friction about the contact normal"
        [ test "spinning cube decelerates at α = μ·m·g·r̄/Iz" <|
            \_ ->
                let
                    -- 15 frames = 0.25 s into the spin-down
                    expected =
                        10 - alpha * 0.25
                in
                Expect.within (Expect.Absolute 0.8) expected (spunOmegaZ cube 15)
        , test "spinning cube grinds to a halt" <|
            \_ ->
                let
                    -- α ≈ 16.6 rad/s² stops 10 rad/s in ~0.6 s; 90 frames = 1.5 s
                    wz =
                        spunOmegaZ cube 90
                in
                (abs wz < 0.05)
                    |> Expect.equal True
                    |> Expect.onFail ("cube kept spinning: wz = " ++ String.fromFloat wz)
        , test "spinning sphere keeps spinning (point contact has no lever arm)" <|
            \_ ->
                let
                    wz =
                        spunOmegaZ ball 60
                in
                (wz > 9)
                    |> Expect.equal True
                    |> Expect.onFail ("sphere lost its spin: wz = " ++ String.fromFloat wz)
        ]


circularCone : Test
circularCone =
    let
        diagonal scale =
            ( scale * fMax / sqrt 2, scale * fMax / sqrt 2 )
    in
    describe "circular Coulomb cone (direction-independent friction cap)"
        [ test "diagonal push at 0.9·Fmax holds" <|
            \_ ->
                let
                    speed =
                        pushSpeed (diagonal 0.9)
                in
                (speed < 0.005)
                    |> Expect.equal True
                    |> Expect.onFail ("cube crept under sub-friction push: speed = " ++ String.fromFloat speed)
        , test "diagonal push at 1.15·Fmax slides (a per-axis clamp holds to √2·Fmax)" <|
            \_ ->
                let
                    speed =
                        pushSpeed (diagonal 1.15)
                in
                (speed > 0.15)
                    |> Expect.equal True
                    |> Expect.onFail ("cube did not slide under diagonal push: speed = " ++ String.fromFloat speed)
        , test "kinetic slide speed matches between diagonal and axis pushes" <|
            \_ ->
                Expect.within
                    (Expect.Relative 0.25)
                    (pushSpeed ( 1.15 * fMax, 0 ))
                    (pushSpeed (diagonal 1.15))
        ]


suite : Test
suite =
    describe "central friction validation"
        [ twist
        , circularCone
        ]
