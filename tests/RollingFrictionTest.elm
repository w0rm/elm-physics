module RollingFrictionTest exposing (suite)

{-| Rolling resistance for the exactly-round shapes: a rolling ball must slow
down without skidding and eventually rest, slippery balls must roll much
farther than grippy ones, and a resting ball must stay put.
-}

import AngularSpeed
import Cylinder3d
import Direction3d
import Expect
import Internal.NarrowPhase
import Length
import Physics exposing (Body, onEarth)
import Physics.Material as Material exposing (Material)
import Plane3d
import Point3d
import Speed
import Sphere3d
import Test exposing (Test, describe, test)
import Vector3d


floor : Material any -> Body
floor material =
    Physics.plane Plane3d.xy material


{-| A radius 0.5 ball resting on the floor, rolling along x without slipping
at 1 m/s.
-}
rollingBall : Material Material.Dense -> Body
rollingBall material =
    Physics.sphere (Sphere3d.withRadius (Length.meters 0.5) (Point3d.meters 0 0 0.5)) material
        |> Physics.setVelocityTo (Vector3d.metersPerSecond 1 0 0)
        |> Physics.setAngularVelocityTo (Vector3d.xyz (AngularSpeed.radiansPerSecond 0) (AngularSpeed.radiansPerSecond 2) (AngularSpeed.radiansPerSecond 0))


run : Int -> List ( Int, Body ) -> List ( Int, Body )
run steps bodies =
    loop steps Physics.emptyContacts bodies


loop : Int -> Physics.Contacts Int -> List ( Int, Body ) -> List ( Int, Body )
loop steps contacts bodies =
    if steps <= 0 then
        bodies

    else
        let
            ( nextBodies, nextContacts ) =
                Physics.simulate { onEarth | contacts = contacts } bodies
        in
        loop (steps - 1) nextContacts nextBodies


speedOf : Int -> List ( Int, Body ) -> Float
speedOf wanted bodies =
    bodies
        |> List.filter (\( id, _ ) -> id == wanted)
        |> List.head
        |> Maybe.map (\( _, body ) -> Speed.inMetersPerSecond (Vector3d.length (Physics.velocity body)))
        |> Maybe.withDefault (1 / 0)


spinOf : Int -> List ( Int, Body ) -> Float
spinOf wanted bodies =
    bodies
        |> List.filter (\( id, _ ) -> id == wanted)
        |> List.head
        |> Maybe.map (\( _, body ) -> AngularSpeed.inRadiansPerSecond (Vector3d.length (Physics.angularVelocity body)))
        |> Maybe.withDefault (1 / 0)


suite : Test
suite =
    describe "rolling friction"
        [ describe "rolling radius"
            [ test "uses the round shape's radius against a facetted shape" <|
                \_ ->
                    Expect.all
                        [ \_ -> Internal.NarrowPhase.rollingRadius 0.5 0 |> Expect.within (Expect.Absolute 0.000001) 0.5
                        , \_ -> Internal.NarrowPhase.rollingRadius 0 0.5 |> Expect.within (Expect.Absolute 0.000001) 0.5
                        ]
                        ()
            , test "uses the smaller radius when both shapes are round" <|
                \_ ->
                    Expect.all
                        [ \_ -> Internal.NarrowPhase.rollingRadius 0.5 2 |> Expect.within (Expect.Absolute 0.000001) 0.5
                        , \_ -> Internal.NarrowPhase.rollingRadius 2 0.5 |> Expect.within (Expect.Absolute 0.000001) 0.5
                        ]
                        ()
            , test "disables rolling when neither shape has a rolling radius" <|
                \_ ->
                    Internal.NarrowPhase.rollingRadius 0 0
                        |> Expect.within (Expect.Absolute 0.000001) 0
            ]
        , test "a rolling wooden ball slows down" <|
            \_ ->
                run 120 [ ( 0, floor Material.wood ), ( 1, rollingBall Material.wood ) ]
                    |> speedOf 1
                    |> Expect.all
                        [ Expect.atMost 0.75
                        , Expect.atLeast 0.65
                        ]
        , test "a rolling ball on ice barely slows" <|
            \_ ->
                run 120 [ ( 0, floor Material.ice ), ( 1, rollingBall Material.ice ) ]
                    |> speedOf 1
                    |> Expect.all
                        [ Expect.atMost 0.99
                        , Expect.atLeast 0.93
                        ]
        , test "a rolling wooden ball comes to rest" <|
            \_ ->
                run 600 [ ( 0, floor Material.wood ), ( 1, rollingBall Material.wood ) ]
                    |> speedOf 1
                    |> Expect.atMost 0.001
        , test "a resting ball stays at rest" <|
            \_ ->
                run 300
                    [ ( 0, floor Material.wood )
                    , ( 1, Physics.sphere (Sphere3d.withRadius (Length.meters 0.5) (Point3d.meters 0 0 0.5)) Material.wood )
                    ]
                    |> speedOf 1
                    |> Expect.atMost 0.000001
        , test "a ball spinning about the normal stops spinning" <|
            \_ ->
                -- a point contact has no lever arm, so only the rolling
                -- budget in the twist cone brakes this
                let
                    spinningBall =
                        Physics.sphere (Sphere3d.withRadius (Length.meters 0.5) (Point3d.meters 0 0 0.5)) Material.wood
                            |> Physics.setAngularVelocityTo (Vector3d.xyz (AngularSpeed.radiansPerSecond 0) (AngularSpeed.radiansPerSecond 0) (AngularSpeed.radiansPerSecond 2))

                    scene =
                        [ ( 0, floor Material.wood ), ( 1, spinningBall ) ]
                in
                Expect.all
                    [ \_ -> run 60 scene |> spinOf 1 |> Expect.all [ Expect.atMost 1.1, Expect.atLeast 0.95 ]
                    , \_ -> run 300 scene |> spinOf 1 |> Expect.atMost 0.001
                    ]
                    ()
        , test "a rolling capsule slows down" <|
            \_ ->
                run 120
                    [ ( 0, floor Material.wood )
                    , ( 1
                      , Physics.capsule
                            (Cylinder3d.centeredOn (Point3d.meters 0 0 0.5)
                                Direction3d.x
                                { radius = Length.meters 0.5, length = Length.meters 1 }
                            )
                            Material.wood
                            |> Physics.setVelocityTo (Vector3d.metersPerSecond 0 1 0)
                            |> Physics.setAngularVelocityTo (Vector3d.xyz (AngularSpeed.radiansPerSecond -2) (AngularSpeed.radiansPerSecond 0) (AngularSpeed.radiansPerSecond 0))
                      )
                    ]
                    |> speedOf 1
                    |> Expect.all
                        [ Expect.atMost 0.78
                        , Expect.atLeast 0.65
                        ]
        ]
