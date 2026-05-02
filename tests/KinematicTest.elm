module KinematicTest exposing (integration)

{-| Tests that a kinematic body's transform advances by velocity × dt
each simulation step, and that contacts with dynamic bodies use the
kinematic's velocity for friction (so dynamic bodies riding a moving
kinematic platform don't slide off).
-}

import Expect
import Length
import Physics exposing (onEarth)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d
import Sphere3d
import Test exposing (Test, describe, test)
import Vector3d


{-| Kinematic bodies translate by velocity × dt and rotate by
angularVelocity × dt each simulation step, regardless of gravity or
contacts. The user-set velocity is preserved across the step.
-}
integration : Test
integration =
    describe "Physics.kinematic"
        [ test "translates by velocity * dt each step" <|
            \_ ->
                let
                    floor =
                        Physics.plane Plane3d.xy Material.wood

                    platform =
                        Physics.kinematic
                            [ ( Shape.sphere (Sphere3d.atOrigin (Length.meters 0.5))
                              , Material.wood
                              )
                            ]
                            |> Physics.moveTo (Point3d.meters 0 0 5)
                            |> Physics.setVelocityTo
                                (Vector3d.metersPerSecond 1 0 0)

                    ( simulated, _ ) =
                        Physics.simulate onEarth
                            [ ( "floor", floor ), ( "platform", platform ) ]

                    platformAfter =
                        simulated
                            |> List.filter (\( id, _ ) -> id == "platform")
                            |> List.head
                            |> Maybe.map Tuple.second
                in
                case platformAfter of
                    Just b ->
                        let
                            origin =
                                Point3d.toMeters (Physics.originPoint b)
                        in
                        Expect.all
                            [ \_ -> origin.x |> Expect.within (Expect.Absolute 0.0001) (1 / 60)
                            , \_ -> origin.z |> Expect.within (Expect.Absolute 0.0001) 5
                            ]
                            ()

                    Nothing ->
                        Expect.fail "platform missing from simulation output"
        , test "preserves velocity (gravity does not affect a kinematic body)" <|
            \_ ->
                let
                    platform =
                        Physics.kinematic
                            [ ( Shape.sphere (Sphere3d.atOrigin (Length.meters 0.5))
                              , Material.wood
                              )
                            ]
                            |> Physics.setVelocityTo
                                (Vector3d.metersPerSecond 1 0 0)

                    ( simulated, _ ) =
                        Physics.simulate onEarth
                            [ ( "platform", platform ) ]

                    platformAfter =
                        simulated
                            |> List.head
                            |> Maybe.map Tuple.second
                in
                case platformAfter of
                    Just b ->
                        let
                            v =
                                Vector3d.unwrap (Physics.velocity b)
                        in
                        Expect.all
                            [ \_ -> v.x |> Expect.within (Expect.Absolute 0.0001) 1
                            , \_ -> v.y |> Expect.within (Expect.Absolute 0.0001) 0
                            , \_ -> v.z |> Expect.within (Expect.Absolute 0.0001) 0
                            ]
                            ()

                    Nothing ->
                        Expect.fail "platform missing from simulation output"
        , test "moveTo does not affect velocity (teleport keeps the kinematic moving)" <|
            \_ ->
                let
                    platform =
                        Physics.kinematic
                            [ ( Shape.sphere (Sphere3d.atOrigin (Length.meters 0.5))
                              , Material.wood
                              )
                            ]
                            |> Physics.setVelocityTo
                                (Vector3d.metersPerSecond 2 0 0)
                            |> Physics.moveTo (Point3d.meters 100 0 5)

                    v =
                        Vector3d.unwrap (Physics.velocity platform)
                in
                v.x |> Expect.within (Expect.Absolute 0.0001) 2
        , test "static bodies still ignore setVelocityTo" <|
            \_ ->
                let
                    wall =
                        Physics.static
                            [ ( Shape.sphere (Sphere3d.atOrigin (Length.meters 0.5))
                              , Material.wood
                              )
                            ]
                            |> Physics.setVelocityTo
                                (Vector3d.metersPerSecond 5 0 0)

                    v =
                        Vector3d.unwrap (Physics.velocity wall)
                in
                v.x |> Expect.within (Expect.Absolute 0.0001) 0
        ]
