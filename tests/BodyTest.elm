module BodyTest exposing (boundingSphereRadius, updateMassProperties, volume)

import Expect
import Extra.Expect as Expect
import Fixtures.Convex as Convex
import Internal.Body as Body
import Internal.Const as Const
import Internal.Material as Material
import Internal.Shape as Shape exposing (Shape)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Physics exposing (BodyCoordinates)
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


boundingSphereRadius : Test
boundingSphereRadius =
    describe "Body.boundingSphereRadius"
        [ test "is set to zero by default" <|
            \_ ->
                Expect.equal 0 (Body.compound [] |> .boundingSphereRadius)
        , test "addShape computes the bounding sphere radius" <|
            \_ ->
                Body.compound [ ( box 2 2 2, Material.wood, 1 ) ]
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 1, y = 1, z = 1 })
        , test "addShape expands the bounding sphere radius" <|
            \_ ->
                Body.compound [ ( box 2 2 2, Material.wood, 1 ), ( box 4 4 4, Material.wood, 1 ) ]
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 2, y = 2, z = 2 })
        , test "addShape sets the bounding sphere radius to maxNumber for a plane shape" <|
            \_ ->
                Body.compound [ ( plane, Material.wood, 1 ) ]
                    |> .boundingSphereRadius
                    |> Expect.atLeast Const.maxNumber
        ]


updateMassProperties : Test
updateMassProperties =
    describe "Body.compound inertia"
        [ test "compound body out of two cubes has the same invInertia as a block with twice the length" <|
            \_ ->
                let
                    mat =
                        { friction = 0, bounciness = 0, density = 700 }

                    body1 =
                        Body.compound
                            [ ( Convex.fromBlock 2 2 2
                                    |> Convex.placeIn (Transform3d.atPoint { x = -1, y = 0, z = 0 })
                                    |> Shape.Convex
                              , mat
                              , 1
                              )
                            , ( Convex.fromBlock 2 2 2
                                    |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })
                                    |> Shape.Convex
                              , mat
                              , 1
                              )
                            ]

                    body2 =
                        Body.compound [ ( box 4 2 2, mat, 1 ) ]
                in
                Expect.mat3 body1.invInertia body2.invInertia
        , test "cube box body should have the same invInertia as a compound body out of tetrahedrons" <|
            \_ ->
                let
                    mat =
                        { friction = 0, bounciness = 0, density = 700 }

                    body1 =
                        Body.compound
                            (Convex.blockOfTetrahedrons 2 3 1
                                |> List.map (\s -> ( Shape.Convex s, mat, 1 ))
                            )

                    body2 =
                        Body.compound
                            [ ( Shape.Convex (Convex.block Transform3d.atOrigin 2 3 1), mat, 1 ) ]
                in
                Expect.mat3 body1.invInertia body2.invInertia
        ]


volume : Test
volume =
    describe "Body.volume"
        [ test "solid box has volume equal to its dimensions" <|
            \_ ->
                Body.compound [ ( box 2 3 4, Material.wood, 1 ) ]
                    |> .volume
                    |> Expect.within (Expect.Absolute 0.00001) (2 * 3 * 4)
        , test "hollow crate has correct mass (outer minus inner)" <|
            \_ ->
                -- mass = density × net_volume = 700 × (1 - 0.8³)
                Body.compound
                    [ ( box 1 1 1, Material.wood, 1 )
                    , ( box 0.8 0.8 0.8, Material.wood, -1 )
                    ]
                    |> .mass
                    |> Expect.within (Expect.Absolute 0.001) (700 * (1 - 0.8 ^ 3))
        , test "hollow crate subtracts void volume" <|
            \_ ->
                -- outer 1×1×1, inner void 0.8×0.8×0.8
                -- net volume = 1 - 0.512 = 0.488
                Body.compound
                    [ ( box 1 1 1, Material.wood, 1 )
                    , ( box 0.8 0.8 0.8, Material.wood, -1 )
                    ]
                    |> .volume
                    |> Expect.within (Expect.Absolute 0.00001) (1 - 0.8 ^ 3)
        , test "hollow crate has correct inertia (outer minus inner)" <|
            \_ ->
                -- For a uniform box a×a×a with density ρ:
                --   mass = ρ·a³,  Ixx = mass/12·(a²+a²) = ρ·a⁵/6
                --   invIxx = 6/(ρ·a⁵)
                -- Hollow: invIxx_net = 1/(Ixx_outer - Ixx_inner)
                --   = 1/(ρ/6·(1 - 0.8⁵)) = 6/(ρ·(1-0.8⁵))
                let
                    rho =
                        700

                    expectedInvIxx =
                        6 / (rho * (1 - 0.8 ^ 5))

                    hollow =
                        Body.compound
                            [ ( box 1 1 1, Material.wood, 1 )
                            , ( box 0.8 0.8 0.8, Material.wood, -1 )
                            ]
                in
                hollow.invInertia.m11
                    |> Expect.within (Expect.Relative 0.0001) expectedInvIxx
        ]


box : Float -> Float -> Float -> Shape BodyCoordinates
box sizeX sizeY sizeZ =
    Shape.Convex (Convex.fromBlock sizeX sizeY sizeZ)


plane : Shape BodyCoordinates
plane =
    Shape.Plane { position = Vec3.zero, normal = Vec3.zAxis }
