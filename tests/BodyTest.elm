module BodyTest exposing (boundingSphereRadius, invInertia)

import Expect
import Extra.Expect as Expect
import Internal.Body as Body
import Internal.Const as Const
import Internal.Shape as Shape exposing (Shape)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Physics.Coordinates exposing (BodyCoordinates)
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


boundingSphereRadius : Test
boundingSphereRadius =
    describe "Body.boundingSphereRadius"
        [ test "is set to zero by default" <|
            \_ ->
                Expect.equal 0 (Body.compound [] () |> .boundingSphereRadius)
        , test "addShape computes the bounding sphere radius" <|
            \_ ->
                Body.compound [ box 2 2 2 ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 1, y = 1, z = 1 })
        , test "addShape expands the bounding sphere radius" <|
            \_ ->
                Body.compound [ box 2 2 2, box 4 4 4 ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 2, y = 2, z = 2 })
        , test "addShape sets the bounding sphere radius to maxNumber for a plane shape" <|
            \_ ->
                Body.compound [ plane ] ()
                    |> .boundingSphereRadius
                    |> Expect.atLeast Const.maxNumber
        ]


invInertia : Test
invInertia =
    describe ".invInertia"
        [ test "compound body out of two cubes has the same invInertia as a block with twice the length" <|
            \_ ->
                Expect.mat3
                    (Body.compound
                        [ Convex.fromBlock 2 2 2
                            |> Convex.placeIn (Transform3d.atPoint { x = -1, y = 0, z = 0 })
                            |> Shape.Convex
                        , Convex.fromBlock 2 2 2
                            |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })
                            |> Shape.Convex
                        ]
                        ()
                    ).invInertia
                    (Body.compound [ box 4 2 2 ] ()).invInertia
        ]


box : Float -> Float -> Float -> Shape BodyCoordinates
box sizeX sizeY sizeZ =
    Shape.Convex (Convex.fromBlock sizeX sizeY sizeZ)


plane : Shape BodyCoordinates
plane =
    Shape.Plane { position = Vec3.zero, normal = Vec3.zAxis }
