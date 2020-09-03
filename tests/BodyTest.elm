module BodyTest exposing (boundingSphereRadius, updateMassProperties)

import Expect
import Extra.Expect as Expect
import Fixtures.Convex as Convex
import Internal.Body as Body
import Internal.Const as Const
import Internal.Shape as Shape exposing (Shape)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Physics.Coordinates exposing (BodyCoordinates)
import Shapes.Convex as Convex exposing (Convex)
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


updateMassProperties : Test
updateMassProperties =
    describe "Body.updateMassProperties"
        [ test "compound body out of two cubes has the same invInertia as a block with twice the length" <|
            \_ ->
                let
                    body1 =
                        Body.compound
                            [ Convex.fromBlock 2 2 2
                                |> Convex.placeIn (Transform3d.atPoint { x = -1, y = 0, z = 0 })
                                |> Shape.Convex
                            , Convex.fromBlock 2 2 2
                                |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })
                                |> Shape.Convex
                            ]
                            ()

                    body2 =
                        Body.compound [ box 4 2 2 ] ()
                in
                Expect.mat3
                    (Body.updateMassProperties { body1 | mass = 1 }).invInertia
                    (Body.updateMassProperties { body2 | mass = 1 }).invInertia
        , test "cube box body should have the same inInertia as a compound body out of tetrahedrons" <|
            \_ ->
                let
                    body1 =
                        Body.compound
                            (Convex.blockOfTetrahedrons 2 3 1
                                |> List.map Shape.Convex
                            )
                            ()

                    body2 =
                        Body.compound
                            [ Shape.Convex (Convex.block Transform3d.atOrigin 2 3 1) ]
                            ()
                in
                Expect.mat3
                    (Body.updateMassProperties { body1 | mass = 1 }).invInertia
                    (Body.updateMassProperties { body2 | mass = 1 }).invInertia
        ]


box : Float -> Float -> Float -> Shape BodyCoordinates
box sizeX sizeY sizeZ =
    Shape.Convex (Convex.fromBlock sizeX sizeY sizeZ)


plane : Shape BodyCoordinates
plane =
    Shape.Plane { position = Vec3.zero, normal = Vec3.zAxis }
