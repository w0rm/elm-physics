module BodyTest exposing (boundingSphereRadius, box)

import Expect
import Internal.Body as Body
import Internal.Const as Const
import Internal.Convex as Convex
import Internal.Shape as Shape exposing (Shape)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Physics.Coordinates exposing (BodyCoordinates)
import Test exposing (Test, describe, test)


boundingSphereRadius : Test
boundingSphereRadius =
    describe "Body.boundingSphereRadius"
        [ test "is set to zero by default" <|
            \_ ->
                Expect.equal 0 (Body.compound [] () |> .boundingSphereRadius)
        , test "addShape computes the bounding sphere radius" <|
            \_ ->
                Body.compound [ box 1 1 1 ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 1, y = 1, z = 1 })
        , test "addShape expands the bounding sphere radius" <|
            \_ ->
                Body.compound [ box 1 1 1, box 2 2 2 ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 2, y = 2, z = 2 })
        , test "addShape sets the bounding sphere radius to maxNumber for a plane shape" <|
            \_ ->
                Body.compound [ plane ] ()
                    |> .boundingSphereRadius
                    |> Expect.atLeast Const.maxNumber
        ]


box : Float -> Float -> Float -> Shape BodyCoordinates
box x y z =
    { transform3d = Transform3d.atOrigin
    , volume = x * y * z
    , kind = Shape.Convex (Convex.fromBlock x y z)
    }


plane : Shape BodyCoordinates
plane =
    { transform3d = Transform3d.atOrigin
    , volume = 0
    , kind = Shape.Plane
    }
