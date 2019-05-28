module BodyTest exposing (boundingSphereRadius, box)

import Expect exposing (Expectation)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.Convex as Convex
import Internal.Quaternion as Quaternion
import Internal.Shape as Shape exposing (Shape)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Test exposing (..)


boundingSphereRadius : Test
boundingSphereRadius =
    describe "Body.boundingSphereRadius"
        [ test "is set to zero by default" <|
            \_ ->
                Expect.equal 0 (Body.compound [] () |> .boundingSphereRadius)
        , test "addShape computes the bounding sphere radius" <|
            \_ ->
                Body.compound [ box { x = 1, y = 1, z = 1 } ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 1, y = 1, z = 1 })
        , test "addShape expands the bounding sphere radius" <|
            \_ ->
                Body.compound [ box { x = 1, y = 1, z = 1 }, box { x = 2, y = 2, z = 2 } ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length { x = 2, y = 2, z = 2 })
        , test "addShape sets the bounding sphere radius to maxNumber for a plane shape" <|
            \_ ->
                Body.compound [ plane ] ()
                    |> .boundingSphereRadius
                    |> Expect.atLeast Const.maxNumber
        ]


box : Vec3 -> Shape
box halfExtends =
    { position = Vec3.zero
    , orientation = Quaternion.identity
    , kind = Shape.Convex (Convex.fromBox halfExtends)
    }


plane : Shape
plane =
    { position = Vec3.zero
    , orientation = Quaternion.identity
    , kind = Shape.Plane
    }
