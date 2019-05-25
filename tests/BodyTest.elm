module BodyTest exposing (boundingSphereRadius, box)

import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Expect exposing (Expectation)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.Convex as Convex
import Internal.Quaternion as Quaternion
import Internal.Shape as Shape exposing (Shape)
import Test exposing (..)


boundingSphereRadius : Test
boundingSphereRadius =
    describe "Body.boundingSphereRadius"
        [ test "is set to zero by default" <|
            \_ ->
                Expect.equal 0 (Body.compound [] () |> .boundingSphereRadius)
        , test "addShape computes the bounding sphere radius" <|
            \_ ->
                Body.compound [ box (vec3 1 1 1) ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length (vec3 1 1 1))
        , test "addShape expands the bounding sphere radius" <|
            \_ ->
                Body.compound [ box (vec3 1 1 1), box (vec3 2 2 2) ] ()
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length (vec3 2 2 2))
        , test "addShape sets the bounding sphere radius to maxNumber for a plane shape" <|
            \_ ->
                Body.compound [ plane ] ()
                    |> .boundingSphereRadius
                    |> Expect.atLeast Const.maxNumber
        ]


box : Vec3 -> Shape
box halfExtends =
    { position = vec3 0 0 0
    , orientation = Quaternion.identity
    , kind = Shape.Convex (Convex.fromBox halfExtends)
    }


plane : Shape
plane =
    { position = vec3 0 0 0
    , orientation = Quaternion.identity
    , kind = Shape.Plane
    }
