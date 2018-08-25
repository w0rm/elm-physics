module Body exposing (boundingSphereRadius, box)

import Expect exposing (Expectation)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Body as Body exposing (Body)
import Physics.Const as Const
import Physics.ConvexPolyhedron as ConvexPolyhedron
import Physics.Shape as Shape exposing (Shape)
import Test exposing (..)


boundingSphereRadius : Test
boundingSphereRadius =
    describe "Body.boundingSphereRadius"
        [ test "is set to zero by default" <|
            \_ ->
                Expect.equal 0 (Body.body |> .boundingSphereRadius)
        , test "addShape computes the bounding sphere radius" <|
            \_ ->
                Body.body
                    |> Body.addShape (box (vec3 1 1 1))
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length (vec3 1 1 1))
        , test "addShape expands the bounding sphere radius" <|
            \_ ->
                Body.body
                    |> Body.addShape (box (vec3 1 1 1))
                    |> Body.addShape (box (vec3 2 2 2))
                    |> .boundingSphereRadius
                    |> Expect.within (Expect.Absolute 0.00001) (Vec3.length (vec3 2 2 2))
        , test "addShape sets the bounding sphere radius to maxNumber for a plane shape" <|
            \_ ->
                Body.body
                    |> Body.addShape Shape.Plane
                    |> .boundingSphereRadius
                    |> Expect.atLeast Const.maxNumber
        ]


box : Vec3 -> Shape
box halfExtends =
    Shape.Convex (ConvexPolyhedron.fromBox halfExtends)
