module Common.Bodies exposing
    ( DemoBody
    , box
    , fromTriangles
    , plane
    , sphere
    )

import Common.Meshes as Meshes exposing (Attributes)
import Physics.Body as Body exposing (Body)
import Physics.Material as Material exposing (Material)
import Physics.Shape as Shape exposing (Shape)
import WebGL exposing (Mesh)


type alias DemoBody =
    { mesh : Mesh Attributes
    , wireframe : Mesh Attributes
    , name : String
    }


fromTriangles : String -> List ( Attributes, Attributes, Attributes ) -> DemoBody
fromTriangles name triangles =
    { mesh = Meshes.toMesh triangles
    , wireframe = Meshes.toWireframe triangles
    , name = name
    }


{-| A plane-shaped body
-}
plane : Body DemoBody
plane =
    []
        |> fromTriangles "plane"
        |> Body.plane


{-| A cube with sides of 2 and mass of 5
-}
box : String -> { x : Float, y : Float, z : Float } -> Body DemoBody
box name boxDimensions =
    Meshes.box boxDimensions
        |> fromTriangles name
        |> Body.box boxDimensions


{-| A sphere with radius of 1.2 and mass of 5
-}
sphere : String -> Float -> Body DemoBody
sphere name radius =
    Meshes.sphere 2 radius
        |> fromTriangles name
        |> Body.sphere radius
        |> Body.setMass 5


sphereRadius : Float
sphereRadius =
    1.2
