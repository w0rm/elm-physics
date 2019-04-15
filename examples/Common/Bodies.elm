module Common.Bodies exposing
    ( DemoBody
    , box
    , compound
    , domino
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
    }


fromTriangles : List ( Attributes, Attributes, Attributes ) -> DemoBody
fromTriangles triangles =
    { mesh = Meshes.toMesh triangles
    , wireframe = Meshes.toWireframe triangles
    }


{-| A plane-shaped body
-}
plane : Body DemoBody
plane =
    []
        |> fromTriangles
        |> Body.plane


{-| A domino piece
-}
domino : Body DemoBody
domino =
    Meshes.box dominoDimensions
        |> fromTriangles
        |> Body.box dominoDimensions
        |> Body.setMass 0.01
        |> Body.setMaterial slippy


slippy : Material
slippy =
    Material.custom
        { bounciness = 0
        , friction = 0.01
        }


dominoDimensions : { x : Float, y : Float, z : Float }
dominoDimensions =
    { x = 0.1, y = 1, z = 2 }


{-| A cube with sides of 2 and mass of 5
-}
box : Body DemoBody
box =
    Meshes.box boxDimensions
        |> fromTriangles
        |> Body.box boxDimensions
        |> Body.setMass 5


boxDimensions : { x : Float, y : Float, z : Float }
boxDimensions =
    { x = 2, y = 2, z = 2 }


{-| A sphere with radius of 1.2 and mass of 5
-}
sphere : Body DemoBody
sphere =
    Meshes.sphere 2 sphereRadius
        |> fromTriangles
        |> Body.sphere sphereRadius
        |> Body.setMass 5


sphereRadius : Float
sphereRadius =
    1.2


{-| A compound body made of three boxes
-}
compound : Body DemoBody
compound =
    let
        boxTriangles =
            Meshes.box boxDimensions

        boxShape =
            Shape.box boxDimensions
    in
    [ Meshes.moveBy { x = -1, y = 0, z = -1 } boxTriangles
    , Meshes.moveBy { x = -1, y = 0, z = 1 } boxTriangles
    , Meshes.moveBy { x = 1, y = 0, z = 1 } boxTriangles
    ]
        |> List.concat
        |> fromTriangles
        |> Body.compound
            [ Shape.moveBy { x = -1, y = 0, z = -1 } boxShape
            , Shape.moveBy { x = -1, y = 0, z = 1 } boxShape
            , Shape.moveBy { x = 1, y = 0, z = 1 } boxShape
            ]
        |> Body.setMass 5
