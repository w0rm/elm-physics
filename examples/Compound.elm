module Compound exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Common.Meshes as Meshes
import Physics.Body as Body exposing (Body)
import Physics.Shape as Shape exposing (Shape)
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.dropOnClick randomlyRotatedCompoundBody
        |> Demo.run


{-| Bodies in the initial scene
-}
initialBodies : List (Body DemoBody)
initialBodies =
    [ compound
        |> Body.moveBy { x = 0, y = 0, z = 2 }
        |> Body.rotateBy (-pi / 5) { x = 0, y = 1, z = 0 }
    , compound
        |> Body.moveBy { x = -1.2, y = 0, z = 9 }
        |> Body.rotateBy (-pi / 4) { x = 1, y = 0, z = 0 }
    , compound
        |> Body.moveBy { x = 1.3, y = 0, z = 6 }
        |> Body.rotateBy (pi / 5) { x = 0, y = 1, z = 0 }
    ]


{-| A compound body raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedCompoundBody : Random.Generator (Body DemoBody)
randomlyRotatedCompoundBody =
    Random.map4
        (\angle x y z ->
            compound
                |> Body.moveBy { x = 0, y = 0, z = 15 }
                |> Body.rotateBy angle { x = x, y = y, z = z }
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


{-| A compound body made of three boxes
-}
compound : Body DemoBody
compound =
    let
        boxDimensions =
            { x = 2, y = 2, z = 2 }

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
        |> Bodies.fromTriangles "compound"
        |> Body.compound
            [ Shape.moveBy { x = -1, y = 0, z = -1 } boxShape
            , Shape.moveBy { x = -1, y = 0, z = 1 } boxShape
            , Shape.moveBy { x = 1, y = 0, z = 1 } boxShape
            ]
        |> Body.setMass 5
