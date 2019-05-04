module Dominos exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Physics.Body as Body exposing (Body)
import Physics.Material as Material exposing (Material)


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.run


initialBodies : List (Body DemoBody)
initialBodies =
    (domino
        |> Body.rotateBy (pi / 8) { x = 0, y = 1, z = 0 }
        |> Body.rotateBy (pi / 4) { x = 0, y = 0, z = 1 }
        |> Body.moveBy { x = -5.5, y = -5.5, z = 0 }
    )
        :: List.map
            (\i ->
                domino
                    |> Body.rotateBy (pi / 4) { x = 0, y = 0, z = 1 }
                    |> Body.moveBy { x = toFloat (5 - i), y = toFloat (5 - i), z = 0 }
            )
            (List.range 0 10)


{-| A domino piece
-}
domino : Body DemoBody
domino =
    Bodies.box "domino" { x = 0.1, y = 1, z = 2 }
        |> Body.setMass 0.01
        |> Body.setMaterial slippy


slippy : Material
slippy =
    Material.custom
        { bounciness = 0
        , friction = 0.01
        }
