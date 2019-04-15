module Dominos exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Physics.Body as Body exposing (Body)


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.run


initialBodies : List (Body DemoBody)
initialBodies =
    List.range 0 10
        |> List.map
            (\i ->
                Bodies.domino
                    |> Body.rotateBy (pi / 4) { x = 0, y = 0, z = 1 }
                    |> Body.moveBy { x = toFloat (5 - i), y = toFloat (5 - i), z = 0 }
            )
        |> (::)
            (Bodies.domino
                |> Body.rotateBy (pi / 8) { x = 0, y = 1, z = 0 }
                |> Body.rotateBy (pi / 4) { x = 0, y = 0, z = 1 }
                |> Body.moveBy { x = -5.5, y = -5.5, z = 0 }
            )
