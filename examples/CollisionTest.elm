module CollisionTest exposing (main)

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
    [ -- corner:
      sphere
    , box
        |> Body.moveBy { x = 0, y = 0, z = 10 }
        |> Body.rotateBy (pi / 3) { x = 1, y = 1, z = 0 }

    -- edge:
    , Body.moveBy { x = 4, y = 0, z = 0 } sphere
    , box
        |> Body.moveBy { x = 4, y = 0, z = 10 }
        |> Body.rotateBy (pi / 3) { x = 1, y = 0, z = 0 }

    -- face:
    , Body.moveBy { x = -4, y = 0, z = 0 } sphere
    , Body.moveBy { x = -4, y = 0, z = 10 } box
    ]


box : Body DemoBody
box =
    Bodies.box "box" { x = 2, y = 2, z = 2 }
        |> Body.setMass 5


sphere : Body DemoBody
sphere =
    Bodies.sphere "sphere" 1.2
        |> Body.setMass 0
