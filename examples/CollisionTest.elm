module CollisionTest exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Body as Body exposing (Body)


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.run


initialBodies : List (Body DemoBody)
initialBodies =
    [ -- corner:
      Bodies.sphere
        |> Body.setMass 0
    , Bodies.box
        |> Body.moveBy { x = 0, y = 0, z = 10 }
        |> Body.rotateBy (pi / 3) { x = 1, y = 1, z = 0 }

    -- edge:
    , Bodies.sphere
        |> Body.setMass 0
        |> Body.moveBy { x = 4, y = 0, z = 0 }
    , Bodies.box
        |> Body.moveBy { x = 4, y = 0, z = 10 }
        |> Body.rotateBy (pi / 3) { x = 1, y = 0, z = 0 }

    -- face:
    , Bodies.sphere
        |> Body.setMass 0
        |> Body.moveBy { x = -4, y = 0, z = 0 }
    , Bodies.box
        |> Body.moveBy { x = -4, y = 0, z = 10 }
    ]
