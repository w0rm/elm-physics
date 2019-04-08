module Spheres exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Physics.Body as Body exposing (Body)
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.dropOnClick randomBody
        |> Demo.run


{-| Bodies in the initial scene
-}
initialBodies : List (Body DemoBody)
initialBodies =
    [ Bodies.box
        |> Body.moveBy { x = 0, y = 0, z = 2 }
        |> Body.rotateBy (-pi / 5) { x = 0, y = 1, z = 0 }
    , Bodies.box
        |> Body.moveBy { x = -1.2, y = 0, z = 9 }
        |> Body.rotateBy (-pi / 4) { x = 0, y = 1, z = 0 }
    , Bodies.box
        |> Body.moveBy { x = 1.3, y = 0, z = 6 }
        |> Body.rotateBy (pi / 5) { x = 0, y = 1, z = 0 }
    ]


{-| A sphere or box raised above the plane, shifted or rotated to a random 3d angle
-}
randomBody : Random.Generator (Body DemoBody)
randomBody =
    Random.map5
        (\angle x y z isSphere ->
            case isSphere of
                0 ->
                    Bodies.box
                        |> Body.moveBy { x = 0, y = 0, z = 10 }
                        |> Body.rotateBy angle { x = x, y = y, z = z }

                _ ->
                    Bodies.sphere
                        |> Body.moveBy { x = x, y = y, z = z + 10 }
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.int 0 1)
