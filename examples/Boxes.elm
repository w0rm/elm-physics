module Boxes exposing (main)

import Common.Bodies as Bodies exposing (DemoBody(..))
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Physics
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBoxes
        |> Demo.dropOnClick randomlyRotatedBox
        |> Demo.run


{-| Boxes in the initial scene
-}
initialBoxes : List ( DemoBody, Physics.Body )
initialBoxes =
    [ Bodies.getBody DemoBox
        (Physics.offsetBy { x = 0, y = 0, z = 2 }
            >> Physics.rotateBy { x = 0, y = 1, z = 0 } (-pi / 5)
        )
    , Bodies.getBody DemoBox
        (Physics.offsetBy { x = -1.2, y = 0, z = 9 }
            >> Physics.rotateBy { x = 0, y = 1, z = 0 } (-pi / 4)
        )
    , Bodies.getBody DemoBox
        (Physics.offsetBy { x = 1.3, y = 0, z = 6 }
            >> Physics.rotateBy { x = 0, y = 1, z = 0 } (pi / 5)
        )
    ]


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator ( DemoBody, Physics.Body )
randomlyRotatedBox =
    Random.map4
        (\angle x y z ->
            Bodies.getBody DemoBox
                (Physics.offsetBy { x = 0, y = 0, z = 10 }
                    >> Physics.rotateBy { x = x, y = y, z = z } angle
                )
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
