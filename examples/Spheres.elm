module Spheres exposing (main)

import Common.Bodies as Bodies exposing (DemoBody(..))
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBoxes
        |> Demo.dropOnClick randomShape
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


{-| A shape raised above the plane, shifted or rotated to a random 3d angle
-}
randomShape : Random.Generator ( DemoBody, Physics.Body )
randomShape =
    Random.map5
        (\angle x y z isSphere ->
            case isSphere of
                0 ->
                    Bodies.getBody DemoBox
                        (Physics.offsetBy { x = 0, y = 0, z = 10 }
                            >> Physics.rotateBy { x = x, y = y, z = z } angle
                        )

                _ ->
                    Bodies.getBody DemoSphere
                        (Physics.offsetBy { x = 0, y = 0, z = 10 }
                            >> Physics.offsetBy { x = x, y = y, z = z }
                        )
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.int 0 1)
