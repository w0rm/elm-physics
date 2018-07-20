module Boxes exposing (main)

import Common.Demo as Demo exposing (Demo, DemoProgram)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBoxes
        |> Demo.dropOnClick randomlyRotatedBox
        |> Demo.run


{-| A constant cube-shaped body with unit sides and mass of 5
-}
box : Physics.Body
box =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))


{-| Boxes in the initial scene
-}
initialBoxes : List Physics.Body
initialBoxes =
    [ box
        |> Physics.offsetBy (vec3 0 0 2)
        |> Physics.rotateBy Vec3.j (-pi / 5)
    , box
        |> Physics.offsetBy (vec3 -1.2 0 9)
        |> Physics.rotateBy Vec3.j (-pi / 4)
    , box
        |> Physics.offsetBy (vec3 1.3 0 6)
        |> Physics.rotateBy Vec3.j (pi / 5)
    ]


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator Physics.Body
randomlyRotatedBox =
    Random.map4
        (\angle x y z ->
            box
                |> Physics.offsetBy (vec3 0 0 10)
                |> Physics.rotateBy (vec3 x y z) angle
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
