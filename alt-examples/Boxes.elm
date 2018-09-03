module Boxes exposing (main)

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltPhysics
import Common.Bodies as Bodies exposing (DemoBody(..))
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBoxes
        |> Demo.dropOnClick randomlyRotatedBox
        |> Demo.run


{-| Boxes in the initial scene
-}
initialBoxes : List ( DemoBody, AltPhysics.Body )
initialBoxes =
    [ Bodies.getBody DemoBox
        (AltPhysics.offsetBy (vec3 0 0 2)
            >> AltPhysics.rotateBy Vec3.j (-pi / 5)
        )
    , Bodies.getBody DemoBox
        (AltPhysics.offsetBy (vec3 -1.2 0 9)
            >> AltPhysics.rotateBy Vec3.j (-pi / 4)
        )
    , Bodies.getBody DemoBox
        (AltPhysics.offsetBy (vec3 1.3 0 6)
            >> AltPhysics.rotateBy Vec3.j (pi / 5)
        )
    ]


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator ( DemoBody, AltPhysics.Body )
randomlyRotatedBox =
    Random.map4
        (\angle x y z ->
            Bodies.getBody DemoBox
                (AltPhysics.offsetBy (vec3 0 0 10)
                    >> AltPhysics.rotateBy (vec3 x y z) angle
                )
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
