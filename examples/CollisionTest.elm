module CollisionTest exposing (main)

import Common.Bodies as Bodies exposing (DemoBody(..))
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.run


initialBodies : List ( DemoBody, Physics.Body )
initialBodies =
    [ -- corner:
      Bodies.getBody DemoSphere (Physics.setMass 0)
    , Bodies.getBody DemoBox
        (Physics.offsetBy (vec3 0 0 10)
            >> Physics.rotateBy (vec3 1 1 0) (pi / 3)
        )

    -- edge:
    , Bodies.getBody DemoSphere
        (Physics.setMass 0
            >> Physics.offsetBy (vec3 4 0 0)
        )
    , Bodies.getBody DemoBox
        (Physics.offsetBy (vec3 4 0 10)
            >> Physics.rotateBy (vec3 1 0 0) (pi / 3)
        )

    -- face:
    , Bodies.getBody DemoSphere
        (Physics.setMass 0
            >> Physics.offsetBy (vec3 -4 0 0)
        )
    , Bodies.getBody DemoBox
        (Physics.offsetBy (vec3 -4 0 10))
    ]
