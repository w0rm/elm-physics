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
        (Physics.offsetBy { x = 0, y = 0, z = 10 }
            >> Physics.rotateBy { x = 1, y = 1, z = 0 } (pi / 3)
        )

    -- edge:
    , Bodies.getBody DemoSphere
        (Physics.setMass 0
            >> Physics.offsetBy { x = 4, y = 0, z = 0 }
        )
    , Bodies.getBody DemoBox
        (Physics.offsetBy { x = 4, y = 0, z = 10 }
            >> Physics.rotateBy { x = 1, y = 0, z = 0 } (pi / 3)
        )

    -- face:
    , Bodies.getBody DemoSphere
        (Physics.setMass 0
            >> Physics.offsetBy { x = -4, y = 0, z = 0 }
        )
    , Bodies.getBody DemoBox
        (Physics.offsetBy { x = -4, y = 0, z = 10 })
    ]
