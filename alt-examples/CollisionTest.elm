module CollisionTest exposing (main)

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltPhysics
import Common.Bodies as Bodies exposing (DemoBody(..))
import Common.Demo as Demo exposing (Demo, DemoProgram)


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBodies
        |> Demo.run


initialBodies : List ( DemoBody, AltPhysics.Body )
initialBodies =
    [ -- corner:
      Bodies.getBody DemoSphere (AltPhysics.setMass 0)
    , Bodies.getBody DemoBox
        (AltPhysics.offsetBy (vec3 0 0 10)
            >> AltPhysics.rotateBy (vec3 1 1 0) (pi / 3)
        )

    -- edge:
    , Bodies.getBody DemoSphere
        (AltPhysics.setMass 0
            >> AltPhysics.offsetBy (vec3 4 0 0)
        )
    , Bodies.getBody DemoBox
        (AltPhysics.offsetBy (vec3 4 0 10)
            >> AltPhysics.rotateBy (vec3 1 0 0) (pi / 3)
        )

    -- face:
    , Bodies.getBody DemoSphere
        (AltPhysics.setMass 0
            >> AltPhysics.offsetBy (vec3 -4 0 0)
        )
    , Bodies.getBody DemoBox
        (AltPhysics.offsetBy (vec3 -4 0 10))
    ]
