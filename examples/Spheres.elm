module Spheres exposing (main)

import Common.Demo as Demo exposing (Demo, DemoProgram)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Random
import Common.Meshes as Meshes exposing (Attributes)
import WebGL exposing (Mesh)


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies initialBoxes
        |> Demo.dropOnClick randomShape
        |> Demo.run


{-| A constant cube-shaped body with unit sides and mass of 5
-}
box : Physics.Body
box =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))
        |> Tuple.first


{-| A constant sphere
-}
sphere : Physics.Body
sphere =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.sphere 1.2)
        |> Tuple.first


{-| Boxes in the initial scene
-}
initialBoxes : List ( Mesh Attributes, Physics.Body )
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
        |> List.map ((,) cubeMesh)


cubeMesh : Mesh Attributes
cubeMesh =
    Meshes.makeBox (vec3 1 1 1)


sphereMesh : Mesh Attributes
sphereMesh =
    Meshes.makeSphere 3 1.2


{-| A shape raised above the plane, shifted or rotated to a random 3d angle
-}
randomShape : Random.Generator ( Mesh Attributes, Physics.Body )
randomShape =
    Random.map5
        (\angle x y z isSphere ->
            case isSphere of
                True ->
                    sphere
                        |> Physics.offsetBy (vec3 0 0 10)
                        |> Physics.offsetBy (vec3 x y z)
                        |> (,) sphereMesh

                False ->
                    box
                        |> Physics.offsetBy (vec3 0 0 10)
                        |> Physics.rotateBy (vec3 x y z) angle
                        |> (,) cubeMesh
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.bool)
