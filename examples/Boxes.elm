module Boxes exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Physics.Body as Body exposing (Body)
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies (range3 boxesPerDimension boxSize)
        |> Demo.dropOnClick randomlyRotatedBox
        |> Demo.run


boxesPerDimension : Int
boxesPerDimension =
    4


boxSize : Float
boxSize =
    1


range3 : Int -> Float -> List (Body DemoBody)
range3 number distance =
    List.foldl
        (\x acc1 ->
            List.foldl
                (\y acc2 ->
                    List.foldl
                        (\z acc3 ->
                            addBoxAt
                                ((toFloat x - toFloat (number - 1) / 2) * distance)
                                ((toFloat y - toFloat (number - 1) / 2) * distance)
                                ((toFloat z - toFloat (number - 1) / 2) * distance)
                                acc3
                        )
                        acc2
                        (List.range 0 (number - 1))
                )
                acc1
                (List.range 0 (number - 1))
        )
        []
        (List.range 0 (number - 1))


addBoxAt : Float -> Float -> Float -> List (Body DemoBody) -> List (Body DemoBody)
addBoxAt x y z =
    (::)
        (Body.moveBy
            { x = x
            , y = y
            , z = z + toFloat boxesPerDimension * 1.5 * boxSize -- raise above the ground
            }
            box
        )


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator (Body DemoBody)
randomlyRotatedBox =
    Random.map4
        (\angle x y z ->
            box
                |> Body.moveBy { x = 0, y = 0, z = 10 }
                |> Body.rotateBy angle { x = x, y = y, z = z }
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


box : Body DemoBody
box =
    Bodies.box "box" { x = boxSize, y = boxSize, z = boxSize }
        |> Body.setMass 1
