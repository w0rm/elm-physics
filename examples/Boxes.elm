module Boxes exposing (main)

import Common.Bodies as Bodies exposing (DemoBody)
import Common.Demo as Demo exposing (Demo, DemoProgram)
import Physics.Body as Body exposing (Body)
import Random


main : DemoProgram
main =
    Demo.demo
        |> Demo.addBodies (range3 addBoxAt 4 2 [])
        |> Demo.dropOnClick randomlyRotatedBox
        |> Demo.run


addBoxAt : Float -> Float -> Float -> List (Body DemoBody) -> List (Body DemoBody)
addBoxAt x y z =
    (::) (Body.moveBy { x = x, y = y + 3, z = z + 5 } Bodies.box)


range3 : (Float -> Float -> Float -> a -> a) -> Int -> Float -> a -> a
range3 fn size distance init =
    List.foldl
        (\x acc1 ->
            List.foldl
                (\y acc2 ->
                    List.foldl
                        (\z acc3 ->
                            fn
                                ((toFloat x - toFloat (size - 1) / 2) * distance)
                                ((toFloat y - toFloat (size - 1) / 2) * distance)
                                (toFloat z * distance)
                                acc3
                        )
                        acc2
                        (List.range 0 (size - 1))
                )
                acc1
                (List.range 0 (size - 1))
        )
        init
        (List.range 0 (size - 1))


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator (Body DemoBody)
randomlyRotatedBox =
    Random.map4
        (\angle x y z ->
            Bodies.box
                |> Body.moveBy { x = 0, y = 0, z = 10 }
                |> Body.rotateBy angle { x = x, y = y, z = z }
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
