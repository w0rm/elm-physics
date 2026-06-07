module Boxes exposing (main)

{-| This demo is used to test performance. It drops 5×5×5 boxes.
Try changing `boxesPerDimension` to drop even more!
-}

import Array exposing (Array)
import Block3d
import Common.Demo as Demo
import Common.Meshes as Meshes
import Frame3d
import Length
import Mass
import Physics exposing (Body)
import Physics.Material as Material
import Plane3d
import Point3d


boxesPerDimension : number
boxesPerDimension =
    5


main : Program () (Demo.Model Int ()) (Demo.Msg msg)
main =
    Demo.program
        (Demo.defaults
            { initialBodies = initialBodies
            , lookupMesh = \_ id -> Array.get id initialMeshes
            , camera =
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
                }
            , initialState = ()
            }
        )


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        dimensions =
            List.map toFloat (List.range 0 (boxesPerDimension - 1))

        distance =
            1

        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1, Length.meters 1, Length.meters 1 )

        boxBody =
            Physics.block block3d Material.wood
                |> Physics.scaleMassTo (Mass.kilograms 5)

        boxes =
            List.indexedMap
                (\idx ( x, y, z ) ->
                    ( idx + 1
                    , boxBody
                        |> Physics.moveTo
                            (Point3d.meters
                                ((x - (boxesPerDimension - 1) / 2) * distance)
                                ((y - (boxesPerDimension - 1) / 2) * distance)
                                ((z + (2 * boxesPerDimension + 1) / 2) * distance)
                            )
                    )
                )
                (List.concatMap
                    (\x ->
                        List.concatMap
                            (\y -> List.map (\z -> ( x, y, z )) dimensions)
                            dimensions
                    )
                    dimensions
                )
    in
    ( 0, floorBody ) :: boxes


initialMeshes : Array Meshes.Meshes
initialMeshes =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1, Length.meters 1, Length.meters 1 )

        floorMesh =
            Meshes.fromTriangles []

        boxMesh =
            Meshes.fromTriangles (Meshes.block block3d)

        boxCount =
            boxesPerDimension ^ 3
    in
    Array.fromList (floorMesh :: List.repeat boxCount boxMesh)
