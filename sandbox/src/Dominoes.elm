module Dominoes exposing (main)

{-| This demo is used to show custom materials.
Without the slippy material, dominoes would not slide along each other.
Try to make the floor slippy too!
-}

import Angle
import Array exposing (Array)
import Axis3d
import Block3d
import Common.Demo as Demo
import Common.Meshes as Meshes exposing (Attributes)
import Density
import Frame3d
import Length
import Mass
import Physics exposing (Body, BodyCoordinates)
import Physics.Material as Material
import Plane3d
import Point3d
import WebGL exposing (Mesh)


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


slippy =
    Material.dense { density = Density.kilogramsPerCubicMeter 600, bounciness = 0, friction = 0.001 }


dominoBlock3d : Block3d.Block3d Length.Meters BodyCoordinates
dominoBlock3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 0.1
        , Length.meters 1
        , Length.meters 2
        )


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy (Material.surface { friction = 0.3, bounciness = 0 })
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        dominoBody =
            Physics.block dominoBlock3d slippy
                |> Physics.scaleMassTo (Mass.kilograms 5)

        tiltedDomino =
            dominoBody
                |> Physics.rotateAround Axis3d.y (Angle.radians (pi / 8))
                |> Physics.rotateAround Axis3d.z (Angle.radians (pi / 4))
                |> Physics.moveTo (Point3d.meters -5.5 -5.5 0)

        regularDominos =
            List.indexedMap
                (\idx i ->
                    ( idx + 2
                    , dominoBody
                        |> Physics.rotateAround Axis3d.z (Angle.radians (pi / 4))
                        |> Physics.moveTo (Point3d.meters (toFloat (5 - i)) (toFloat (5 - i)) 0)
                    )
                )
                (List.range 0 10)
    in
    ( 0, floorBody ) :: ( 1, tiltedDomino ) :: regularDominos


initialMeshes : Array (Mesh Attributes)
initialMeshes =
    let
        floorMesh =
            Meshes.fromTriangles []

        dominoMesh =
            Meshes.fromTriangles (Meshes.block dominoBlock3d)
    in
    Array.fromList (floorMesh :: List.repeat 12 dominoMesh)
