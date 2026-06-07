module Dominoes exposing (main)

{-| A chain of plastic dominoes at real playing-domino dimensions
(9 × 25 × 50 mm) sitting on a wooden table, arranged diagonally.
Material combination gives μ ≈ 0.37 on the floor and ≈ 0.35 between
dominoes, derived from the built-in plastic and wood materials.
-}

import Angle
import Array exposing (Array)
import Axis3d
import Block3d
import Common.Demo as Demo
import Common.Meshes as Meshes
import Duration
import Frame3d
import Length
import Physics exposing (Body, BodyCoordinates)
import Physics.Material as Material
import Plane3d
import Point3d


main : Program () (Demo.Model Int ()) (Demo.Msg msg)
main =
    let
        defaults =
            Demo.defaults
                { initialBodies = initialBodies
                , lookupMesh = \_ id -> Array.get id initialMeshes
                , camera =
                    { from = { x = 0.05, y = 0.8, z = -0.45 }
                    , to = { x = 0, y = 0, z = -0.95 }
                    }
                , initialState = ()
                }
    in
    Demo.program
        { defaults
            | timestep =
                -- mm-scale bodies need a shorter step so per-step motion at
                -- peak collision velocity stays small relative to the
                -- 9 mm domino thickness
                { duration = Duration.seconds (1 / 120)
                , maxSteps = 2
                }
            , solverIterations = 20

            -- mm-scale dominoes: shrink the contact dots to match (the
            -- default 0.07 m is wider than a whole domino)
            , contactRadius = 0.002
        }


{-| Real playing-domino dimensions: 9 mm thick × 25 mm wide × 50 mm long.
-}
dominoBlock3d : Block3d.Block3d Length.Meters BodyCoordinates
dominoBlock3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.millimeters 9
        , Length.millimeters 25
        , Length.millimeters 50
        )


{-| Centre-to-centre spacing along the chain. Real domino chains use about
30 mm between pieces — tight enough for one to reach the next when it
topples, loose enough not to start in contact.
-}
spacing : Float
spacing =
    0.03


{-| Z-coordinate of each domino's centre so the bottom face sits on the
floor at `Demo.floorZ.z` (currently -1). Half-height is 25 mm.
-}
restingZ : Float
restingZ =
    Demo.floorZ.z + 0.025


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        dominoBody =
            Physics.block dominoBlock3d Material.plastic

        tiltedDomino =
            dominoBody
                |> Physics.rotateAround Axis3d.y (Angle.radians (pi / 8))
                |> Physics.rotateAround Axis3d.z (Angle.radians (pi / 4))
                |> Physics.moveTo (Point3d.meters (-6 * spacing) (-6 * spacing) restingZ)

        regularDominos =
            List.indexedMap
                (\idx i ->
                    ( idx + 2
                    , dominoBody
                        |> Physics.rotateAround Axis3d.z (Angle.radians (pi / 4))
                        |> Physics.moveTo
                            (Point3d.meters
                                (toFloat (5 - i) * spacing)
                                (toFloat (5 - i) * spacing)
                                restingZ
                            )
                    )
                )
                (List.range 0 10)
    in
    ( 0, floorBody ) :: ( 1, tiltedDomino ) :: regularDominos


initialMeshes : Array Meshes.Meshes
initialMeshes =
    let
        floorMesh =
            Meshes.fromTriangles []

        dominoMesh =
            Meshes.fromTriangles (Meshes.block dominoBlock3d)
    in
    Array.fromList (floorMesh :: List.repeat 12 dominoMesh)
