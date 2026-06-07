module CapsulesDrop exposing (main)

{-| Capsule-vs-Convex collision tester. The scene is laid out as three
rows of independent (box, capsule) pairs:

  - Front row (y = -7) exercises the cap-on-feature branches
    (capsule axis nearly aligned with the separating axis):
    A face, B edge, C vertex, D 30° tilted, E 60° tilted,
    F diagonal cap on vertex (tilted in two axes).

  - Middle row (y = 0) is two STATIC "diamond" blocks rotated so a
    vertex points up: L vertical capsule (cap on vertex) and
    M horizontal capsule (cylinder body on vertex).

  - Back row (y = +7) exercises the cylinder-body branches
    (capsule axis perpendicular to the separating axis):
    G face support, H parallel-edge fallback, I perpendicular over
    edge, J cylinder over a vertex, K face support on a rotated box.

Each capsule is dropped from z = 6 onto its own box so the scenarios
don't interact. The Restart button resets the scene.

-}

import Angle
import Array exposing (Array)
import Axis3d
import Block3d
import Common.Demo as Demo
import Common.Meshes as Meshes
import Cylinder3d
import Direction3d
import Frame3d
import Length
import Mass
import Physics exposing (Body)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d


main : Program () (Demo.Model Int ()) (Demo.Msg msg)
main =
    Demo.program
        (Demo.defaults
            { initialBodies = initialBodies
            , lookupMesh = \_ id -> Array.get id initialMeshes
            , camera =
                { from = { x = 0, y = 28, z = 22 }
                , to = { x = 0, y = 0, z = 1 }
                }
            , initialState = ()
            }
        )


boxBlock3d : Block3d.Block3d Length.Meters Physics.BodyCoordinates
boxBlock3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 2, Length.meters 2, Length.meters 2 )


capsule3d : Cylinder3d.Cylinder3d Length.Meters Physics.BodyCoordinates
capsule3d =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = Length.meters 0.5, length = Length.meters 2.5 }


boxBody : Body
boxBody =
    Physics.block boxBlock3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


staticBoxBody : Body
staticBoxBody =
    Physics.static [ ( Shape.block boxBlock3d, Material.wood ) ]


capsuleBody : Body
capsuleBody =
    Physics.capsule capsule3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


type alias Scenario =
    { label : String
    , box : Body
    , capsule : Body
    }


scenarios : List Scenario
scenarios =
    let
        boxAt x y =
            boxBody |> Physics.moveTo (Point3d.meters x y 0)

        boxAtRotated x y angleDeg =
            boxBody
                |> Physics.rotateAround Axis3d.z (Angle.degrees angleDeg)
                |> Physics.moveTo (Point3d.meters x y 0)

        staticBoxVertexUp x y =
            staticBoxBody
                |> Physics.rotateAround Axis3d.x (Angle.degrees 45)
                |> Physics.rotateAround Axis3d.y (Angle.degrees -35.264)
                |> Physics.moveTo (Point3d.meters x y 1)

        capAt x y =
            capsuleBody |> Physics.moveTo (Point3d.meters x y 6)

        capTiltedX x y angleDeg =
            capsuleBody
                |> Physics.rotateAround Axis3d.x (Angle.degrees angleDeg)
                |> Physics.moveTo (Point3d.meters x y 6)

        capAxisX x y =
            capsuleBody
                |> Physics.rotateAround Axis3d.y (Angle.degrees 90)
                |> Physics.moveTo (Point3d.meters x y 6)
    in
    [ { label = "A vertical cap → face center"
      , box = boxAt -10 -7
      , capsule = capAt -10 -7
      }
    , { label = "B vertical cap → +x top edge (cap straddles edge)"
      , box = boxAt -6 -7
      , capsule = capAt (-6 + 1) -7
      }
    , { label = "C vertical cap → +x +y top corner (cap on vertex)"
      , box = boxAt -2 -7
      , capsule = capAt (-2 + 1) (-7 + 1)
      }
    , { label = "D 30° tilted cap → face"
      , box = boxAt 2 -7
      , capsule = capTiltedX 2 -7 30
      }
    , { label = "E 60° tilted cap → face (steep tilt, cap heavily off-axis)"
      , box = boxAt 6 -7
      , capsule = capTiltedX 6 -7 60
      }
    , { label = "F diagonal cap (X 35°, Y 25°) → +x +y top corner"
      , box = boxAt 10 -7
      , capsule =
            capsuleBody
                |> Physics.rotateAround Axis3d.x (Angle.degrees 35)
                |> Physics.rotateAround Axis3d.y (Angle.degrees 25)
                |> Physics.moveTo (Point3d.meters (10 + 1) (-7 + 1) 6)
      }
    , { label = "G horizontal capsule → face center (Face Support, 2 contacts)"
      , box = boxAt -8 7
      , capsule = capTiltedX -8 7 90
      }
    , { label = "H axis ∥ +y top edge, just outside the face (parallel-edge fallback)"
      , box = boxAt -4 7
      , capsule = capAxisX -4 (7 + 1.3)
      }
    , { label = "I axis ⊥ +x top edge, half the cylinder overhanging"
      , box = boxAt 0 7
      , capsule = capTiltedX 0.5 7 90
      }
    , { label = "J horizontal capsule centered over +x +y top corner"
      , box = boxAt 4 7
      , capsule = capTiltedX 5 8 90
      }
    , { label = "K horizontal capsule on box rotated 45° around z (rotated-face support)"
      , box = boxAtRotated 8 7 45
      , capsule = capTiltedX 8 7 90
      }
    , { label = "L vertical capsule (cap) → upward vertex of static diamond"
      , box = staticBoxVertexUp -3 0
      , capsule = capAt -3 0
      }
    , { label = "M horizontal capsule (cylinder) → upward vertex of static diamond"
      , box = staticBoxVertexUp 3 0
      , capsule = capTiltedX 3 0 90
      }
    ]


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        n =
            List.length scenarios
    in
    ( 0, floorBody )
        :: List.indexedMap (\i s -> ( i + 1, s.box )) scenarios
        ++ List.indexedMap (\i s -> ( i + 1 + n, s.capsule )) scenarios


initialMeshes : Array Meshes.Meshes
initialMeshes =
    let
        floorMesh =
            Meshes.fromTriangles []

        boxMesh =
            Meshes.fromTriangles (Meshes.block boxBlock3d)

        capsuleMesh =
            Meshes.fromTriangles (Meshes.capsule 12 capsule3d)

        n =
            List.length scenarios
    in
    Array.fromList
        (floorMesh
            :: List.repeat n boxMesh
            ++ List.repeat n capsuleMesh
        )
