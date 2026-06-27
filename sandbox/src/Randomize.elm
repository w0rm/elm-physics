module Randomize exposing (main)

{-| This demo drops random bodies.
It also shows how to make a compound body out of multiple shapes.
-}

import Angle
import Array exposing (Array)
import Axis3d
import Block3d
import Common.Demo as Demo
import Common.Meshes as Meshes
import Cone3d
import Cylinder3d
import Direction3d
import Frame3d
import Length
import Mass
import Physics exposing (Body, BodyCoordinates)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d
import Random
import Sphere3d
import Vector3d


type alias State =
    { meshes : Array Meshes.Meshes
    , nextId : Int
    }


type Msg
    = Drop
    | AddRandom ( Body, Meshes.Meshes )


initialState : State
initialState =
    let
        ( _, meshes, nextId ) =
            initialBodiesAndMeshes
    in
    { meshes = meshes, nextId = nextId }


main : Program () (Demo.Model Int State) (Demo.Msg Msg)
main =
    let
        ( bodies, _, _ ) =
            initialBodiesAndMeshes

        base =
            Demo.defaults
                { initialBodies = bodies
                , lookupMesh = \state id -> Array.get id state.meshes
                , camera =
                    { from = { x = 0, y = 30, z = 20 }
                    , to = { x = 0, y = 0, z = 0 }
                    }
                , initialState = initialState
                }
    in
    Demo.program
        { base
            | update = update
            , buttons = \_ -> [ Demo.button Drop "Drop a random body" ]
        }


update : Msg -> State -> List ( Int, Body ) -> ( State, List ( Int, Body ), Cmd Msg )
update msg state bodies =
    case msg of
        Drop ->
            ( state, bodies, Random.generate AddRandom randomBody )

        AddRandom ( body, mesh ) ->
            ( { state
                | meshes = Array.push mesh state.meshes
                , nextId = state.nextId + 1
              }
            , ( state.nextId, body ) :: bodies
            , Cmd.none
            )


boxBlock3d : Block3d.Block3d Length.Meters BodyCoordinates
boxBlock3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 2
        , Length.meters 2
        , Length.meters 2
        )


sphere3d : Sphere3d.Sphere3d Length.Meters BodyCoordinates
sphere3d =
    Sphere3d.atOrigin (Length.meters 1.2)


cylinder3d : Cylinder3d.Cylinder3d Length.Meters BodyCoordinates
cylinder3d =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.x
        { radius = Length.meters 0.5, length = Length.meters 2 }


cone3d : Cone3d.Cone3d Length.Meters BodyCoordinates
cone3d =
    Cone3d.startingAt Point3d.origin
        Direction3d.x
        { radius = Length.meters 0.5, length = Length.meters 2 }


capsule3d : Cylinder3d.Cylinder3d Length.Meters BodyCoordinates
capsule3d =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = Length.meters 0.6, length = Length.meters 2 }


compoundBlocks : List (Block3d.Block3d Length.Meters BodyCoordinates)
compoundBlocks =
    List.map
        (\center ->
            Block3d.centeredOn
                (Frame3d.atPoint center)
                ( Length.meters 1
                , Length.meters 1
                , Length.meters 1
                )
        )
        [ Point3d.meters -0.5 0 -0.5
        , Point3d.meters -0.5 0 0.5
        , Point3d.meters 0.5 0 0.5
        ]


makeBox : Body
makeBox =
    Physics.block boxBlock3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


makeSphere : Body
makeSphere =
    Physics.sphere sphere3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


makeCylinder : Body
makeCylinder =
    Physics.cylinder cylinder3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


makeCone : Body
makeCone =
    Physics.cone cone3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


makeCapsule : Body
makeCapsule =
    Physics.capsule capsule3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


makeCompound : Body
makeCompound =
    Physics.dynamic
        (List.map (\b -> ( Shape.block b, Material.wood )) compoundBlocks)
        |> Physics.scaleMassTo (Mass.kilograms 5)


initialBodiesAndMeshes : ( List ( Int, Body ), Array Meshes.Meshes, Int )
initialBodiesAndMeshes =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        boxBody =
            makeBox
                |> Physics.rotateAround Axis3d.y (Angle.radians (-pi / 5))
                |> Physics.moveTo (Point3d.meters 0 0 2)

        sphereBody =
            makeSphere
                |> Physics.moveTo (Point3d.meters 0.5 0 8)

        cylinderBody =
            makeCylinder
                |> Physics.rotateAround
                    (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 }))
                    (Angle.radians (pi / 2))
                |> Physics.moveTo (Point3d.meters 0.5 0 11)

        coneBody =
            makeCone
                |> Physics.rotateAround
                    (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 }))
                    (Angle.radians (pi / 2))
                |> Physics.moveTo (Point3d.meters 0.5 0 11)

        compoundBody =
            makeCompound
                |> Physics.rotateAround
                    (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 }))
                    (Angle.radians (pi / 5))
                |> Physics.moveTo (Point3d.meters -1.2 0 5)

        capsuleBody =
            makeCapsule
                |> Physics.rotateAround Axis3d.x (Angle.radians (pi / 4))
                |> Physics.moveTo (Point3d.meters 1.5 0 14)

        bodies =
            [ ( 0, floorBody )
            , ( 1, boxBody )
            , ( 2, sphereBody )
            , ( 3, cylinderBody )
            , ( 4, compoundBody )
            , ( 5, capsuleBody )
            , ( 6, coneBody )
            ]

        meshes =
            Array.fromList
                [ Meshes.fromTriangles []
                , Meshes.fromTriangles (Meshes.block boxBlock3d)
                , Meshes.fromTriangles (Meshes.sphere 2 sphere3d)
                , Meshes.fromTriangles (Meshes.cylinder 12 cylinder3d)
                , Meshes.fromTriangleGroups (List.map Meshes.block compoundBlocks)
                , Meshes.fromTriangles (Meshes.capsule 12 capsule3d)
                , Meshes.fromTriangles (Meshes.cone 12 cone3d)
                ]
    in
    ( bodies, meshes, 7 )


randomBody : Random.Generator ( Body, Meshes.Meshes )
randomBody =
    Random.map5
        (\angle x y z bodyKind ->
            let
                ( body, mesh ) =
                    case bodyKind of
                        0 ->
                            ( makeBox, Meshes.fromTriangles (Meshes.block boxBlock3d) )

                        1 ->
                            ( makeSphere, Meshes.fromTriangles (Meshes.sphere 2 sphere3d) )

                        2 ->
                            ( makeCylinder, Meshes.fromTriangles (Meshes.cylinder 12 cylinder3d) )

                        3 ->
                            ( makeCompound, Meshes.fromTriangleGroups (List.map Meshes.block compoundBlocks) )

                        6 ->
                            ( makeCone, Meshes.fromTriangles (Meshes.cone 12 cone3d) )

                        _ ->
                            ( makeCapsule, Meshes.fromTriangles (Meshes.capsule 12 capsule3d) )
            in
            ( body
                |> Physics.rotateAround
                    (Axis3d.through Point3d.origin (Maybe.withDefault Direction3d.x (Vector3d.direction (Vector3d.from Point3d.origin (Point3d.meters x y z)))))
                    (Angle.radians angle)
                |> Physics.moveTo (Point3d.meters 0 0 10)
            , mesh
            )
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.int 0 6)
