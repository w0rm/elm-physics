module Kinematic exposing (main)

{-| This demo shows a kinematic platform sliding back and forth between
two endpoints. A stack of two boxes sits on top and rides along thanks to
friction with the moving surface.

The platform is kinematic: it has infinite mass like a static body, but
the engine integrates its position from the velocity the user sets each
frame. Here the velocity follows a cosine wave so the platform decelerates
smoothly toward each endpoint and accelerates back — a constant velocity
flipped instantaneously at the endpoints would jolt the boxes.

-}

import Array exposing (Array)
import Block3d exposing (Block3d)
import Common.Demo as Demo
import Common.Meshes as Meshes
import Duration exposing (Duration)
import Frame3d
import Length exposing (Meters)
import Physics exposing (Body, BodyCoordinates)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d
import Vector3d


platformId : Int
platformId =
    1


platformAmplitude : Float
platformAmplitude =
    3


platformOmega : Float
platformOmega =
    0.6


peakSpeed : Float
peakSpeed =
    platformAmplitude * platformOmega


type alias State =
    { phase : Float }


initialState : State
initialState =
    { phase = -pi / 2 }


main : Program () (Demo.Model Int State) (Demo.Msg msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = initialBodies
                , lookupMesh = \_ id -> Array.get id initialMeshes
                , camera =
                    { from = { x = 0, y = 18, z = 10 }
                    , to = { x = 0, y = 0, z = 1 }
                    }
                , initialState = initialState
                }
    in
    Demo.program
        { base | preSimulate = preSimulate }


preSimulate : Duration -> State -> List ( Int, Body ) -> ( State, List ( Int, Body ) )
preSimulate dt state bodies =
    let
        newPhase =
            state.phase + Duration.inSeconds dt * platformOmega

        platformVx =
            peakSpeed * cos newPhase

        newBodies =
            List.map
                (\( id, body ) ->
                    if id == platformId then
                        ( id
                        , body
                            |> Physics.setVelocityTo
                                (Vector3d.metersPerSecond platformVx 0 0)
                        )

                    else
                        ( id, body )
                )
                bodies
    in
    ( { state | phase = newPhase }, newBodies )


platformBlock : Block3d Meters BodyCoordinates
platformBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 4, Length.meters 2, Length.meters 0.4 )


riderBlock : Block3d Meters BodyCoordinates
riderBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 0.8, Length.meters 0.8, Length.meters 0.8 )


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        platform =
            Physics.kinematic [ ( Shape.block platformBlock, Material.steel ) ]
                |> Physics.moveTo (Point3d.meters -platformAmplitude 0 0.2)

        rider =
            Physics.block riderBlock Material.wood

        bottomBox =
            rider |> Physics.moveTo (Point3d.meters -platformAmplitude 0 0.8)

        topBox =
            rider |> Physics.moveTo (Point3d.meters -platformAmplitude 0 1.6)
    in
    [ ( 0, floorBody )
    , ( platformId, platform )
    , ( 2, bottomBox )
    , ( 3, topBox )
    ]


initialMeshes : Array Meshes.Meshes
initialMeshes =
    let
        emptyMesh =
            Meshes.fromTriangles []

        platformMesh =
            Meshes.fromTriangles (Meshes.block platformBlock)

        riderMesh =
            Meshes.fromTriangles (Meshes.block riderBlock)
    in
    Array.fromList
        [ emptyMesh
        , platformMesh
        , riderMesh
        , riderMesh
        ]
