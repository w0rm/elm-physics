module Randomize exposing (main)

{-| This demo drops random bodies.
It also shows how to make a compound body out of multiple shapes.
-}

import Angle
import Array exposing (Array)
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Cylinder3d
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics exposing (Body, onEarth)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material as Material
import Physics.Shape as Shape
import Point3d exposing (Point3d)
import Random
import Sphere3d
import Task
import Vector3d
import WebGL exposing (Mesh)


type BodyShape
    = BoxShape
    | SphereShape
    | CylinderShape
    | CompoundShape


type alias Model =
    { bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : Physics.Contacts Int
    , nextId : Int
    , fps : List Float
    , settings : Settings
    , camera : Camera
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart
    | Random
    | AddRandom ( Body, Mesh Attributes )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( bodies, meshes, nextId ) =
            initialBodiesAndMeshes
    in
    ( { bodies = bodies
      , contacts = Physics.emptyContacts
      , meshes = meshes
      , nextId = nextId
      , fps = []
      , settings = { settings | showSettings = True }
      , camera =
            Camera.camera
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
                }
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model
                | settings = Settings.update settingsMsg model.settings
              }
            , Cmd.none
            )

        Tick dt ->
            let
                ( newBodies, newContacts ) =
                    Physics.simulate
                        { onEarth | contacts = model.contacts }
                        model.bodies
            in
            ( { model
                | fps = Fps.update dt model.fps
                , bodies = newBodies
                , contacts = newContacts
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            let
                ( bodies, meshes, nextId ) =
                    initialBodiesAndMeshes
            in
            ( { model | bodies = bodies, meshes = meshes, nextId = nextId, contacts = Physics.emptyContacts }, Cmd.none )

        Random ->
            ( model, Random.generate AddRandom randomBody )

        AddRandom ( body, mesh ) ->
            ( { model
                | bodies = ( model.nextId, body ) :: model.bodies
                , meshes = Array.push mesh model.meshes
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, bodies, contacts, meshes, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , bodies = List.filterMap (\( id, body ) -> Maybe.map (\mesh -> ( mesh, body )) (Array.get id meshes)) bodies
            , contacts = List.concatMap (\( _, _, c ) -> c) (Physics.contacts contacts)
            , camera = camera
            , floorOffset = floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Random ]
                [ Html.text "Drop a random body" ]
            , Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length bodies) (Physics.solverIterations contacts)

          else
            Html.text ""
        ]


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


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
        |> Physics.scaleTo (Mass.kilograms 5)


makeSphere : Body
makeSphere =
    Physics.sphere sphere3d Material.wood
        |> Physics.scaleTo (Mass.kilograms 5)


makeCylinder : Body
makeCylinder =
    Physics.cylinder cylinder3d Material.wood
        |> Physics.scaleTo (Mass.kilograms 5)


makeCompound : Body
makeCompound =
    Physics.dynamic
        (List.map (\b -> ( Shape.block b, Material.wood )) compoundBlocks)
        |> Physics.scaleTo (Mass.kilograms 5)


initialBodiesAndMeshes : ( List ( Int, Body ), Array (Mesh Attributes), Int )
initialBodiesAndMeshes =
    let
        -- id=0 floor, 1 box, 2 sphere, 3 cylinder, 4 compound
        floorBody =
            Physics.plane Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

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

        compoundBody =
            makeCompound
                |> Physics.rotateAround
                    (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 }))
                    (Angle.radians (pi / 5))
                |> Physics.moveTo (Point3d.meters -1.2 0 5)

        bodies =
            [ ( 0, floorBody )
            , ( 1, boxBody )
            , ( 2, sphereBody )
            , ( 3, cylinderBody )
            , ( 4, compoundBody )
            ]

        meshes =
            Array.fromList
                [ Meshes.fromTriangles []
                , Meshes.fromTriangles (Meshes.block boxBlock3d)
                , Meshes.fromTriangles (Meshes.sphere 2 sphere3d)
                , Meshes.fromTriangles (Meshes.cylinder 12 cylinder3d)
                , Meshes.fromTriangles (List.concatMap Meshes.block compoundBlocks)
                ]
    in
    ( bodies, meshes, 5 )


{-| A random body raised above the plane, shifted or rotated to a random 3d angle
-}
randomBody : Random.Generator ( Body, Mesh Attributes )
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

                        _ ->
                            ( makeCompound, Meshes.fromTriangles (List.concatMap Meshes.block compoundBlocks) )
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
        (Random.int 0 3)
