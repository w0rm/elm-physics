module Dominoes exposing (main)

{-| This demo is used to show custom materials.
Without the slippy material, dominoes would not slide along each other.
Try to make the floor slippy too!
-}

import Angle
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Timestep exposing (Timestep)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Common.Sps as Sps exposing (Sps)
import Density
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics exposing (Body, BodyCoordinates, WorldCoordinates, onEarth)
import Physics.Material as Material exposing (Material)
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d exposing (Point3d)
import Task
import WebGL exposing (Mesh)


type alias Model =
    { prevBodies : List ( Int, Body )
    , bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : Physics.Contacts Int
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , timestep : Timestep
    , sps : Sps
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Duration
    | Resize Float Float
    | Restart


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
    ( { prevBodies = initialBodies
      , bodies = initialBodies
      , meshes = initialMeshes
      , contacts = Physics.emptyContacts
      , fps = []
      , settings = settings
      , camera =
            Camera.camera
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
                }
      , timestep = Timestep.init { duration = Duration.seconds (1 / 120), maxSteps = 2 }
      , sps = Sps.init
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
                next =
                    Timestep.advance simulateStep dt model
            in
            ( { next
                | fps = Fps.update dt next.fps
                , sps = Sps.update dt (Timestep.steps next.timestep) next.sps
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model
                | prevBodies = initialBodies
                , bodies = initialBodies
                , meshes = initialMeshes
                , contacts = Physics.emptyContacts
              }
            , Cmd.none
            )


simulateStep : Model -> Model
simulateStep model =
    let
        ( newBodies, newContacts ) =
            Physics.simulate
                { onEarth | duration = Timestep.duration model.timestep, contacts = model.contacts }
                model.bodies
    in
    { model | prevBodies = model.bodies, bodies = newBodies, contacts = newContacts }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        ]


view : Model -> Html Msg
view { settings, fps, sps, prevBodies, bodies, contacts, meshes, camera, timestep } =
    Html.div []
        [ Scene.view
            { settings = settings
            , bodies = Scene.interpolatedBodies (Timestep.progress timestep) prevBodies bodies (\id -> Array.get id meshes)
            , contacts = List.concatMap (\( _, _, c ) -> c) (Physics.contactPoints (\_ _ -> True) contacts)
            , camera = camera
            , floorOffset = floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            let
                (Contacts c) =
                    contacts
            in
            Html.div []
                [ Fps.view fps (List.length bodies) c.iterations
                , Sps.view sps
                ]

          else
            Html.text ""
        ]


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


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
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        dominoBody =
            Physics.block dominoBlock3d slippy
                |> Physics.scaleMassTo (Mass.kilograms 5)

        -- id=0 floor, id=1 tilted first domino, ids 2..12 regular dominos
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

        -- 13 bodies total: floor + 1 tilted + 11 regular
        dominoCount =
            12
    in
    Array.fromList (floorMesh :: List.repeat dominoCount dominoMesh)
