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
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Density
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
    { bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : Physics.Contacts Int
    , fps : List Float
    , settings : Settings
    , camera : Camera
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
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
    ( { bodies = initialBodies
      , meshes = initialMeshes
      , contacts = Physics.emptyContacts
      , fps = []
      , settings = settings
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
            ( { model | bodies = initialBodies, meshes = initialMeshes, contacts = Physics.emptyContacts }, Cmd.none )


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
            Fps.view fps (List.length bodies) c.iterations

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
                |> Physics.scaleTo (Mass.kilograms 5)

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
