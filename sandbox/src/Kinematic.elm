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
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Physics exposing (Body, BodyCoordinates, onEarth)
import Physics.Material as Material
import Physics.Shape as Shape
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d
import Task
import Vector3d
import WebGL exposing (Mesh)


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


simDt : Float
simDt =
    1 / 60


type alias Model =
    { bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : Physics.Contacts Int
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , phase : Float
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
      , settings = { settings | showFpsMeter = True }
      , camera =
            Camera.camera
                { from = { x = 0, y = 18, z = 10 }
                , to = { x = 0, y = 0, z = 1 }
                }
      , phase = -pi / 2
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model | settings = Settings.update settingsMsg model.settings }
            , Cmd.none
            )

        Tick dt ->
            let
                newPhase =
                    model.phase + simDt * platformOmega

                platformVx =
                    peakSpeed * cos newPhase

                bodiesPreSim =
                    model.bodies
                        |> List.map
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

                ( simulated, newContacts ) =
                    Physics.simulate
                        { onEarth | contacts = model.contacts }
                        bodiesPreSim
            in
            ( { model
                | fps = Fps.update dt model.fps
                , bodies = simulated
                , contacts = newContacts
                , phase = newPhase
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model
                | bodies = initialBodies
                , contacts = Physics.emptyContacts
                , phase = -pi / 2
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
view { settings, fps, bodies, meshes, contacts, camera } =
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
            [ Html.button [ onClick Restart ] [ Html.text "Restart the demo" ] ]
        , if settings.showFpsMeter then
            let
                (Contacts c) =
                    contacts
            in
            Fps.view fps (List.length bodies) c.iterations

          else
            Html.text ""
        ]


floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


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
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

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


initialMeshes : Array (Mesh Attributes)
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
        [ emptyMesh -- 0: floor
        , platformMesh -- 1: platform
        , riderMesh -- 2: bottom box
        , riderMesh -- 3: top box
        ]
