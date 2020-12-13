module Dominoes exposing (main)

{-| This demo is used to show custom materials.
Without the slippy material, dominoes would not slide along each other.
Try to make the floor slippy too!
-}

import Acceleration
import Angle
import Axis3d
import Block3d
import Browser
import Common.Camera as Camera exposing (Camera)
import Common.Events as Events
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Meshes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Direction3d
import Duration
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Material as Material exposing (Material)
import Physics.World as World exposing (World)
import Point3d


type alias Model =
    { world : World Meshes
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
    ( { world = initialWorld
      , fps = []
      , settings = settings
      , camera =
            Camera.camera
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
                }
      }
    , Events.measureSize Resize
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
            ( { model
                | fps = Fps.update dt model.fps
                , world = World.simulate (Duration.seconds (1 / 60)) model.world
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model | world = initialWorld }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, world, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , world = world
            , camera = camera
            , meshes = identity
            , maybeRaycastResult = Nothing
            , floorOffset = floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length (World.bodies world))

          else
            Html.text ""
        ]


initialWorld : World Meshes
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        |> World.add
            (domino
                |> Body.rotateAround Axis3d.y (Angle.radians (pi / 8))
                |> Body.rotateAround Axis3d.z (Angle.radians (pi / 4))
                |> Body.moveTo (Point3d.meters -5.5 -5.5 0)
            )
        |> addDominos


addDominos : World Meshes -> World Meshes
addDominos world =
    List.foldl
        (\i ->
            domino
                |> Body.rotateAround Axis3d.z (Angle.radians (pi / 4))
                |> Body.moveTo (Point3d.meters (toFloat (5 - i)) (toFloat (5 - i)) 0)
                |> World.add
        )
        world
        (List.range 0 10)


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body Meshes
floor =
    Body.plane (Meshes.fromTriangles [])
        |> Body.moveTo (Point3d.fromMeters floorOffset)


{-| A domino piece
-}
domino : Body Meshes
domino =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 0.1
                , Length.meters 1
                , Length.meters 2
                )
    in
    Body.block block3d
        (Meshes.fromTriangles (Meshes.block block3d))
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))
        |> Body.withMaterial slippy


slippy : Material
slippy =
    Material.custom
        { bounciness = 0
        , friction = 0.001
        }
