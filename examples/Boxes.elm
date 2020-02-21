module Boxes exposing (main)

{-| This demo is used to test performance. It drops 4×4×4 boxes.
Try changing `boxesPerDimension` to drop even more!
-}

import Acceleration
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
import Physics.World as World exposing (World)
import Point3d


boxesPerDimension : number
boxesPerDimension =
    4


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
      , settings = { settings | showFpsMeter = True }
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
        |> addBoxes


addBoxes : World Meshes -> World Meshes
addBoxes world =
    let
        dimensions =
            List.map toFloat (List.range 0 (boxesPerDimension - 1))

        distance =
            1
    in
    List.foldl
        (\x world1 ->
            List.foldl
                (\y world2 ->
                    List.foldl
                        (\z ->
                            box
                                |> Body.moveTo
                                    (Point3d.meters
                                        ((x - (boxesPerDimension - 1) / 2) * distance)
                                        ((y - (boxesPerDimension - 1) / 2) * distance)
                                        ((z + (2 * boxesPerDimension + 1) / 2) * distance)
                                    )
                                |> World.add
                        )
                        world2
                        dimensions
                )
                world1
                dimensions
        )
        world
        dimensions


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


box : Body Meshes
box =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1, Length.meters 1, Length.meters 1 )
    in
    Body.block block3d
        (Meshes.fromTriangles (Meshes.block block3d))
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))
