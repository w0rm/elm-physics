module Boxes exposing (main)

{-| This demo is used to test performance. It drops 4×4×4 boxes.
Try changing `boxesPerDimension` to drop even more!
-}

import Browser
import Common.Events as Events
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Meshes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Physics.Body as Body exposing (Body)
import Physics.World as World exposing (World)


boxesPerDimension =
    4


type alias Model =
    { world : World Meshes
    , fps : List Float
    , settings : Settings
    , width : Float
    , height : Float
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
      , width = 0
      , height = 0
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
                , world = World.simulate (1000 / 60) model.world
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = width, height = height }
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
view { settings, fps, world, width, height } =
    Html.div []
        [ Scene.view
            { settings = settings
            , world = world
            , width = width
            , height = height
            , meshes = identity
            , raycastResult = Nothing
            , floorOffset = Just floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length (World.getBodies world))

          else
            Html.text ""
        ]


initialWorld : World Meshes
initialWorld =
    World.empty
        |> World.setGravity { x = 0, y = 0, z = -10 }
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
                                |> Body.moveBy
                                    { x = (x - (boxesPerDimension - 1) / 2) * distance
                                    , y = (y - (boxesPerDimension - 1) / 2) * distance
                                    , z = (z + (2 * boxesPerDimension + 1) / 2) * distance
                                    }
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
        |> Body.setMass 0
        |> Body.setPosition floorOffset


box : Body Meshes
box =
    let
        size =
            { x = 1, y = 1, z = 1 }
    in
    Meshes.box size
        |> Meshes.fromTriangles
        |> Body.box size
        |> Body.setMass 5
