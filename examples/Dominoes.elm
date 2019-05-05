module Dominoes exposing (main)

{-| This demo is used to show custom materials.
Without the slippy material, dominoes would not slide along each other.
Try to make the floor slippy too!
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
import Physics.Material as Material exposing (Material)
import Physics.World as World exposing (World)


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
        |> World.add
            (domino
                |> Body.rotateBy (pi / 8) { x = 0, y = 1, z = 0 }
                |> Body.rotateBy (pi / 4) { x = 0, y = 0, z = 1 }
                |> Body.moveBy { x = -5.5, y = -5.5, z = 0 }
            )
        |> addDominos


addDominos : World Meshes -> World Meshes
addDominos world =
    List.foldl
        (\i ->
            domino
                |> Body.rotateBy (pi / 4) { x = 0, y = 0, z = 1 }
                |> Body.moveBy
                    { x = toFloat (5 - i)
                    , y = toFloat (5 - i)
                    , z = 0
                    }
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
        |> Body.setMass 0
        |> Body.setPosition floorOffset


{-| A domino piece
-}
domino : Body Meshes
domino =
    let
        size =
            { x = 0.1, y = 1, z = 2 }
    in
    Meshes.box size
        |> Meshes.fromTriangles
        |> Body.box size
        |> Body.setMass 5
        |> Body.setMaterial slippy


slippy : Material
slippy =
    Material.custom
        { bounciness = 0
        , friction = 0.001
        }
