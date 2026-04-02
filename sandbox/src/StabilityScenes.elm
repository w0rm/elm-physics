module StabilityScenes exposing (main)

{-| Interactive browser view for the stack-of-5 stability benchmark.

The current maxSpeed is shown in the top-left corner — watch it stay near zero
until the stack collapses around frame 443.

-}

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Length exposing (Meters)
import Physics exposing (Body, onEarth)
import Physics.Coordinates exposing (WorldCoordinates)
import Point3d exposing (Point3d)
import Stability.Metrics as Metrics
import Stability.Scenarios as Scenarios
import Task
import WebGL exposing (Mesh)


type alias Model =
    { bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : List ( Int, Int, List (Point3d Meters WorldCoordinates) )
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , frame : Int
    , score : Float
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float


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
    ( initialModel
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


initialModel : Model
initialModel =
    { bodies = Scenarios.stackOf5.bodies
    , meshes = meshes
    , contacts = []
    , fps = []
    , settings = settings
    , camera =
        Camera.camera
            { from = { x = 0, y = 30, z = 20 }
            , to = { x = 0, y = 0, z = 5 }
            }
    , frame = 0
    , score = 0
    }


meshes : Array (Mesh Attributes)
meshes =
    let
        empty =
            Meshes.fromTriangles []

        blockMesh =
            Meshes.fromTriangles (Meshes.block Scenarios.unitBlock)
    in
    Array.fromList (empty :: List.repeat 5 blockMesh)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model | settings = Settings.update settingsMsg model.settings }
            , Cmd.none
            )

        Tick dt ->
            let
                ( newBodies, newContacts ) =
                    Physics.simulate onEarth model.bodies

                newScore =
                    (Metrics.compute newBodies).maxSpeed
            in
            ( { model
                | fps = Fps.update dt model.fps
                , bodies = newBodies
                , contacts = newContacts
                , frame = model.frame + 1
                , score = newScore
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, bodies, contacts, camera, score, frame } =
    Html.div []
        [ Scene.view
            { settings = settings
            , bodies = List.filterMap (\( id, body ) -> Maybe.map (\mesh -> ( mesh, body )) (Array.get id meshes)) bodies
            , contacts = List.concatMap (\( _, _, c ) -> c) contacts
            , camera = camera
            , floorOffset = { x = 0, y = 0, z = 0 }
            }
        , scoreOverlay score frame
        , Settings.view ForSettings settings []
        , if settings.showFpsMeter then
            Fps.view fps (List.length bodies)

          else
            Html.text ""
        ]


scoreOverlay : Float -> Int -> Html msg
scoreOverlay s frameNum =
    Html.div
        [ style "position" "fixed"
        , style "left" "6px"
        , style "top" "0"
        , style "font-family" "monospace"
        , style "color" "white"
        , style "padding" "6px"
        , style "background" "rgba(0,0,0,0.4)"
        , style "border-radius" "0 0 4px 0"
        , style "pointer-events" "none"
        ]
        [ Html.div [] [ Html.text ("maxSpeed  " ++ formatFloat s ++ " m/s") ]
        , Html.div [] [ Html.text ("frame  " ++ String.fromInt frameNum) ]
        ]


formatFloat : Float -> String
formatFloat f =
    let
        s =
            String.fromFloat (toFloat (round (f * 1000)) / 1000)
    in
    if String.contains "." s then
        s

    else
        s ++ ".0"
