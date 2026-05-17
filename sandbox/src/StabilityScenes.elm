module StabilityScenes exposing (main)

{-| Interactive browser view for the stack-of-5 stability benchmark.

The current maxSpeed is shown in the top-left corner — watch it stay near zero
until the stack collapses around frame 443.

-}

import Array exposing (Array)
import Common.Demo as Demo
import Common.Meshes as Meshes exposing (Attributes)
import Duration
import Html exposing (Html)
import Html.Attributes exposing (style)
import Stability.Metrics as Metrics
import Stability.Scenarios as Scenarios
import WebGL exposing (Mesh)


type alias State =
    { frame : Int, score : Float }


main : Program () (Demo.Model Int State) (Demo.Msg msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = Scenarios.stackOf5.bodies
                , lookupMesh = \_ id -> Array.get id meshes
                , camera =
                    { from = { x = 0, y = 30, z = 20 }
                    , to = { x = 0, y = 0, z = 5 }
                    }
                , initialState = { frame = 0, score = 0 }
                }
    in
    Demo.program
        { base
            | floorOffset = Demo.floorAtZero
            , timestep = { duration = Duration.seconds (1 / 60), maxSteps = 1 }
            , postSimulate =
                \_ newBodies _ state ->
                    { frame = state.frame + 1
                    , score = (Metrics.compute newBodies).maxSpeed
                    }
            , overlay = \state _ _ -> scoreOverlay state.score state.frame
        }


meshes : Array (Mesh Attributes)
meshes =
    let
        empty =
            Meshes.fromTriangles []

        blockMesh =
            Meshes.fromTriangles (Meshes.block Scenarios.unitBlock)
    in
    Array.fromList (empty :: List.repeat 10 blockMesh)


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
