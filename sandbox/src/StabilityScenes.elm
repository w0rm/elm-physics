module StabilityScenes exposing (main)

{-| Interactive browser view for the solver stability benchmarks.

A dropdown in the settings panel switches between the scenarios exercised by
`StabilityTest`. The current maxSpeed is shown top-left — for the resting
scenes it should hover near zero; for the slope it stays tiny while the box
grips the incline.

-}

import Array exposing (Array)
import Common.Demo as Demo
import Common.Meshes as Meshes
import Duration
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Physics
import Stability.Scenarios as Scenarios



-- Scene options


type Scene
    = StackOf5
    | StackOf5Dropped
    | Slope
    | CylinderStack
    | CylinderStackDropped


allScenes : List Scene
allScenes =
    [ StackOf5, StackOf5Dropped, Slope, CylinderStack, CylinderStackDropped ]


sceneName : Scene -> String
sceneName scene =
    case scene of
        StackOf5 ->
            "Stack of 5"

        StackOf5Dropped ->
            "Stack of 5 (dropped)"

        Slope ->
            "Box on a slope"

        CylinderStack ->
            "Stack of 5 cylinders"

        CylinderStackDropped ->
            "Stack of 5 cylinders (dropped)"


sceneFromName : String -> Scene
sceneFromName name =
    case name of
        "Stack of 5 (dropped)" ->
            StackOf5Dropped

        "Box on a slope" ->
            Slope

        "Stack of 5 cylinders" ->
            CylinderStack

        "Stack of 5 cylinders (dropped)" ->
            CylinderStackDropped

        _ ->
            StackOf5


sceneBodies : Scene -> List ( Int, Physics.Body )
sceneBodies scene =
    case scene of
        StackOf5 ->
            Scenarios.stackOf5.bodies

        StackOf5Dropped ->
            Scenarios.stackOf5Dropped.bodies

        Slope ->
            Scenarios.restingOnSlope.bodies

        CylinderStack ->
            Scenarios.stackOfCylinders.bodies

        CylinderStackDropped ->
            Scenarios.stackOfCylindersDropped.bodies



-- State / Msg


type alias State =
    { scene : Scene }


type Msg
    = SelectScene String


initialScene : Scene
initialScene =
    StackOf5


main : Program () (Demo.Model Int State) (Demo.Msg Msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = sceneBodies initialScene
                , lookupMesh =
                    \state id ->
                        case state.scene of
                            Slope ->
                                Array.get id slopeMeshes

                            CylinderStack ->
                                Array.get id cylinderMeshes

                            CylinderStackDropped ->
                                Array.get id cylinderMeshes

                            _ ->
                                Array.get id stackMeshes
                , camera =
                    { from = { x = 0, y = 30, z = 20 }
                    , to = { x = 0, y = 0, z = 2.5 }
                    }
                , initialState = { scene = initialScene }
                }
    in
    Demo.program
        { base
            | floorOffset = Demo.floorAtZero
            , timestep = { duration = Duration.seconds (1 / 60), maxSteps = 1 }
            , update =
                \(SelectScene name) _ _ ->
                    let
                        scene =
                            sceneFromName name
                    in
                    ( { scene = scene }
                    , sceneBodies scene
                    , Cmd.none
                    )

            -- Restart keeps the selected scene but replays it from the start.
            , reset = \state -> { scene = state.scene }
            , restartBodies = \state -> sceneBodies state.scene
            , buttons = \state -> [ sceneSelect state.scene ]
        }


{-| Stack scenes: id 0 is the floor plane (no mesh), ids 1.. are unit boxes.
-}
stackMeshes : Array Meshes.Meshes
stackMeshes =
    Array.fromList
        (Meshes.fromTriangles []
            :: List.repeat 10 (Meshes.fromTriangles (Meshes.block Scenarios.unitBlock))
        )


{-| Cylinder stack: id 0 floor, ids 1.. unit cylinders; 12-gon mesh matches the hull.
-}
cylinderMeshes : Array Meshes.Meshes
cylinderMeshes =
    Array.fromList
        (Meshes.fromTriangles []
            :: List.repeat 10 (Meshes.fromTriangles (Meshes.cylinder 12 Scenarios.unitCylinder))
        )


{-| Slope scene: id 0 is the static ramp slab, id 1 is the box.
-}
slopeMeshes : Array Meshes.Meshes
slopeMeshes =
    Array.fromList
        [ Meshes.fromTriangles (Meshes.block Scenarios.slopeRamp)
        , Meshes.fromTriangles (Meshes.block Scenarios.unitBlock)
        ]


sceneSelect : Scene -> Html Msg
sceneSelect current =
    Html.select
        [ Html.Events.onInput SelectScene
        , style "display" "block"
        , style "box-sizing" "border-box"
        , style "width" "100%"
        , style "padding" "6px 22px 6px 6px"
        , style "margin" "0"
        , style "border" "none"
        , style "color" "inherit"
        , style "font" "inherit"
        , style "text-align" "center"
        , style "cursor" "pointer"
        , style "appearance" "none"
        , style "-webkit-appearance" "none"
        , style "background-color" "rgb(61, 61, 61)"
        , style "background-image"
            "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12'><path fill='white' d='M2 4 L6 8 L10 4 Z'/></svg>\")"
        , style "background-repeat" "no-repeat"
        , style "background-position" "right 6px center"
        , style "background-size" "12px 12px"
        ]
        (List.map
            (\scene ->
                Html.option
                    [ Html.Attributes.value (sceneName scene)
                    , Html.Attributes.selected (scene == current)
                    ]
                    [ Html.text (sceneName scene) ]
            )
            allScenes
        )
