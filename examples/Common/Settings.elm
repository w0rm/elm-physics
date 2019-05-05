module Common.Settings exposing
    ( Settings
    , SettingsMsg
    , settings
    , update
    , view
    )

{-| This module is used to render the settings panel.
More controls can be injected with view’s extraContent.
-}

import Browser
import Html exposing (Html)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (on, onCheck, onClick)


type alias Settings =
    { debugContacts : Bool -- Set to True to see collision points
    , debugNormals : Bool -- Set to True to see normal spikes
    , debugEdges : Bool -- Set to True to see edge markers
    , debugWireframes : Bool -- Set to True to see wireframes
    , showFpsMeter : Bool
    , showSettings : Bool
    }


type SettingsMsg
    = ToggleContacts Bool
    | ToggleNormals Bool
    | ToggleEdges Bool
    | ToggleWireframes Bool
    | ToggleFpsMeter Bool
    | ToggleSettings


settings : Settings
settings =
    { debugContacts = False
    , debugNormals = False
    , debugEdges = False
    , debugWireframes = False
    , showSettings = False
    , showFpsMeter = False
    }


update : SettingsMsg -> Settings -> Settings
update msg model =
    case msg of
        ToggleSettings ->
            { model | showSettings = not model.showSettings }

        ToggleContacts debugContacts ->
            { model | debugContacts = debugContacts }

        ToggleNormals debugNormals ->
            { model | debugNormals = debugNormals }

        ToggleEdges debugEdges ->
            { model | debugEdges = debugEdges }

        ToggleWireframes debugWireframes ->
            { model | debugWireframes = debugWireframes }

        ToggleFpsMeter showFpsMeter ->
            { model | showFpsMeter = showFpsMeter }


view : (SettingsMsg -> msg) -> Settings -> List (Html msg) -> Html msg
view msg { showSettings, debugContacts, debugNormals, debugEdges, debugWireframes, showFpsMeter } extraContent =
    Html.div
        [ style "position" "fixed"
        , style "right" "6px"
        , style "top" "0"
        , style "font-family" "monospace"
        , style "color" "white"
        ]
        (if showSettings then
            [ button (msg ToggleSettings) "Hide Settings"
            , Html.div
                [ style "padding" "6px"
                , style "min-width" "24ch"
                , style "background" "rgb(50, 50, 50)"
                , style "border-radius" "0 0 4px 4px"
                ]
                ([ checkbox (ToggleContacts >> msg) debugContacts "collision points"
                 , checkbox (ToggleNormals >> msg) debugNormals "normals"
                 , checkbox (ToggleEdges >> msg) debugEdges "unique edges"
                 , checkbox (ToggleWireframes >> msg) debugWireframes "wireframes"
                 , checkbox (ToggleFpsMeter >> msg) showFpsMeter "fps meter"
                 ]
                    ++ List.map wrapWithMargin extraContent
                )
            ]

         else
            [ button (msg ToggleSettings) "Show Settings" ]
        )


wrapWithMargin : Html msg -> Html msg
wrapWithMargin el =
    Html.div [ style "margin" "10px 0 5px" ] [ el ]


button : msg -> String -> Html msg
button msg text =
    Html.button
        [ style "padding" "6px"
        , style "box-sizing" "content-box"
        , style "min-width" "24ch"
        , style "color" "inherit"
        , style "border" "none"
        , style "font" "inherit"
        , style "text-align" "center"
        , style "margin" "0"
        , style "display" "block"
        , style "background" "rgb(61, 61, 61)"
        , onClick msg
        ]
        [ Html.text text ]


checkbox : (Bool -> msg) -> Bool -> String -> Html msg
checkbox msg value label =
    Html.label [ style "display" "block", style "padding" "5px 0" ]
        [ Html.input
            [ onCheck msg
            , checked value
            , type_ "checkbox"
            , style "margin-right" "10px"
            ]
            []
        , Html.text label
        ]
