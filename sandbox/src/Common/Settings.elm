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

import Html exposing (Html)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onCheck, onClick)


type alias Settings =
    { debugContacts : Bool -- Set to True to see collision points
    , debugContactIds : Bool -- Set to True to see per-contact id labels (Collisions demo)
    , debugWireframes : Bool -- Set to True to see wireframes
    , debugCenterOfMass : Bool -- Set to True to see center of mass
    , debugInertia : Bool -- Set to True to see principal-axis inertia ellipsoid
    , showSettings : Bool
    }


type SettingsMsg
    = ToggleContacts Bool
    | ToggleContactIds Bool
    | ToggleWireframes Bool
    | ToggleCenterOfMass Bool
    | ToggleInertia Bool
    | ToggleSettings


settings : Settings
settings =
    { debugContacts = False
    , debugContactIds = False
    , debugWireframes = False
    , debugCenterOfMass = False
    , debugInertia = False
    , showSettings = False
    }


update : SettingsMsg -> Settings -> Settings
update msg model =
    case msg of
        ToggleSettings ->
            { model | showSettings = not model.showSettings }

        ToggleContacts debugContacts ->
            { model | debugContacts = debugContacts }

        ToggleContactIds debugContactIds ->
            { model | debugContactIds = debugContactIds }

        ToggleWireframes debugWireframes ->
            { model | debugWireframes = debugWireframes }

        ToggleCenterOfMass debugCenterOfMass ->
            { model | debugCenterOfMass = debugCenterOfMass }

        ToggleInertia debugInertia ->
            { model | debugInertia = debugInertia }


view :
    (SettingsMsg -> msg)
    -> Settings
    -> List (Html msg)
    -> List (Html msg)
    -> Html msg
view msg { showSettings, debugContacts, debugContactIds, debugWireframes, debugCenterOfMass, debugInertia } header extraContent =
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
                (List.map wrapBelow header
                    ++ [ contactsRow msg debugContacts debugContactIds
                       , centerRow msg debugCenterOfMass debugInertia
                       , checkbox (ToggleWireframes >> msg) debugWireframes "wireframes"
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


wrapBelow : Html msg -> Html msg
wrapBelow el =
    Html.div [ style "margin" "5px 0 10px" ] [ el ]


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


inlineCheckbox : (Bool -> msg) -> Bool -> String -> Html msg
inlineCheckbox msg value label =
    Html.label [ style "display" "inline-flex", style "align-items" "center" ]
        [ Html.input
            [ onCheck msg
            , checked value
            , type_ "checkbox"
            , style "margin-right" "10px"
            ]
            []
        , Html.text label
        ]


contactsRow : (SettingsMsg -> msg) -> Bool -> Bool -> Html msg
contactsRow msg debugContacts debugContactIds =
    Html.div
        [ style "display" "flex"
        , style "gap" "10px"
        , style "padding" "5px 0"
        , style "flex-wrap" "wrap"
        ]
        (inlineCheckbox (ToggleContacts >> msg) debugContacts "collisions"
            :: (if debugContacts then
                    [ inlineCheckbox (ToggleContactIds >> msg) debugContactIds "ids" ]

                else
                    []
               )
        )


centerRow : (SettingsMsg -> msg) -> Bool -> Bool -> Html msg
centerRow msg debugCenterOfMass debugInertia =
    Html.div
        [ style "display" "flex"
        , style "gap" "10px"
        , style "padding" "5px 0"
        , style "flex-wrap" "wrap"
        ]
        (inlineCheckbox (ToggleCenterOfMass >> msg) debugCenterOfMass "centers"
            :: (if debugCenterOfMass then
                    [ inlineCheckbox (ToggleInertia >> msg) debugInertia "inertia" ]

                else
                    []
               )
        )
