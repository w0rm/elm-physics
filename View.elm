module View exposing (view)

import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Html exposing (Html)
import WebGL
import Types exposing (..)
import Render


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0 0 0 1
        ]
        [ width (round (model.screenWidth * model.devicePixelRatio))
        , height (round (model.screenHeight * model.devicePixelRatio))
        , style
            [ ( "display", "block" )
            , ( "width", toString model.screenWidth ++ "px" )
            , ( "height", toString model.screenHeight ++ "px" )
            ]
        , onClick AddCube
        ]
        (Render.world model)
