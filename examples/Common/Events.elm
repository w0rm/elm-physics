module Common.Events exposing
    ( measureSize
    , onAnimationFrameDelta
    , onMouseDown
    , onMouseMove
    , onMouseUp
    , onResize
    )

{-| Helpers for some common events,
subscriptions and commands
-}

import Browser.Dom as Dom
import Browser.Events as Events
import Common.Math as Math
import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Task


measureSize : (Float -> Float -> msg) -> Cmd msg
measureSize fn =
    Task.perform
        (\{ viewport } -> fn viewport.width viewport.height)
        Dom.getViewport


onResize : (Float -> Float -> msg) -> Sub msg
onResize fn =
    Events.onResize (\w h -> fn (toFloat w) (toFloat h))


onAnimationFrameDelta : (Float -> msg) -> Sub msg
onAnimationFrameDelta =
    Events.onAnimationFrameDelta


onMouseDown : (Float -> Float -> msg) -> Attribute msg
onMouseDown fn =
    Events.on "mousedown" (coordinates fn)


onMouseMove : (Float -> Float -> msg) -> Attribute msg
onMouseMove fn =
    Events.on "mousemove" (coordinates fn)


onMouseUp : (Float -> Float -> msg) -> Attribute msg
onMouseUp fn =
    Events.on "mouseup" (coordinates fn)


coordinates : (Float -> Float -> msg) -> Decoder msg
coordinates fn =
    Decode.map2 fn
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
