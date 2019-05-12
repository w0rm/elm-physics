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
import Common.Camera as Camera exposing (Camera)
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


onMouseDown : Camera -> ({ x : Float, y : Float, z : Float } -> msg) -> Attribute msg
onMouseDown camera fn =
    Events.on "mousedown" (coordinates camera fn)


onMouseMove : Camera -> ({ x : Float, y : Float, z : Float } -> msg) -> Attribute msg
onMouseMove camera fn =
    Events.on "mousemove" (coordinates camera fn)


onMouseUp : Camera -> ({ x : Float, y : Float, z : Float } -> msg) -> Attribute msg
onMouseUp camera fn =
    Events.on "mouseup" (coordinates camera fn)


coordinates : Camera -> ({ x : Float, y : Float, z : Float } -> msg) -> Decoder msg
coordinates camera fn =
    Decode.map2
        (\x y -> fn (Camera.mouseDirection camera x y))
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
