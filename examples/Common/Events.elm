module Common.Events exposing
    ( measureSize
    , onAnimationFrameDelta
    , onResize
    )

import Browser.Dom as Dom
import Browser.Events as Events
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
