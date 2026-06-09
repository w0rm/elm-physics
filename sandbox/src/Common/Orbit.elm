module Common.Orbit exposing
    ( Msg
    , Orbit
    , fromCartesian
    , setTarget
    , subscriptions
    , toCamera
    , update
    , wheelDecoder
    )

{-| Mouse-orbit camera state shared by every demo. Hold left mouse to
orbit, wheel to zoom.
-}

import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3


type Orbit
    = Orbit
        { azimuth : Float
        , elevation : Float
        , distance : Float
        , minDistance : Float
        , maxDistance : Float
        , target : { x : Float, y : Float, z : Float }
        , orbiting : Bool
        }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove Float Float
    | MouseWheel Float


fromCartesian :
    { from : { x : Float, y : Float, z : Float }
    , to : { x : Float, y : Float, z : Float }
    }
    -> Orbit
fromCartesian { from, to } =
    let
        dx =
            from.x - to.x

        dy =
            from.y - to.y

        dz =
            from.z - to.z

        distance =
            max 0.001 (sqrt (dx * dx + dy * dy + dz * dz))

        elevation =
            asin (clamp -1 1 (dz / distance))

        azimuth =
            atan2 dy dx
    in
    Orbit
        { azimuth = azimuth
        , elevation = elevation
        , distance = distance

        -- Zoom limits scale with the framing distance, so tiny scenes (mm
        -- dominoes) and wide ones (box piles) both get a usable zoom range.
        , minDistance = distance * 0.2
        , maxDistance = distance * 5
        , target = to
        , orbiting = False
        }


setTarget : { x : Float, y : Float, z : Float } -> Orbit -> Orbit
setTarget t (Orbit o) =
    Orbit { o | target = t }


fromPoint : Orbit -> { x : Float, y : Float, z : Float }
fromPoint (Orbit o) =
    { x = o.target.x + o.distance * cos o.elevation * cos o.azimuth
    , y = o.target.y + o.distance * cos o.elevation * sin o.azimuth
    , z = o.target.z + o.distance * sin o.elevation
    }


{-| Apply the orbit state to a camera, refreshing `from`, `to`, the camera
transform, and the distance-dependent perspective, but leaving the viewport
size untouched.
-}
toCamera : Orbit -> Camera -> Camera
toCamera ((Orbit o) as orbit) cam =
    let
        newFrom =
            fromPoint orbit
    in
    Camera.refreshPerspective
        { cam
            | from = newFrom
            , to = o.target
            , cameraTransform =
                Mat4.makeLookAt
                    (Vec3.fromRecord newFrom)
                    (Vec3.fromRecord o.target)
                    Vec3.k
        }


update : Msg -> Orbit -> Orbit
update msg ((Orbit o) as orbit) =
    case msg of
        MouseDown ->
            Orbit { o | orbiting = True }

        MouseUp ->
            Orbit { o | orbiting = False }

        MouseMove dx dy ->
            if o.orbiting then
                Orbit
                    { o
                        | azimuth = o.azimuth - dx * 0.005
                        , elevation =
                            clamp (degrees -85)
                                (degrees 85)
                                (o.elevation + dy * 0.005)
                    }

            else
                orbit

        MouseWheel deltaY ->
            Orbit
                { o
                    | distance =
                        clamp o.minDistance
                            o.maxDistance
                            (o.distance * (1 + deltaY * 0.001))
                }


subscriptions : (Msg -> msg) -> Orbit -> Sub msg
subscriptions toMsg (Orbit o) =
    if o.orbiting then
        Sub.batch
            [ Events.onMouseMove (mouseMoveDecoder toMsg)
            , Events.onMouseUp (Decode.succeed (toMsg MouseUp))
            ]

    else
        Events.onMouseDown (mouseDownDecoder toMsg)


{-| Mouse-down decoder that only fires for left-button presses on the
WebGL canvas. Right-clicks, scroll-wheel clicks, and clicks on any
HTML overlay (settings panel, buttons, etc.) are ignored so the camera
doesn't orbit when the user is interacting with UI.
-}
mouseDownDecoder : (Msg -> msg) -> Decoder msg
mouseDownDecoder toMsg =
    Decode.map2 Tuple.pair
        (Decode.field "button" Decode.int)
        (Decode.at [ "target", "tagName" ] Decode.string)
        |> Decode.andThen
            (\( button, tagName ) ->
                if button == 0 && tagName == "CANVAS" then
                    Decode.succeed (toMsg MouseDown)

                else
                    Decode.fail "ignored"
            )


mouseMoveDecoder : (Msg -> msg) -> Decoder msg
mouseMoveDecoder toMsg =
    Decode.map2 (\x y -> toMsg (MouseMove x y))
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


{-| Wheel decoder for use with `Html.Events.preventDefaultOn "wheel"`
to keep the page from scrolling while zooming.
-}
wheelDecoder : (Msg -> msg) -> Decoder ( msg, Bool )
wheelDecoder toMsg =
    Decode.map (\dy -> ( toMsg (MouseWheel dy), True ))
        (Decode.field "deltaY" Decode.float)
