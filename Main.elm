module Dice exposing (main)

import Html exposing (Html)
import Window
import Task
import Types exposing (..)
import View exposing (view)
import AnimationFrame
import Physics.Quaternion as Quaternion
import Physics
import Physics.World as Physics
import Physics.Body as Physics
import Physics.Shape as Physics
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


ground : Physics.Body
ground =
    Physics.body
        |> Physics.setPosition (vec3 0 0 -1)
        |> Physics.setQuaternion (Quaternion.fromAngleAxis 0 Vec3.j)
        |> Physics.addShape Physics.Plane


box : Vec3 -> Physics.Body
box position =
    Physics.body
        |> Physics.setMass 5
        |> Physics.setPosition position
        |> Physics.addShape (Physics.Box (vec3 1 1 1))


init : ( Model, Cmd Msg )
init =
    ( { screenWidth = 1
      , screenHeight = 1
      , world =
            Physics.world
                |> Physics.setGravity (vec3 0 0 -10)
                |> Physics.addBody ground
                |> Physics.addBody (box (vec3 0 0 2))
                |> Physics.addBody
                    (box (vec3 1 0 6)
                     --                        |> Physics.setQuaternion (Quaternion.fromAngleAxis (pi / 5) Vec3.j)
                    )
      , devicePixelRatio = 2
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize { width, height } ->
            ( { model
                | screenWidth = toFloat width
                , screenHeight = toFloat height
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model | world = Physics.step (dt / 1000) model.world }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]
