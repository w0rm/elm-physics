module Types exposing (..)

import Window exposing (Size)
import Time exposing (Time)
import Physics.World as Physics


type alias Model =
    { screenWidth : Float
    , screenHeight : Float
    , world : Physics.World
    , devicePixelRatio : Float
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | AddCube
