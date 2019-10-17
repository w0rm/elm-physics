module Physics.Coordinates exposing (BodyLocalCoordinates, ShapeLocalCoordinates, WorldCoordinates)

import Internal.Coordinates as Coordinates


type alias WorldCoordinates =
    Coordinates.WorldCoordinates


type alias BodyLocalCoordinates =
    Coordinates.BodyLocalCoordinates


type alias ShapeLocalCoordinates =
    Coordinates.ShapeLocalCoordinates
