module Internal.Coordinates exposing (BodyLocalCoordinates, ShapeLocalCoordinates, ShapeWorldFrame3d, WorldCoordinates)

import Frame3d exposing (Frame3d)
import Length exposing (Meters)


type WorldCoordinates
    = WorldCoordinates


type BodyLocalCoordinates
    = BodyLocalCoordinates


type ShapeLocalCoordinates
    = ShapeLocalCoordinates


type alias ShapeWorldFrame3d =
    Frame3d Meters WorldCoordinates { defines : ShapeLocalCoordinates }
