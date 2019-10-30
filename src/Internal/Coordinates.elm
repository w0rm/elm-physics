module Internal.Coordinates exposing (ShapeWorldFrame3d)

import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Physics.Coordinates exposing (ShapeCoordinates, WorldCoordinates)


type alias ShapeWorldFrame3d =
    Frame3d Meters WorldCoordinates { defines : ShapeCoordinates }
