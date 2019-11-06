module Internal.Coordinates exposing (CenterOfMassCoordinates, ShapeCoordinates, ShapeWorldTransform3d)

import Internal.Transform3d exposing (Transform3d)
import Physics.Coordinates exposing (WorldCoordinates)


type alias ShapeWorldTransform3d =
    Transform3d WorldCoordinates { defines : ShapeCoordinates }


type CenterOfMassCoordinates
    = CenterOfMassCoordinates


type ShapeCoordinates
    = ShapeCoordinates
