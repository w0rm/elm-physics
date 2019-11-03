module Physics.Shape exposing
    ( Shape, block, sphere
    , setFrame3d
    )

{-|

@docs Shape, block, sphere


## Positioning and Orientation

Shapes are positioned in the local body coordinate system,
they can be moved and rotated just like bodies in the world using frame3d.

@docs setFrame3d

-}

import Frame3d exposing (Frame3d)
import Internal.Convex as Convex
import Internal.Shape as Internal exposing (Protected(..))
import Length exposing (Length, Meters)
import Physics.Coordinates exposing (BodyCoordinates, ShapeCoordinates)
import Point3d


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The only supported shapes are:

  - [block](#block),
  - [sphere](#sphere).

-}
type alias Shape =
    Protected


{-| A block is defined by dimensions along the x, y and z axes.
-}
block : Length -> Length -> Length -> Shape
block x y z =
    let
        halfX =
            Length.inMeters x * 0.5

        halfY =
            Length.inMeters y * 0.5

        halfZ =
            Length.inMeters z * 0.5
    in
    Protected
        { frame3d = defaultFrame
        , kind = Internal.Convex (Convex.fromBlock halfX halfY halfZ)
        , volume = Length.inMeters x * Length.inMeters y * Length.inMeters z
        }


{-| A sphere is defined by a radius and a position.
-}
sphere : Length -> Shape
sphere radius =
    Protected
        { frame3d = defaultFrame
        , kind = Internal.Sphere (Length.inMeters radius)
        , volume = 4 / 3 * pi * (Length.inMeters radius ^ 3)
        }


defaultFrame : Frame3d Meters BodyCoordinates { defines : ShapeCoordinates }
defaultFrame =
    Frame3d.atPoint Point3d.origin


{-| Change the position and orientation of the shape in the body.
-}
setFrame3d :
    Frame3d Meters BodyCoordinates { defines : ShapeCoordinates }
    -> Shape
    -> Shape
setFrame3d frame3d (Protected shape) =
    Protected
        { shape | frame3d = frame3d }
