module Physics.Shape exposing
    ( Shape, block, plane, sphere, particle
    , setFrame3d
    )

{-|

@docs Shape, block, plane, sphere, particle


## Positioning and Orientation

Shapes are positioned in the local body coordinate system,
they can be moved and rotated just like bodies in the world using frame3d.

@docs setFrame3d

-}

import Frame3d exposing (Frame3d)
import Internal.Convex as Convex
import Internal.Coordinates exposing (BodyLocalCoordinates, ShapeLocalCoordinates)
import Internal.Shape as Internal exposing (Protected(..))
import Length exposing (Length, Meters)
import Point3d


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The supported shapes are:

  - [block](#block),
  - [plane](#plane),
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
        }


{-| A plane with the normal that points in the
direction of the z axis.
-}
plane : Shape
plane =
    Protected
        { frame3d = defaultFrame
        , kind = Internal.Plane
        }


{-| A sphere is defined by a radius and a position.
-}
sphere : Length -> Shape
sphere radius =
    Protected
        { frame3d = defaultFrame
        , kind = Internal.Sphere (Length.inMeters radius)
        }


{-| -}
particle : Shape
particle =
    Protected
        { frame3d = defaultFrame
        , kind = Internal.Particle
        }


defaultFrame : Frame3d Meters BodyLocalCoordinates { defines : ShapeLocalCoordinates }
defaultFrame =
    Frame3d.atPoint Point3d.origin


{-| Move the shape in a body by a vector offset from
the current local position.
-}
setFrame3d :
    Frame3d Meters BodyLocalCoordinates { defines : ShapeLocalCoordinates }
    -> Shape
    -> Shape -- Vector3d Meters BodyCoordinates
setFrame3d frame3d (Protected shape) =
    Protected
        { shape | frame3d = frame3d }
