module Physics.Shape exposing
    ( Shape, box, plane, sphere, particle
    , moveBy, rotateBy, setPosition, setOrientation
    )

{-|

@docs Shape, box, plane, sphere, particle


## Positioning and Orientation

Shapes are positioned in the local body coordinate system,
they can be moved and rotated just like bodies in the world.

@docs moveBy, rotateBy, setPosition, setOrientation

-}

import Internal.Convex as Convex
import Internal.Quaternion as Quaternion
import Internal.Shape as Internal exposing (Protected(..))
import Internal.Vector3 as Vec3


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The supported shapes are:

  - [box](#box),
  - [plane](#plane),
  - [sphere](#sphere).

-}
type alias Shape =
    Protected


{-| A box is defined by dimensions along the corresponding axes.
-}
box : { x : Float, y : Float, z : Float } -> Shape
box dimensions =
    Protected
        { position = Vec3.zero
        , orientation = Quaternion.identity
        , kind = Internal.Convex (Convex.fromBox (Vec3.scale 0.5 dimensions))
        }


{-| A plane with the normal that points in the
direction of the z axis.
-}
plane : Shape
plane =
    Protected
        { position = Vec3.zero
        , orientation = Quaternion.identity
        , kind = Internal.Plane
        }


{-| A sphere is defined by a radius and a position.
-}
sphere : Float -> Shape
sphere radius =
    Protected
        { position = Vec3.zero
        , orientation = Quaternion.identity
        , kind = Internal.Sphere radius
        }


{-| -}
particle : Shape
particle =
    Protected
        { position = Vec3.zero
        , orientation = Quaternion.identity
        , kind = Internal.Particle
        }


{-| Move the shape in a body by a vector offset from
the current local position.
-}
moveBy : { x : Float, y : Float, z : Float } -> Shape -> Shape
moveBy offset (Protected shape) =
    Protected
        { shape | position = Vec3.add offset shape.position }


{-| Rotate the shape in a body by a specific angle
around the axis from the current local orientation.
-}
rotateBy : Float -> { x : Float, y : Float, z : Float } -> Shape -> Shape
rotateBy angle axis (Protected shape) =
    Protected
        { shape
            | orientation =
                Quaternion.mul
                    (Quaternion.fromAngleAxis angle axis)
                    shape.orientation
        }


{-| Set the local position of the shape in a body.
-}
setPosition : { x : Float, y : Float, z : Float } -> Shape -> Shape
setPosition position (Protected shape) =
    Protected { shape | position = position }


{-| Set the local shape orientation to a [unit quaternion](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation).
-}
setOrientation : { x : Float, y : Float, z : Float, w : Float } -> Shape -> Shape
setOrientation orientation (Protected shape) =
    Protected { shape | orientation = orientation }
