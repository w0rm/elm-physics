module Physics.Shape exposing
    ( Shape, box, plane, sphere
    , moveBy, rotateBy, setPosition, setOrientation
    )

{-|

@docs Shape, box, plane, sphere

Shapes are positioned in the local body coordinate system,
they can be moved and rotated just like bodies in the world.

@docs moveBy, rotateBy, setPosition, setOrientation

-}

import AltMath.Vector3 as Vec3 exposing (Vec3)
import Array exposing (Array)
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Shape as Internal exposing (Protected(..))


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The supported shapes are:

  - [box](#box),
  - [plane](#plane),
  - [sphere](#sphere).

-}
type alias Shape =
    Internal.Protected


{-| A box is defined by dimensions.
-}
box : Vec3 -> Shape
box dimensions =
    Protected
        { position = Const.zero3
        , orientation = Quaternion.identity
        , kind = Internal.Convex (ConvexPolyhedron.fromBox (Vec3.scale 0.5 dimensions))
        }


{-| A plane with the normal that points in the
direction of the z axis.
-}
plane : Shape
plane =
    Protected
        { position = Const.zero3
        , orientation = Quaternion.identity
        , kind = Internal.Plane
        }


{-| A sphere is defined by a radius and a position.
-}
sphere : Float -> Shape
sphere radius =
    Protected
        { position = Const.zero3
        , orientation = Quaternion.identity
        , kind = Internal.Sphere radius
        }


{-| Move the shape in a body by a vector offset from
the current local position.
-}
moveBy : Vec3 -> Shape -> Shape
moveBy offset (Protected shape) =
    Protected
        { shape | position = Vec3.add offset shape.position }


{-| Rotates the shape in a body by a specific angle
around the axis from the current local orientation.
-}
rotateBy : Float -> Vec3 -> Shape -> Shape
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
setPosition : Vec3 -> Shape -> Shape
setPosition position (Protected shape) =
    Protected { shape | position = position }


{-| Sets the local shape orientation to a [unit quaternion](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation).
-}
setOrientation : Quaternion -> Shape -> Shape
setOrientation orientation (Protected shape) =
    Protected { shape | orientation = orientation }



{- Future

   {-| Make a convex polyhedron out of an array of points
   and a list of faces, where each face lists indices from
   this array.
   -}
   convexPolyhedron : Array Vec3 -> List (List Int) -> Shape
   convexPolyhedron _ _ =
       Shape
-}
