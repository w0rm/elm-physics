module Physics.Shape exposing (Shape, box, plane, sphere)

{-|

@docs Shape, box, plane, sphere

-}

import AltMath.Vector3 as Vec3 exposing (Vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4)
import Array exposing (Array)


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The supported shapes are:

  - [box](#box),
  - [plane](#plane),
  - [sphere](#sphere).

Shapes are positioned in the local body coordinate system.

-}
type Shape
    = Shape


{-| A box is defined by dimensions, a position and an orientation.
-}
box : { dimensions : Vec3, position : Vec3, orientation : Vec4 } -> Shape
box { dimensions, position, orientation } =
    Shape


{-| A plane is defined by a normal and a position.
-}
plane : { normal : Vec3, position : Vec3 } -> Shape
plane { normal, position } =
    Shape


{-| A sphere is defined by a radius and a position.
-}
sphere : { radius : Float, position : Vec3 } -> Shape
sphere { radius, position } =
    Shape



-- Future


{-| Make a convex polyhedron out of an array of points
and a list of faces, where each face lists indices from
this array.
-}
convexPolyhedron : Array Vec3 -> List (List Int) -> Shape
convexPolyhedron _ _ =
    -- TODO: return Maybe Shape or rename to unsafe?
    Shape
