module Physics.Body exposing
    ( Body, box, plane, sphere
    , setMass, setPosition, getTransformation
    , moveBy, getPosition
    , rotateBy, setOrientation, getOrientation
    , setData, getData
    , compound
    )

{-|

@docs Body, box, plane, sphere
@docs setMass, setPosition, getTransformation


## Positioning

@docs moveBy, getPosition


## Orientation

@docs rotateBy, setOrientation, getOrientation


## User-Defined Data

@docs setData, getData


## Advanced

@docs compound

-}

import Internal.Body as Internal exposing (Protected(..))
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Shape as InternalShape
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Shape as Shape exposing (Shape)


{-| Represents a rigid body containing
user defined data, like a WebGL mesh.

By default a body is positioned in the center
of the world and has zero mass. To change this,
use [setPosition](#setPosition) or [setMass](#setMass).
Bodies with zero mass don’t move!

The supported bodies are:

  - [box](#box),
  - [plane](#plane),
  - [sphere](#sphere).

For complex bodies check [compound](#compound).

-}
type alias Body data =
    Protected data


{-| A box is defined by its dimensions.
To create a 1x1 box, call this:

    box { x = 1, y = 1, z = 1 } data

-}
box : Vec3 -> data -> Body data
box dimensions =
    compound [ Shape.box dimensions ]


{-| A plane with the normal that points
in the direction of the z axis.

A plane is collidable in the direction of the normal.

-}
plane : data -> Body data
plane =
    compound [ Shape.plane ]


{-| A sphere is defined by its radius.
-}
sphere : Float -> data -> Body data
sphere radius =
    compound [ Shape.sphere radius ]


{-| Set the body mass. Bodies with zero mass don’t move!
-}
setMass : Float -> Body data -> Body data
setMass mass (Protected body) =
    Protected (Internal.updateMassProperties { body | mass = mass })


{-| Set the absolute position of the body in the world.
-}
setPosition : Vec3 -> Body data -> Body data
setPosition position (Protected body) =
    Protected (Internal.updateMassProperties { body | position = position })


{-| Get the position and the orientation of the body in a single matrix.

To use this with WebGL, pass the result to [`Math.Matrix4.fromRecord`](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/Math-Matrix4#fromRecord).

-}
getTransformation : Body data -> Mat4
getTransformation (Protected { position, orientation }) =
    let
        { x, y, z, w } =
            orientation
    in
    { m11 = 1 - 2 * y * y - 2 * z * z
    , m21 = 2 * x * y + 2 * w * z
    , m31 = 2 * x * z - 2 * w * y
    , m41 = 0
    , m12 = 2 * x * y - 2 * w * z
    , m22 = 1 - 2 * x * x - 2 * z * z
    , m32 = 2 * y * z + 2 * w * x
    , m42 = 0
    , m13 = 2 * x * z + 2 * w * y
    , m23 = 2 * y * z - 2 * w * x
    , m33 = 1 - 2 * x * x - 2 * y * y
    , m43 = 0
    , m14 = position.x
    , m24 = position.y
    , m34 = position.z
    , m44 = 1
    }


{-| 4x4 matrix type
-}
type alias Mat4 =
    { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float }


{-| Move the body by a vector offset from the current position.
-}
moveBy : Vec3 -> Body data -> Body data
moveBy offset (Protected body) =
    Protected
        (Internal.updateMassProperties
            { body | position = Vec3.add offset body.position }
        )


{-| -}
getPosition : Body data -> Vec3
getPosition (Protected { position }) =
    position


{-| Rotates the body by a specific angle around the axis
from the current orientation.
-}
rotateBy : Float -> Vec3 -> Body data -> Body data
rotateBy angle axis (Protected body) =
    Protected
        (Internal.updateMassProperties
            { body
                | orientation =
                    Quaternion.mul
                        (Quaternion.fromAngleAxis angle axis)
                        body.orientation
            }
        )


{-| Sets the body orientation to a [unit quaternion](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation).
-}
setOrientation : Quaternion -> Body data -> Body data
setOrientation orientation (Protected body) =
    Protected
        (Internal.updateMassProperties
            { body | orientation = orientation }
        )


{-| Gets orientation as a quaternion.
-}
getOrientation : Body data -> Quaternion
getOrientation (Protected { orientation }) =
    orientation


{-| Sets the user-defined data.
-}
setData : data -> Body data -> Body data
setData data (Protected body) =
    Protected { body | data = data }


{-| Gets the user-defined data.
-}
getData : Body data -> data
getData (Protected { data }) =
    data


{-| Makes a compound body from a list of [shapes](Physics-Shape#Shape).

For example, the [sphere](#sphere) from above can be defined like this:

    sphere radius data =
        compound [ Shape.sphere radius ] data

-}
compound : List Shape -> data -> Body data
compound shapes data =
    let
        unprotectedShapes =
            List.map (\(InternalShape.Protected shape) -> shape) shapes
    in
    Protected (Internal.compound unprotectedShapes data)



{- Future

   type Material
       = Material

   setMaterial : Material -> Body data -> Body data
   setMaterial _ =
       identity
-}
