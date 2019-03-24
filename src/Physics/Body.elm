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

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4)
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
type Body data
    = Body data


{-| A box is defined by its dimensions.
To create a 1x1 box, call this:

    box { x = 1, y = 1, z = 1 } data

-}
box : Vec3 -> data -> Body data
box dimensions =
    compound
        [ Shape.box
            { dimensions = dimensions
            , position = { x = 0, y = 0, z = 0 }
            , orientation = { x = 0, y = 0, z = 0, w = 1 }
            }
        ]


{-| A plane with the normal that points
in the direction of the z axis.

A plane is collidable in the direction of the normal.

TODO: no mass for plane?
TODO: no plane in compound shapes?

-}
plane : data -> Body data
plane =
    compound
        [ Shape.plane
            { normal = { x = 0, y = 0, z = 1 }
            , position = { x = 0, y = 0, z = 0 }
            }
        ]


{-| A sphere is defined by its radius.
-}
sphere : Float -> data -> Body data
sphere radius =
    compound
        [ Shape.sphere
            { radius = radius
            , position = { x = 0, y = 0, z = 0 }
            }
        ]


{-| Set the body mass. Bodies with zero mass don’t move!
-}
setMass : Float -> Body data -> Body data
setMass _ =
    identity


{-| Set the absolute position of the body in the world.
-}
setPosition : Vec3 -> Body data -> Body data
setPosition _ =
    identity


{-| Get the position and the orientation of the body in a single matrix.

To use this with WebGL, pass the result to [`Math.Matrix4.fromRecord`](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/Math-Matrix4#fromRecord).

-}
getTransformation : Body data -> Mat4
getTransformation _ =
    Mat4.identity


{-| Move the body by a vector offset from the current position.
-}
moveBy : Vec3 -> Body data -> Body data
moveBy _ =
    identity


{-| -}
getPosition : Body data -> Vec3
getPosition _ =
    { x = 0, y = 0, z = 0 }


{-| Rotates the body by a specific angle around the axis
from the current orientation.
-}
rotateBy : Float -> Vec3 -> Body data -> Body data
rotateBy _ _ =
    identity


{-| Sets the body orientation to a [unit quaternion](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation).
-}
setOrientation : Vec4 -> Body data -> Body data
setOrientation _ =
    identity


{-| Gets orientation as a quaternion.
-}
getOrientation : Body data -> Vec4
getOrientation _ =
    { x = 0, y = 0, z = 0, w = 1 }


{-| Sets the user-defined data.
-}
setData : data -> Body data -> Body data
setData data _ =
    Body data


{-| Gets the user-defined data.
-}
getData : Body data -> data
getData (Body data) =
    data


{-| Makes a compound body from a list of [shapes](Physics-Shape#Shape).

For example, the [sphere](#sphere) from above can be defined like this:

    sphere radius data =
        compound
            [ Shape.sphere
                { radius = radius
                , position = { x = 0, y = 0, z = 0 }
                }
            ]
            data

-}
compound : List Shape -> data -> Body data
compound _ =
    Body



-- Future


type Material
    = Material


setMaterial : Material -> Body data -> Body data
setMaterial _ =
    identity
