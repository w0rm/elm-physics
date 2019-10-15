module Physics.Body exposing
    ( Body, block, plane, sphere, particle
    , setMass, setMaterial
    , setData, getData
    , compound
    , getFrame3d, setFrame3d
    )

{-|

@docs Body, block, plane, sphere, particle
@docs setMass, setPosition, getTransformation, setMaterial


## Positioning

@docs moveBy, getPosition


## Orientation

@docs rotateBy, setOrientation, getOrientation


## User-Defined Data

@docs setData, getData


## Advanced

@docs compound

-}

import Frame3d exposing (Frame3d)
import Internal.Body as Internal exposing (Protected(..))
import Internal.Coordinates exposing (BodyLocalCoordinates, WorldCoordinates)
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Physics.Material exposing (Material)
import Physics.Shape as Shape exposing (Shape)


{-| Represents a physical body containing
user defined data, like a WebGL mesh.

By default a body is positioned in the center
of the world and has zero mass. To change this,
use [setPosition](#setPosition) or [setMass](#setMass).
Bodies with zero mass don’t move!

The supported bodies are:

  - [block](#block),
  - [plane](#plane),
  - [sphere](#sphere).

For complex bodies check [compound](#compound).

-}
type alias Body data =
    Protected data


{-| A block is defined by its dimensions along the x, y and z axes.
To create a 1x1 block, call this:

    block (Length.meters 1) (Length.meters 1) (Length.meters 1) data

-}
block : Length -> Length -> Length -> data -> Body data
block x y z =
    compound [ Shape.block x y z ]


{-| A plane with the normal that points
in the direction of the z axis.

A plane is collidable in the direction of the normal.

-}
plane : data -> Body data
plane =
    compound [ Shape.plane ]


{-| A sphere is defined by its radius.
-}
sphere : Length -> data -> Body data
sphere radius =
    compound [ Shape.sphere radius ]


{-| A particle is an abstract point that doesn’t have dimenstions.
-}
particle : data -> Body data
particle =
    compound [ Shape.particle ]


{-| Set the body mass. Bodies with zero mass don’t move!
-}
setMass : Mass -> Body data -> Body data
setMass mass (Protected body) =
    Protected (Internal.updateMassProperties { body | mass = Mass.inKilograms mass })


{-| Set the frame3d that controls the absolute position and orientation of the body in the world.
-}
setFrame3d : Frame3d Meters WorldCoordinates { defines : BodyLocalCoordinates } -> Body data -> Body data
setFrame3d frame3d (Protected body) =
    Protected (Internal.updateMassProperties { body | frame3d = frame3d })


{-| Set the [material](Physics-Material) to controll friction and bounciness.
-}
setMaterial : Material -> Body data -> Body data
setMaterial (InternalMaterial.Protected material) (Protected body) =
    Protected { body | material = material }


{-| Get the absolute position and orientation of the body in the world as a frame3d.
-}
getFrame3d : Body data -> Frame3d Meters WorldCoordinates { defines : BodyLocalCoordinates }
getFrame3d (Protected { frame3d }) =
    frame3d


{-| Set user-defined data.
-}
setData : data -> Body data -> Body data
setData data (Protected body) =
    Protected
        { id = body.id
        , data = data
        , material = body.material
        , frame3d = body.frame3d
        , velocity = body.velocity
        , angularVelocity = body.angularVelocity
        , mass = body.mass
        , shapes = body.shapes
        , force = body.force
        , torque = body.torque
        , boundingSphereRadius = body.boundingSphereRadius
        , invMass = body.invMass
        , inertia = body.inertia
        , invInertia = body.invInertia
        , invInertiaWorld = body.invInertiaWorld
        }


{-| Get user-defined data.
-}
getData : Body data -> data
getData (Protected { data }) =
    data


{-| Make a compound body from a list of [shapes](Physics-Shape#Shape).

For example, the [sphere](#sphere) from above can be defined like this:

    sphere radius data =
        compound [ Shape.sphere radius ] data

We only support [rigid bodies](https://en.wikipedia.org/wiki/Rigid_body).

-}
compound : List Shape -> data -> Body data
compound shapes data =
    let
        unprotectedShapes =
            List.map (\(InternalShape.Protected shape) -> shape) shapes
    in
    Protected (Internal.compound unprotectedShapes data)
