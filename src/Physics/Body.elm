module Physics.Body exposing
    ( Body, block, plane, sphere, particle
    , Behavior, dynamic, static, setBehavior
    , setFrame3d, getFrame3d
    , setData, getData
    , setMaterial, compound
    )

{-|

@docs Body, block, plane, sphere, particle


## Behavior

@docs Behavior, dynamic, static, setBehavior


## Positioning

@docs setFrame3d, getFrame3d


## User-Defined Data

@docs setData, getData


## Advanced

@docs setMaterial, compound, centerOfMass

-}

import Frame3d exposing (Frame3d)
import Internal.Body as Internal exposing (Protected(..))
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Physics.Coordinates exposing (BodyCoordinates, ShapeCoordinates, WorldCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape as Shape exposing (Shape)
import Point3d exposing (Point3d)


{-| Represents a physical body containing
user defined data, like a WebGL mesh.

By default a body is positioned in the center
of the world and doesn’t move. To change this,
use [setFrame3d](#setFrame3d) or [setBehavior](#setBehavior).

The supported bodies are:

  - [block](#block),
  - [plane](#plane),
  - [sphere](#sphere),
  - [particle](#particle).

For complex bodies check [compound](#compound).

-}
type alias Body data =
    Protected data


{-| A block is defined by its dimensions along the x, y and z axes.
To create a 1x1x1 cube, call this:

    cubeBody =
        block
            (Length.meters 1)
            (Length.meters 1)
            (Length.meters 1)
            data

-}
block : Length -> Length -> Length -> data -> Body data
block x y z =
    compound [ Shape.block x y z ]


{-| A plane with the normal that points
in the direction of the z axis.

A plane is collidable in the direction of the normal.
Planes don’t collide with other planes and are always static.

-}
plane : data -> Body data
plane =
    compound
        [ InternalShape.Protected
            { frame3d = defaultShapeFrame
            , kind = InternalShape.Plane
            , volume = 0
            }
        ]


defaultShapeFrame : Frame3d Meters BodyCoordinates { defines : ShapeCoordinates }
defaultShapeFrame =
    Frame3d.atPoint Point3d.origin


{-| A sphere is defined by its radius.
-}
sphere : Length -> data -> Body data
sphere radius =
    compound [ Shape.sphere radius ]


{-| A particle is an abstract point that doesn’t have dimensions.
Particles don’t collide with each other.
-}
particle : data -> Body data
particle =
    compound
        [ InternalShape.Protected
            { frame3d = defaultShapeFrame
            , kind = InternalShape.Particle
            , volume = 0
            }
        ]


{-| Bodies may have static or dynamic behavior.
-}
type Behavior
    = Dynamic Float
    | Static


{-| Dynamic bodies move and react to forces and collide with other dynamic and static bodies.
-}
dynamic : Mass -> Behavior
dynamic kilos =
    let
        mass =
            Mass.inKilograms kilos
    in
    if isNaN mass || isInfinite mass || mass <= 0 then
        Static

    else
        Dynamic mass


{-| Static bodies don’t move and only collide with dynamic bodies.
-}
static : Behavior
static =
    Static


{-| Change the behavior, e.g. to make a body dynamic:

    dynamicBody =
        staticBody
            |> setBehavior (dynamic (Mass.kilograms 5))

-}
setBehavior : Behavior -> Body data -> Body data
setBehavior behavior (Protected body) =
    case behavior of
        Dynamic mass ->
            case body.shapes of
                [] ->
                    Protected body

                [ { kind } ] ->
                    if kind == InternalShape.Plane then
                        Protected body

                    else
                        Protected (Internal.updateMassProperties { body | mass = mass })

                _ ->
                    Protected (Internal.updateMassProperties { body | mass = mass })

        Static ->
            Protected (Internal.updateMassProperties { body | mass = 0 })


{-| Set the position and orientation of the body in the world,
e.g. to position a body 5 meters below the origin:

    movedBody =
        body
            |> setFrame3d
                (Frame3d.atPoint (Point3d.meters 0 0 -5))

For all possible ways to construct and modify `Frame3d`, check
[elm-geometry package docs](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/2.0.0/Frame3d).

-}
setFrame3d : Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body data -> Body data
setFrame3d userFrame3d (Protected body) =
    let
        newFrame3d =
            Frame3d.placeIn userFrame3d body.centerOfMassFrame3d
    in
    Protected (Internal.updateMassProperties { body | frame3d = newFrame3d })


{-| Get the position and orientation of the body in the world as `Frame3d`.
-}
getFrame3d : Body data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
getFrame3d (Protected { frame3d, centerOfMassFrame3d }) =
    Frame3d.placeIn frame3d (Frame3d.relativeTo centerOfMassFrame3d (Frame3d.atPoint Point3d.origin))


{-| Set user-defined data.
-}
setData : data -> Body data -> Body data
setData data (Protected body) =
    Protected
        { id = body.id
        , data = data
        , material = body.material
        , frame3d = body.frame3d
        , centerOfMassFrame3d = body.centerOfMassFrame3d
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


{-| Set the [material](Physics-Material) to controll friction and bounciness.
-}
setMaterial : Material -> Body data -> Body data
setMaterial (InternalMaterial.Protected material) (Protected body) =
    Protected { body | material = material }


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
