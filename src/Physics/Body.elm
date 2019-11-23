module Physics.Body exposing
    ( Body, block, plane, sphere, particle
    , Behavior, dynamic, static, setBehavior
    , getFrame3d, moveTo, translateBy, rotateAround, originPoint
    , setData, getData
    , setMaterial, compound
    )

{-|

@docs Body, block, plane, sphere, particle


## Behavior

@docs Behavior, dynamic, static, setBehavior


## Position and Orientation

@docs getFrame3d, moveTo, translateBy, rotateAround, originPoint


## User-Defined Data

@docs setData, getData


## Advanced

@docs setMaterial, compound

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Direction3d
import Frame3d exposing (Frame3d)
import Internal.Body as Internal exposing (Protected(..))
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Mass exposing (Mass)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape as Shape exposing (Shape)
import Point3d exposing (Point3d)
import Sphere3d exposing (Sphere3d)
import Vector3d exposing (Vector3d)


{-| Represents a physical body containing
user defined data, like a WebGL mesh.

By default bodies don’t move. To change this,
use [setBehavior](#setBehavior).

All bodies start out centered on the origin,
use [moveTo](#moveTo) to set the position.

The supported bodies are:

  - [block](#block),
  - [plane](#plane),
  - [sphere](#sphere),
  - [particle](#particle).

For complex bodies check [compound](#compound).

-}
type alias Body data =
    Protected data


{-| A block is created from elm-geometry Block3d.
To create a 1x1x1 cube, centered at the origin of
the body, call this:

    cubeBody =
        block
            (Block3d.centeredOn
                Frame3d.origin
                ( meters 1, meters 1, meters 1 )
            )
            data

-}
block : Block3d Meters BodyCoordinates -> data -> Body data
block block3d =
    compound [ Shape.block block3d ]


{-| A plane with the normal that points
in the direction of the z axis.

A plane is collidable in the direction of the normal.
Planes don’t collide with other planes and are always static.

-}
plane : data -> Body data
plane =
    compound
        [ InternalShape.Protected
            { transform3d = Transform3d.atOrigin
            , kind = InternalShape.Plane
            , volume = 0
            }
        ]


{-| A sphere is created from elm-geometry Sphere3d.

To create a 1 meter radius sphere, that is centered
at the origin of the body, call this:

    sphereBody =
        sphere
            (Sphere3d.atOrigin (meters 1))
            data

-}
sphere : Sphere3d Meters BodyCoordinates -> data -> Body data
sphere sphere3d =
    compound [ Shape.sphere sphere3d ]


{-| A particle is an abstract point that doesn’t have dimensions.
Particles don’t collide with each other.
-}
particle : data -> Body data
particle =
    compound
        [ InternalShape.Protected
            { transform3d = Transform3d.atOrigin
            , kind = InternalShape.Particle
            , volume = 0
            }
        ]


{-| Bodies may have static or dynamic behavior.
-}
type Behavior
    = Dynamic Float
    | Static


{-| Dynamic bodies move and react to forces and collide with
other dynamic and static bodies.
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


{-| Set the position of the body in the world,
e.g. to raise a body 5 meters above the origin:

    movedBody =
        body
            |> moveTo (Point3d.meters 0 0 5)

-}
moveTo : Point3d Meters WorldCoordinates -> Body data -> Body data
moveTo point3d (Protected body) =
    let
        bodyCoordinatesTransform3d =
            Transform3d.placeIn
                body.transform3d
                (Transform3d.inverse body.centerOfMassTransform3d)

        newTransform3d =
            Transform3d.placeIn
                (Transform3d.moveTo (Point3d.toMeters point3d) bodyCoordinatesTransform3d)
                body.centerOfMassTransform3d
    in
    Protected (Internal.updateMassProperties { body | transform3d = newTransform3d })


{-| Rotate the body in the world around axis,
e.g. to rotate a body 45 degrees around Z axis:

    movedBody =
        body
            |> rotateAround Axis3d.z (Angle.degrees 45)

-}
rotateAround : Axis3d Meters WorldCoordinates -> Angle -> Body data -> Body data
rotateAround axis angle (Protected body) =
    let
        bodyCoordinatesTransform3d =
            Transform3d.placeIn
                body.transform3d
                (Transform3d.inverse body.centerOfMassTransform3d)

        rotatedOrigin =
            Point3d.rotateAround
                axis
                angle
                (Point3d.fromMeters
                    (Transform3d.originPoint bodyCoordinatesTransform3d)
                )

        newBodyCoordinatesTransform3d =
            bodyCoordinatesTransform3d
                |> Transform3d.moveTo
                    (Point3d.toMeters rotatedOrigin)
                |> Transform3d.rotateAroundOwn
                    (Direction3d.unwrap (Axis3d.direction axis))
                    (Angle.inRadians angle)

        newTransform3d =
            Transform3d.placeIn
                newBodyCoordinatesTransform3d
                body.centerOfMassTransform3d
    in
    Protected (Internal.updateMassProperties { body | transform3d = newTransform3d })


{-| Get the origin point of a body in the world
-}
originPoint : Body data -> Point3d Meters WorldCoordinates
originPoint (Protected { transform3d, centerOfMassTransform3d }) =
    let
        bodyCoordinatesTransform3d =
            Transform3d.placeIn
                transform3d
                (Transform3d.inverse centerOfMassTransform3d)
    in
    Point3d.fromMeters
        (Transform3d.originPoint bodyCoordinatesTransform3d)


{-| Move the body in the world relative to its current position,
e.g. to translate a body down by 5 meters:

    translatedBody =
        body
            |> translateBy (Vector3d.meters 0 0 -5)

-}
translateBy : Vector3d Meters WorldCoordinates -> Body data -> Body data
translateBy vector3d (Protected body) =
    let
        bodyCoordinatesTransform3d =
            Transform3d.placeIn
                body.transform3d
                (Transform3d.inverse body.centerOfMassTransform3d)

        newTransform3d =
            Transform3d.placeIn
                (Transform3d.translateBy
                    (Vector3d.toMeters vector3d)
                    bodyCoordinatesTransform3d
                )
                body.centerOfMassTransform3d
    in
    Protected (Internal.updateMassProperties { body | transform3d = newTransform3d })


{-| Get the position and orientation of the body in the world
as [Frame3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/2.0.0/Frame3d).

This is useful to render a body with elm-3d-scene, but also to
transform points and directions between world and body coordinates.

-}
getFrame3d : Body data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
getFrame3d (Protected { transform3d, centerOfMassTransform3d }) =
    let
        bodyCoordinatesTransform3d =
            Transform3d.placeIn transform3d (Transform3d.inverse centerOfMassTransform3d)

        { m11, m21, m31, m12, m22, m32, m13, m23, m33 } =
            Transform3d.orientation bodyCoordinatesTransform3d
    in
    Frame3d.unsafe
        { originPoint = Point3d.fromMeters (Transform3d.originPoint bodyCoordinatesTransform3d)
        , xDirection = Direction3d.unsafe { x = m11, y = m21, z = m31 }
        , yDirection = Direction3d.unsafe { x = m12, y = m22, z = m32 }
        , zDirection = Direction3d.unsafe { x = m13, y = m23, z = m33 }
        }


{-| Set user-defined data.
-}
setData : data -> Body data -> Body data
setData data (Protected body) =
    Protected
        { id = body.id
        , data = data
        , material = body.material
        , transform3d = body.transform3d
        , centerOfMassTransform3d = body.centerOfMassTransform3d
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
