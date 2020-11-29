module Physics.Body exposing
    ( Body, block, plane, sphere, cylinder, particle
    , Behavior, dynamic, static, withBehavior
    , frame, originPoint, velocity, angularVelocity, velocityAt, centerOfMass, mass
    , moveTo, translateBy, rotateAround
    , data, withData
    , applyForce, applyImpulse
    , withMaterial, compound, withDamping, transformWithInverseInertia
    )

{-|

@docs Body, block, plane, sphere, cylinder, particle


## Behavior

@docs Behavior, dynamic, static, withBehavior


## Properties

@docs frame, originPoint, velocity, angularVelocity, velocityAt, centerOfMass, mass


## Position and orientation

@docs moveTo, translateBy, rotateAround


## User-Defined Data

@docs data, withData


## Interaction

@docs applyForce, applyImpulse


## Advanced

@docs withMaterial, compound, withDamping, transformWithInverseInertia

-}

import Angle exposing (Angle)
import AngularSpeed exposing (RadiansPerSecond)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Seconds)
import Force exposing (Newtons)
import Frame3d exposing (Frame3d)
import Internal.Body as Internal exposing (Protected(..))
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Length exposing (Meters)
import Mass exposing (Kilograms, Mass)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape as Shape exposing (Shape)
import Point3d exposing (Point3d)
import Quantity exposing (Product, Quantity(..), Rate, Squared)
import Speed exposing (MetersPerSecond)
import Sphere3d exposing (Sphere3d)
import Vector3d exposing (Vector3d)


{-| Represents a physical body containing
user defined data, like a WebGL mesh.

By default bodies don’t move. To change this,
use [withBehavior](#withBehavior).

All bodies start out centered on the origin,
use [moveTo](#moveTo) to set the position.

The supported bodies are:

  - [block](#block),
  - [plane](#plane),
  - [sphere](#sphere),
  - [cylinder](#cylinder),
  - [particle](#particle).

For complex bodies check [compound](#compound).

-}
type alias Body data =
    Protected data


{-| A block is created from elm-geometry [Block3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Block3d).
To create a 1x1x1 cube, centered at the origin of
the body, call this:

    cubeBody =
        block
            (Block3d.centeredOn
                Frame3d.atOrigin
                ( meters 1, meters 1, meters 1 )
            )
            data

-}
block : Block3d Meters BodyCoordinates -> data -> Body data
block block3d =
    compound [ Shape.block block3d ]


{-| A cylinder is created from elm-geometry [Cylinder3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Cylinder3d).
To create a vertical cylinder, centered at the origin of
the body, call this:

    cylinderBody =
        cylinder
            (Cylinder3d.centeredOn
                Point3d.origin
                Direction3d.z
                { radius = Length.meter, length = Length.meter }
            )
            data

This cylinder is approximated with a convex polyhedron mesh that has 12 side faces.

For more subdivisions at the cost of worse performance,
create a [compound body](#compound) with [Shape.cylinder](Physics-Shape#cylinder).

-}
cylinder : Cylinder3d Meters BodyCoordinates -> data -> Body data
cylinder cylinder3d =
    compound [ Shape.cylinder 12 cylinder3d ]


{-| A plane with the normal that points
in the direction of the z axis.

A plane is collidable in the direction of the normal.
Planes don’t collide with other planes and are always static.

-}
plane : data -> Body data
plane =
    compound
        [ InternalShape.Protected
            (InternalShape.Plane
                { position = Vec3.zero
                , normal = Vec3.zAxis
                }
            )
        ]


{-| A sphere is created from elm-geometry [Sphere3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Sphere3d).

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
            (InternalShape.Particle Vec3.zero)
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
        mass_ =
            Mass.inKilograms kilos
    in
    if isNaN mass_ || isInfinite mass_ || mass_ <= 0 then
        Static

    else
        Dynamic mass_


{-| Static bodies don’t move and only collide with dynamic bodies.
-}
static : Behavior
static =
    Static


{-| Change the behavior, e.g. to make a body dynamic:

    dynamicBody =
        staticBody
            |> withBehavior (dynamic (Mass.kilograms 5))

-}
withBehavior : Behavior -> Body data -> Body data
withBehavior behavior (Protected body) =
    case behavior of
        Dynamic mass_ ->
            case body.shapes of
                [] ->
                    Protected body

                [ shape ] ->
                    case shape of
                        InternalShape.Plane _ ->
                            Protected body

                        _ ->
                            Protected (Internal.updateMassProperties { body | mass = mass_ })

                _ ->
                    Protected (Internal.updateMassProperties { body | mass = mass_ })

        Static ->
            Protected (Internal.updateMassProperties { body | mass = 0 })


{-| Get the position and orientation of the body in the world
as [Frame3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Frame3d).

This is useful to transform points and directions between
world and body coordinates.

-}
frame : Body data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
frame (Protected { transform3d, centerOfMassTransform3d }) =
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


{-| Get the linear velocity of a body
-}
velocity : Body data -> Vector3d MetersPerSecond WorldCoordinates
velocity (Protected body) =
    Vector3d.unsafe body.velocity


{-| Get the angular velocity of a body
-}
angularVelocity : Body data -> Vector3d RadiansPerSecond WorldCoordinates
angularVelocity (Protected body) =
    Vector3d.unsafe body.angularVelocity


{-| Get the linear velocity of a point on a body.

This takes into account both linear and angular velocities of the body.

-}
velocityAt : Point3d Meters WorldCoordinates -> Body data -> Vector3d MetersPerSecond WorldCoordinates
velocityAt point (Protected body) =
    let
        origin =
            Transform3d.originPoint body.transform3d

        { x, y, z } =
            Point3d.toMeters point

        originToPoint =
            { x = x - origin.x
            , y = y - origin.y
            , z = z - origin.z
            }

        cross =
            Vec3.cross body.angularVelocity originToPoint
    in
    Vector3d.unsafe (Vec3.add cross body.velocity)


{-| Get the center of mass of a body in the body coordinate system.
-}
centerOfMass : Body data -> Point3d Meters BodyCoordinates
centerOfMass (Protected { centerOfMassTransform3d }) =
    Point3d.fromMeters (Transform3d.originPoint centerOfMassTransform3d)


{-| Get a mass of a body. Returns Nothing if a body is not dynamic.
-}
mass : Body data -> Maybe Mass
mass (Protected body) =
    if body.mass <= 0 then
        Nothing

    else
        Just (Mass.kilograms body.mass)


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
    Protected
        { body
            | transform3d = newTransform3d
            , worldShapes = InternalShape.shapesPlaceIn newTransform3d body.shapes
        }


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
    Protected
        { body
            | transform3d = newTransform3d
            , worldShapes = InternalShape.shapesPlaceIn newTransform3d body.shapes
        }


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
    Protected
        { body
            | transform3d = newTransform3d
            , worldShapes = InternalShape.shapesPlaceIn newTransform3d body.shapes
            , invInertiaWorld = Transform3d.invertedInertiaRotateIn newTransform3d body.invInertia
        }


{-| Update user-defined data.
-}
withData : data -> Body data -> Body data
withData newData (Protected body) =
    Protected { body | data = newData }


{-| Get user-defined data.
-}
data : Body data -> data
data (Protected body) =
    body.data


{-| Apply a force in a direction at a point on a body.
The force will be applied during one simulation step.

Keep applying the force every simulation step to accelerate.

    pushedBox =
        box
            |> applyForce
                (Force.newtons 50)
                Direction3d.positiveY
                pointOnBox

-}
applyForce : Quantity Float Newtons -> Direction3d WorldCoordinates -> Point3d Meters WorldCoordinates -> Body data -> Body data
applyForce (Quantity force) direction point (Protected body) =
    if body.mass > 0 then
        Protected
            (Internal.applyForce
                force
                (Direction3d.unwrap direction)
                (Point3d.toMeters point)
                body
            )

    else
        Protected body


{-| Apply an impulse in a direction at a point on a body.
Applying an impulse is the same as applying a force during
the interval of time. The changes are applied to velocity
and angular velocity of the body.

For example, to hit a billiard ball with a force of 50 newtons,
with the duration of the hit 0.005 seconds:

    impulse =
        Force.newtons 50
            |> Quantity.times (Duration.seconds 0.005)

    hitCueBall =
        cueBall
            |> applyImpulse
                impulse
                Direction3d.positiveY
                hitPoint

-}
applyImpulse : Quantity Float (Product Newtons Seconds) -> Direction3d WorldCoordinates -> Point3d Meters WorldCoordinates -> Body data -> Body data
applyImpulse (Quantity impulse) direction point (Protected body) =
    if body.mass > 0 then
        Protected
            (Internal.applyImpulse
                impulse
                (Direction3d.unwrap direction)
                (Point3d.toMeters point)
                body
            )

    else
        Protected body


{-| Set the [material](Physics-Material) to control friction and bounciness.
-}
withMaterial : Material -> Body data -> Body data
withMaterial (InternalMaterial.Protected material) (Protected body) =
    Protected { body | material = material }


{-| Make a compound body from a list of [shapes](Physics-Shape#Shape).

For example, the [sphere](#sphere) from above can be defined like this:

    sphere radius data =
        compound
            [ Shape.sphere
                (Sphere3d.atOrigin (meters 1))
            ]
            data

We only support [rigid bodies](https://en.wikipedia.org/wiki/Rigid_body).

Under the hood, we find the center of mass and the [inertia tensor](https://en.wikipedia.org/wiki/Moment_of_inertia#Inertia_tensor).
It is assumed, that the shapes have the same density and do not overlap.

-}
compound : List Shape -> data -> Body data
compound shapes newData =
    let
        unprotectedShapes =
            List.foldl
                (\(InternalShape.Protected shape) result -> shape :: result)
                []
                shapes
    in
    Protected (Internal.compound unprotectedShapes newData)


{-| Set linear and angular damping, in order to decrease velocity over time.

These parameters specify the proportion of velocity lost per second.

This may be useful to e.g. simulate the friction of a sphere rolling on
the flat surface. The normal friction between these surfaces doesn’t work,
because there is just 1 contact point.

Inputs are clamped between 0 and 1, the defaults are 0.01.

-}
withDamping : { linear : Float, angular : Float } -> Body data -> Body data
withDamping { linear, angular } (Protected body) =
    Protected
        { body
            | linearDamping = clamp 0 1 linear
            , angularDamping = clamp 0 1 angular
        }


{-| Transform a vector using the inverse moment of inertia of a body.

This lets you compute angular acceleration from torque,
or angular velocity contribution from impulse.

You’ll most likely need to use [Vector3d.unwrap](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Vector3d#unwrap) on the result.

-}
transformWithInverseInertia : Body data -> Vector3d units WorldCoordinates -> Vector3d (Rate units (Product Kilograms (Squared Meters))) WorldCoordinates
transformWithInverseInertia (Protected { invInertiaWorld }) vector =
    let
        { x, y, z } =
            Vector3d.unwrap vector
    in
    Vector3d.unsafe
        { x = invInertiaWorld.m11 * x + invInertiaWorld.m12 * y + invInertiaWorld.m13 * z
        , y = invInertiaWorld.m21 * x + invInertiaWorld.m22 * y + invInertiaWorld.m23 * z
        , z = invInertiaWorld.m31 * x + invInertiaWorld.m32 * y + invInertiaWorld.m33 * z
        }
