module Physics exposing
    ( WorldCoordinates, BodyCoordinates
    , Body
    , block, plane, sphere, cylinder, pointMass
    , moveTo, translateBy, rotateAround, placeIn
    , simulate, onEarth, Config
    , Contacts, emptyContacts, contactPoints
    , frame, originPoint, velocity, angularVelocity, velocityAt
    , centerOfMass, mass
    , raycast, applyForce, applyImpulse
    , dynamic, static
    , damp, scaleTo, applyInverseInertia, angularAccelerationFromTorque, angularVelocityDeltaFromAngularImpulse
    )

{-|


# Coordinates

@docs WorldCoordinates, BodyCoordinates


# Bodies

@docs Body

@docs block, plane, sphere, cylinder, pointMass


# Positioning

@docs moveTo, translateBy, rotateAround, placeIn


# Simulation

@docs simulate, onEarth, Config

@docs Contacts, emptyContacts, contactPoints


# Properties

@docs frame, originPoint, velocity, angularVelocity, velocityAt

@docs centerOfMass, mass


# Interaction

@docs raycast, applyForce, applyImpulse


# Composite bodies

@docs dynamic, static


# Advanced

@docs damp, scaleTo, applyInverseInertia, angularAccelerationFromTorque, angularVelocityDeltaFromAngularImpulse

-}

import Acceleration
import Angle exposing (Angle)
import AngularAcceleration exposing (RadiansPerSecondSquared)
import AngularSpeed exposing (RadiansPerSecond)
import Area exposing (SquareMeters)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Dict
import Direction3d exposing (Direction3d)
import Duration exposing (Duration, Seconds)
import Force exposing (Newtons)
import Frame3d exposing (Frame3d)
import Internal.AssignIds
import Internal.Body as InternalBody
import Internal.BroadPhase as BroadPhase
import Internal.Constraint as InternalConstraint
import Internal.Contact as InternalContact
import Internal.Coordinates
import Internal.Shape as InternalShape
import Internal.Solver as Solver
import Internal.SolverBody as SolverBody
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Length exposing (Meters)
import Mass exposing (Kilograms, Mass)
import Physics.Constraint exposing (Constraint)
import Physics.Material exposing (Dense, Material)
import Physics.Shape as Shape exposing (Shape)
import Physics.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Product, Quantity(..), Rate)
import Speed exposing (MetersPerSecond)
import Sphere3d exposing (Sphere3d)
import Torque exposing (NewtonMeters)
import Vector3d exposing (Vector3d)


{-| Coordinate system of the simulation, used for positions and velocities
of bodies.
-}
type alias WorldCoordinates =
    Internal.Coordinates.WorldCoordinates


{-| Coordinate system of a body, used for defining
[shapes](Physics-Shape#Shape) and [constraints](Physics-Constraint#Constraint)
relative to the body’s origin.
-}
type alias BodyCoordinates =
    Internal.Coordinates.BodyCoordinates


{-| A body is pure physics state — position, velocity, orientation.
All bodies start out centered on the origin; use [moveTo](#moveTo) to set the position.

Use [block](#block), [plane](#plane), [sphere](#sphere), or [cylinder](#cylinder)
for simple bodies, or [dynamic](#dynamic) and [static](#static) for bodies
made of multiple [shapes](Physics-Shape#Shape).

-}
type alias Body =
    Types.Body


{-| -}
block : Block3d Meters BodyCoordinates -> Material Dense -> Body
block block3d mat =
    dynamic [ ( Shape.block block3d, mat ) ]


{-| Create a static plane, collidable only from the direction of the normal,
e.g. for +Z:

    floor =
        Physics.plane Plane3d.xy Material.wood

-}
plane : Plane3d Meters BodyCoordinates -> Material any -> Body
plane plane3d (Types.Material internalMat) =
    let
        position =
            Point3d.toMeters (Plane3d.originPoint plane3d)

        normal =
            Direction3d.unwrap (Plane3d.normalDirection plane3d)
    in
    Types.Body
        (InternalBody.compound
            [ ( InternalShape.Plane { position = position, normal = normal }
              , { internalMat | density = 0 }
              , 1
              )
            ]
        )


{-| -}
sphere : Sphere3d Meters BodyCoordinates -> Material Dense -> Body
sphere sphere3d mat =
    dynamic [ ( Shape.sphere sphere3d, mat ) ]


{-| Create a cylinder, approximated with 12 side faces.
For more subdivisions, use [dynamic](#dynamic) with [Shape.cylinder](Physics-Shape#cylinder).
-}
cylinder : Cylinder3d Meters BodyCoordinates -> Material Dense -> Body
cylinder cylinder3d mat =
    dynamic [ ( Shape.cylinder 12 cylinder3d, mat ) ]


{-| Create a point mass — infinitely small, so it doesn’t collide with other point masses.
-}
pointMass : Point3d Meters WorldCoordinates -> Mass -> Material any -> Body
pointMass position massVal (Types.Material internalMat) =
    Types.Body (InternalBody.particle (max 0 (Mass.inKilograms massVal)) internalMat)
        |> moveTo position


{-| Create a dynamic body from shapes and materials. Mass and center of mass
are derived from geometry and density.
-}
dynamic : List ( Shape, Material Dense ) -> Body
dynamic shapesWithMaterials =
    Types.Body
        (InternalBody.compound
            (List.concatMap
                (\( Types.Shape entries, Types.Material internalMat ) ->
                    List.map (\( shape, sign ) -> ( shape, internalMat, sign )) entries
                )
                shapesWithMaterials
            )
        )


{-| Create a static body from shapes and materials.
Static bodies only collide with dynamic bodies.
-}
static : List ( Shape, Material any ) -> Body
static shapesWithMaterials =
    Types.Body
        (InternalBody.compound
            (List.concatMap
                (\( Types.Shape entries, Types.Material internalMat ) ->
                    -- Strip density so mass = 0 (invMass = 0), making the body truly static
                    List.map (\( shape, _ ) -> ( shape, { internalMat | density = 0 }, 1 )) entries
                )
                shapesWithMaterials
            )
        )


{-| Set the position of the body in the world,
e.g. to raise a body 5 meters above the origin:

    movedBody =
        body
            |> moveTo (Point3d.meters 0 0 5)

-}
moveTo : Point3d Meters WorldCoordinates -> Body -> Body
moveTo point3d (Types.Body body) =
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
    Types.Body
        { body
            | transform3d = newTransform3d
            , worldShapesWithMaterials = List.map (\( s, m ) -> ( InternalShape.placeIn newTransform3d s, m )) body.shapesWithMaterials
        }


{-| Move the body relative to its current position,
e.g. to translate a body down by 5 meters:

    translatedBody =
        body
            |> translateBy (Vector3d.meters 0 0 -5)

-}
translateBy : Vector3d Meters WorldCoordinates -> Body -> Body
translateBy vector3d (Types.Body body) =
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
    Types.Body
        { body
            | transform3d = newTransform3d
            , worldShapesWithMaterials = List.map (\( s, m ) -> ( InternalShape.placeIn newTransform3d s, m )) body.shapesWithMaterials
        }


{-| Rotate the body around an axis in the world,
e.g. to rotate a body 45 degrees around the Z axis:

    rotatedBody =
        body
            |> rotateAround Axis3d.z (Angle.degrees 45)

-}
rotateAround : Axis3d Meters WorldCoordinates -> Angle -> Body -> Body
rotateAround axis angle (Types.Body body) =
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
    Types.Body
        { body
            | transform3d = newTransform3d
            , worldShapesWithMaterials = List.map (\( s, m ) -> ( InternalShape.placeIn newTransform3d s, m )) body.shapesWithMaterials
            , invInertiaWorld = Transform3d.invertedInertiaRotateIn newTransform3d body.invInertia
        }


{-| Set the position and orientation of a body. Like [moveTo](#moveTo)
but also sets the orientation.

    placedBody =
        body
            |> placeIn
                (Frame3d.atPoint (Point3d.meters 2 0 1)
                    |> Frame3d.rotateAround Axis3d.z (Angle.degrees 45)
                )

Left-handed frames are not supported — Z is recomputed from X and Y.

-}
placeIn : Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body -> Body
placeIn frame3d (Types.Body body) =
    let
        origin =
            Point3d.toMeters (Frame3d.originPoint frame3d)

        x =
            Direction3d.unwrap (Frame3d.xDirection frame3d)

        y =
            Direction3d.unwrap (Frame3d.yDirection frame3d)

        z =
            Vec3.cross x y

        newBodyCoordinatesTransform3d =
            Transform3d.fromOriginAndBasis origin x y z

        newTransform3d =
            Transform3d.placeIn
                newBodyCoordinatesTransform3d
                body.centerOfMassTransform3d
    in
    Types.Body
        { body
            | transform3d = newTransform3d
            , worldShapesWithMaterials = List.map (\( s, m ) -> ( InternalShape.placeIn newTransform3d s, m )) body.shapesWithMaterials
            , invInertiaWorld = Transform3d.invertedInertiaRotateIn newTransform3d body.invInertia
        }


{-| Simulates one frame. Returns updated bodies and contacts.
Call this on a message from the `onAnimationFrame` subscription
with [onEarth](#onEarth) to simulate 1/60th of a second in Earth gravity:

    ( simulated, contacts ) =
        simulate onEarth model.bodies

To improve solver stability for stacked objects, pass contacts
from the previous frame back via the config’s `contacts` field:

    ( simulated, contacts ) =
        simulate { onEarth | contacts = model.contacts }
            model.bodies

-}
simulate :
    Config id
    -> List ( id, Body )
    -> ( List ( id, Body ), Contacts id )
simulate config bodiesWithIds =
    let
        (Types.Contacts cache) =
            config.contacts

        ( internalBodiesWithIds, maxId ) =
            Internal.AssignIds.assignIds bodiesWithIds

        gravityVec =
            Vector3d.unwrap config.gravity

        dt =
            Duration.inSeconds config.duration

        constraints =
            InternalConstraint.getConstraints config.constrain
                internalBodiesWithIds

        -- Sort bodies by projection onto gravity axis so BroadPhase
        -- discovers ground contacts first, giving bottom-up solver order
        -- regardless of user body list order.
        sortedBodies =
            let
                projection ( _, body ) =
                    let
                        p =
                            Transform3d.originPoint body.transform3d
                    in
                    -(p.x * gravityVec.x + p.y * gravityVec.y + p.z * gravityVec.z)
            in
            List.sortBy projection internalBodiesWithIds

        contactGroups =
            BroadPhase.getContacts config.collide sortedBodies

        ( solverBodies, newLambdas, iters ) =
            Solver.solve dt gravityVec config.solverIterations constraints contactGroups maxId internalBodiesWithIds cache.lambdas
    in
    ( List.reverse (outputBodiesHelp dt gravityVec solverBodies internalBodiesWithIds [])
    , Types.Contacts
        { lambdas = newLambdas
        , iterations = iters
        , dt = dt
        , gravity = gravityVec
        , contactGroups = contactGroups
        , solverBodies = solverBodies
        }
    )


{-| A ready-to-use simulation config with Earth gravity pointing down (-Z)
at 60 fps. Customize it by updating individual fields,
see [Config](#Config) for available options.
-}
onEarth : Config id
onEarth =
    { gravity = Vector3d.gees 0 0 -1
    , duration = Duration.seconds (1 / 60)
    , solverIterations = 20
    , contacts = emptyContacts
    , constrain = \_ -> Nothing
    , collide = \_ _ -> True
    }


{-| Configures a simulation.

    onEarth =
        { gravity = Vector3d.gees 0 0 -1
        , duration = Duration.seconds (1 / 60)
        , solverIterations = 20
        , contacts = emptyContacts
        , constrain = \_ -> Nothing
        , collide = \_ \_ -> True
        }

  - `gravity` — set the gravity vector, or `Vector3d.zero` for no gravity

  - `duration` — set to `Duration.seconds (1 / 60)` for 60 fps

  - `solverIterations` — balance between precision and performance, 20 is a sweet spot

  - `contacts` — pass [Contacts](#Contacts) from the previous frame for warm starting, or leave as default for cold start

  - `constrain` — limit body movement relative to each other, see [Constraint](Physics-Constraint#Constraint)

  - `collide` — decide which bodies can collide with each other.

-}
type alias Config id =
    { gravity : Vector3d Acceleration.MetersPerSecondSquared WorldCoordinates
    , duration : Duration
    , solverIterations : Int
    , contacts : Contacts id
    , constrain : id -> Maybe (id -> List Constraint)
    , collide : id -> id -> Bool
    }


{-| Contacts from the most recent simulation frame. Contains contact points
and solver state for warm starting.
-}
type alias Contacts id =
    Types.Contacts id


{-| Get contact points from the most recent simulation frame, filtered by a predicate.
Each entry is a pair of body ids and a list of world-space contact points between them.

    crash =
        Physics.contactPoints
            (\a b -> a == "jeep" && b == "wall")
            contacts

-}
contactPoints : (id -> id -> Bool) -> Contacts id -> List ( id, id, List (Point3d Meters WorldCoordinates) )
contactPoints predicate (Types.Contacts c) =
    contactPointsHelp predicate c.dt c.gravity c.contactGroups c.solverBodies []


{-| Empty contacts for the first simulation frame (no warm starting).
-}
emptyContacts : Contacts id
emptyContacts =
    Types.Contacts
        { lambdas = Dict.empty
        , iterations = 0
        , dt = 0
        , gravity = Vec3.zero
        , contactGroups = []
        , solverBodies = Array.empty
        }


{-| Get the position and orientation of the body in the world as Frame3d.
Useful to transform points and directions between world and body coordinates,
e.g. for rendering.
-}
frame : Body -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
frame (Types.Body { transform3d, centerOfMassTransform3d }) =
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


{-| Get the origin point of a body in the world.
-}
originPoint : Body -> Point3d Meters WorldCoordinates
originPoint (Types.Body { transform3d, centerOfMassTransform3d }) =
    let
        bodyCoordinatesTransform3d =
            Transform3d.placeIn
                transform3d
                (Transform3d.inverse centerOfMassTransform3d)
    in
    Point3d.fromMeters
        (Transform3d.originPoint bodyCoordinatesTransform3d)


{-| Get the current linear velocity of a body.
-}
velocity : Body -> Vector3d MetersPerSecond WorldCoordinates
velocity (Types.Body body) =
    Vector3d.unsafe body.velocity


{-| Get the current angular velocity of a body.
-}
angularVelocity : Body -> Vector3d RadiansPerSecond WorldCoordinates
angularVelocity (Types.Body body) =
    Vector3d.unsafe body.angularVelocity


{-| Get the linear velocity of a point on a body.
Takes into account both linear and angular velocities.
-}
velocityAt : Point3d Meters WorldCoordinates -> Body -> Vector3d MetersPerSecond WorldCoordinates
velocityAt position (Types.Body body) =
    let
        origin =
            Transform3d.originPoint body.transform3d

        { x, y, z } =
            Point3d.toMeters position

        originToPoint =
            { x = x - origin.x
            , y = y - origin.y
            , z = z - origin.z
            }

        cross =
            Vec3.cross body.angularVelocity originToPoint
    in
    Vector3d.unsafe (Vec3.add cross body.velocity)


{-| Get the center of mass of a body. Returns Nothing for static bodies.
-}
centerOfMass : Body -> Maybe (Point3d Meters WorldCoordinates)
centerOfMass (Types.Body body) =
    if body.mass > 0 then
        Just (Point3d.fromMeters (Transform3d.originPoint body.transform3d))

    else
        Nothing


{-| Get the mass of a body. Returns Nothing for static bodies.
-}
mass : Body -> Maybe Mass
mass (Types.Body body) =
    if body.mass > 0 then
        Just (Mass.kilograms body.mass)

    else
        Nothing


{-| Find the closest intersection of a ray against a list of bodies.

  - point masses are always excluded because they are infinitely small
  - a plane only intersects when the ray is facing the plane’s normal

-}
raycast :
    Axis3d Meters WorldCoordinates
    -> List ( id, Body )
    -> Maybe ( id, Body, { point : Point3d Meters WorldCoordinates, normal : Direction3d WorldCoordinates } )
raycast axis bodiesWithIds =
    let
        ray =
            { direction = Direction3d.unwrap (Axis3d.direction axis)
            , from = Point3d.toMeters (Axis3d.originPoint axis)
            }
    in
    List.foldl
        (\( id, (Types.Body body) as originalBody ) closest ->
            case InternalBody.raycast ray body of
                Just result ->
                    case closest of
                        Just { bestDist } ->
                            if result.distance < bestDist then
                                Just { closestId = id, closestBody = originalBody, closestResult = result, bestDist = result.distance }

                            else
                                closest

                        Nothing ->
                            Just { closestId = id, closestBody = originalBody, closestResult = result, bestDist = result.distance }

                Nothing ->
                    closest
        )
        Nothing
        bodiesWithIds
        |> Maybe.map
            (\{ closestId, closestBody, closestResult } ->
                ( closestId
                , closestBody
                , { point = Point3d.fromMeters closestResult.point
                  , normal = Direction3d.unsafe (Vec3.normalize closestResult.normal)
                  }
                )
            )


{-| Apply a force at a point on a body.

Keep applying the force every simulation step to accelerate.

    pushedBox =
        box
            |> applyForce (Force.newtons 50)
                Direction3d.positiveY
                pointOnBox

-}
applyForce : Quantity Float Newtons -> Direction3d WorldCoordinates -> Point3d Meters WorldCoordinates -> Body -> Body
applyForce (Quantity force) direction position (Types.Body body) =
    if body.mass > 0 then
        Types.Body
            (InternalBody.applyForce force
                (Direction3d.unwrap direction)
                (Point3d.toMeters position)
                body
            )

    else
        Types.Body body


{-| Apply an impulse at a point on a body.

    impulse =
        Force.newtons 50
            |> Quantity.times (Duration.seconds 0.005)

    hitCueBall =
        cueBall
            |> applyImpulse impulse
                Direction3d.positiveY
                hitPoint

-}
applyImpulse : Quantity Float (Product Newtons Seconds) -> Direction3d WorldCoordinates -> Point3d Meters WorldCoordinates -> Body -> Body
applyImpulse (Quantity impulse) direction position (Types.Body body) =
    if body.mass > 0 then
        Types.Body
            (InternalBody.applyImpulse impulse
                (Direction3d.unwrap direction)
                (Point3d.toMeters position)
                body
            )

    else
        Types.Body body


{-| Scale a body to the given mass. The volume and center of mass
are preserved. Has no effect on static bodies.
-}
scaleTo : Mass -> Body -> Body
scaleTo desiredMass ((Types.Body body) as original) =
    let
        newMass =
            Mass.inKilograms desiredMass
    in
    if body.mass > 0 && newMass > 0 then
        let
            scale =
                body.mass / newMass

            im =
                body.invInertia

            newInvInertia =
                { m11 = im.m11 * scale
                , m12 = im.m12 * scale
                , m13 = im.m13 * scale
                , m21 = im.m21 * scale
                , m22 = im.m22 * scale
                , m23 = im.m23 * scale
                , m31 = im.m31 * scale
                , m32 = im.m32 * scale
                , m33 = im.m33 * scale
                }
        in
        Types.Body
            { body
                | mass = newMass
                , invMass = 1 / newMass
                , invInertia = newInvInertia
                , invInertiaWorld = Transform3d.invertedInertiaRotateIn body.transform3d newInvInertia
            }

    else
        original


{-| Set linear and angular damping, in order to decrease velocity over time.
These parameters specify the proportion of velocity lost per second.
Inputs are clamped between 0 and 1, the defaults are 0.01.
-}
damp : { linear : Float, angular : Float } -> Body -> Body
damp { linear, angular } (Types.Body body) =
    Types.Body
        { body
            | linearDamping = clamp 0 1 linear
            , angularDamping = clamp 0 1 angular
        }


{-| Apply the inverse inertia tensor of a body to a vector.
Returns Vector3d.zero for static bodies.
For common cases, see [angularAccelerationFromTorque](#angularAccelerationFromTorque)
and [angularVelocityDeltaFromAngularImpulse](#angularVelocityDeltaFromAngularImpulse).
-}
applyInverseInertia : Body -> Vector3d units WorldCoordinates -> Vector3d (Rate units (Product Kilograms SquareMeters)) WorldCoordinates
applyInverseInertia (Types.Body body) vector =
    if body.mass > 0 then
        let
            { x, y, z } =
                Vector3d.unwrap vector
        in
        Vector3d.unsafe
            { x = body.invInertiaWorld.m11 * x + body.invInertiaWorld.m12 * y + body.invInertiaWorld.m13 * z
            , y = body.invInertiaWorld.m21 * x + body.invInertiaWorld.m22 * y + body.invInertiaWorld.m23 * z
            , z = body.invInertiaWorld.m31 * x + body.invInertiaWorld.m32 * y + body.invInertiaWorld.m33 * z
            }

    else
        Vector3d.zero


{-| Compute angular acceleration from torque: α = I⁻¹τ

How fast will this torque make the body spin up?

-}
angularAccelerationFromTorque : Body -> Vector3d NewtonMeters WorldCoordinates -> Vector3d RadiansPerSecondSquared WorldCoordinates
angularAccelerationFromTorque body torque =
    Vector3d.unsafe (Vector3d.unwrap (applyInverseInertia body torque))


{-| Compute angular velocity change from an angular impulse: Δω = I⁻¹L

How much does this impulse add to my current spin?

-}
angularVelocityDeltaFromAngularImpulse : Body -> Vector3d (Product NewtonMeters Seconds) WorldCoordinates -> Vector3d RadiansPerSecond WorldCoordinates
angularVelocityDeltaFromAngularImpulse body angularImpulse =
    Vector3d.unsafe (Vector3d.unwrap (applyInverseInertia body angularImpulse))


contactPointsHelp :
    (id -> id -> Bool)
    -> Float
    -> Vec3.Vec3
    -> List InternalContact.ContactGroup
    -> Array (SolverBody.SolverBody id)
    -> List ( id, id, List (Point3d Meters WorldCoordinates) )
    -> List ( id, id, List (Point3d Meters WorldCoordinates) )
contactPointsHelp predicate dt gravity internalContactGroups solverBodies acc =
    case internalContactGroups of
        [] ->
            acc

        contactGroup :: remainingContactGroups ->
            case Array.get contactGroup.body1.id solverBodies of
                Just solverBody1 ->
                    case Array.get contactGroup.body2.id solverBodies of
                        Just solverBody2 ->
                            if predicate solverBody1.extId solverBody2.extId || predicate solverBody2.extId solverBody1.extId then
                                let
                                    newBody1 =
                                        SolverBody.toBody dt gravity solverBody1

                                    -- Transform contact points from pre-sim body1 frame to post-sim body1 frame
                                    transform =
                                        Transform3d.atOrigin
                                            |> Transform3d.relativeTo contactGroup.body1.transform3d
                                            |> Transform3d.placeIn newBody1.transform3d

                                    points =
                                        List.map
                                            (\{ contact } ->
                                                Point3d.fromMeters (Transform3d.pointPlaceIn transform contact.pi)
                                            )
                                            contactGroup.contacts
                                in
                                contactPointsHelp predicate
                                    dt
                                    gravity
                                    remainingContactGroups
                                    solverBodies
                                    (( solverBody1.extId, solverBody2.extId, points ) :: acc)

                            else
                                contactPointsHelp predicate dt gravity remainingContactGroups solverBodies acc

                        Nothing ->
                            contactPointsHelp predicate dt gravity remainingContactGroups solverBodies acc

                Nothing ->
                    contactPointsHelp predicate dt gravity remainingContactGroups solverBodies acc


outputBodiesHelp :
    Float
    -> Vec3.Vec3
    -> Array (SolverBody.SolverBody id)
    -> List ( id, InternalBody.Body )
    -> List ( id, Body )
    -> List ( id, Body )
outputBodiesHelp dt gravity solverBodies bodies acc =
    case bodies of
        [] ->
            acc

        ( extId, body ) :: rest ->
            case Array.get body.id solverBodies of
                Just solverBody ->
                    outputBodiesHelp dt
                        gravity
                        solverBodies
                        rest
                        (( extId, Types.Body (SolverBody.toBody dt gravity solverBody) ) :: acc)

                Nothing ->
                    outputBodiesHelp dt
                        gravity
                        solverBodies
                        rest
                        (( extId, Types.Body body ) :: acc)
