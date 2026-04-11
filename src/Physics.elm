module Physics exposing
    ( Body
    , block, plane, sphere, cylinder, particle
    , moveTo, translateBy, rotateAround, placeIn
    , simulate, onEarth, Config
    , Contacts, contacts, emptyContacts
    , frame, originPoint, velocity, angularVelocity, velocityAt
    , centerOfMass, mass, volume
    , raycast, applyForce, applyImpulse
    , dynamic, static
    , damp, scaleTo, applyInverseInertia, angularAccelerationFromTorque, angularVelocityDeltaFromAngularImpulse
    )

{-|


# Bodies

@docs Body

@docs block, plane, sphere, cylinder, particle


# Positioning

@docs moveTo, translateBy, rotateAround, placeIn


# Simulation

@docs simulate, onEarth, Config

@docs Contacts, contacts, emptyContacts


# Querying

@docs frame, originPoint, velocity, angularVelocity, velocityAt

@docs centerOfMass, mass, volume


# Interaction

@docs raycast, applyForce, applyImpulse


# Composite bodies

For bodies made of multiple shapes or custom materials, use these instead of the simple constructors above.

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
import Internal.Shape as InternalShape
import Internal.Solver as Solver
import Internal.SolverBody as SolverBody
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Length exposing (Meters)
import Mass exposing (Kilograms, Mass)
import Physics.Constraint exposing (Constraint)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material exposing (HasDensity, Material)
import Physics.Shape as Shape exposing (Shape)
import Physics.Types as Types
import Point3d exposing (Point3d)
import Quantity exposing (Product, Quantity(..), Rate)
import Speed exposing (MetersPerSecond)
import Sphere3d exposing (Sphere3d)
import Torque exposing (NewtonMeters)
import Vector3d exposing (Vector3d)
import Volume exposing (Volume)


{-| A body is pure physics state — position, velocity, orientation.
All bodies start out centered on the origin; use [moveTo](#moveTo) to set the position.
-}
type alias Body =
    Types.Body


{-| Create a block body from geometry and material.
-}
block : Block3d Meters BodyCoordinates -> Material HasDensity -> Body
block block3d mat =
    dynamic [ ( Shape.block block3d, mat ) ]


{-| Create a static infinite plane. The normal points in the Z direction.
-}
plane : Material any -> Body
plane (Types.Material internalMat) =
    Types.Body
        (InternalBody.compound
            [ ( InternalShape.Plane { position = Vec3.zero, normal = Vec3.zAxis }
              , { internalMat | density = 0 }
              , 1
              )
            ]
        )


{-| Create a sphere body from geometry and material.
-}
sphere : Sphere3d Meters BodyCoordinates -> Material HasDensity -> Body
sphere sphere3d mat =
    dynamic [ ( Shape.sphere sphere3d, mat ) ]


{-| Create a cylinder body, approximated with 12 side faces.
For more subdivisions, use [dynamic](#dynamic) with [Shape.cylinder](Physics-Shape#cylinder).
-}
cylinder : Cylinder3d Meters BodyCoordinates -> Material HasDensity -> Body
cylinder cylinder3d mat =
    dynamic [ ( Shape.cylinder 12 cylinder3d, mat ) ]


{-| Create a particle — a point mass with no volume.
Takes mass directly since there is no geometry to derive it from.
Particles don’t collide with each other.
-}
particle : Mass -> Material a -> Body
particle massVal (Types.Material internalMat) =
    Types.Body (InternalBody.particle (max 0 (Mass.inKilograms massVal)) internalMat)


{-| Create a dynamic body from shapes and materials. Mass and center of mass
are derived from geometry and density. Shapes are assumed not to overlap —
use [Shape.minus](Physics-Shape#minus) for hollow bodies.
-}
dynamic : List ( Shape, Material HasDensity ) -> Body
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


{-| Create a static body from shapes and materials. Static bodies don’t move
but collide with dynamic bodies.
-}
static : List ( Shape, Material a ) -> Body
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


{-| Place the body at an absolute position and orientation,
e.g. to place a body at a specific location and rotation:

    placedBody =
        body
            |> placeIn
                (Frame3d.atPoint (Point3d.meters 2 0 1)
                    |> Frame3d.rotateAround Axis3d.z (Angle.degrees 45)
                )

This is similar to [moveTo](#moveTo) but also sets the orientation.
Like `moveTo`, this is absolute — calling it again overwrites the previous
position and orientation.

Left-handed (mirrored) frames are not supported — the Z direction is
recomputed as the cross product of X and Y, so any mirroring is ignored.

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


{-| Contacts from the most recent simulation frame. Contains contact points
and solver state for warm starting.
-}
type alias Contacts id =
    Types.Contacts id


{-| Get the contact points detected during the most recent simulation frame.
Each entry is a pair of body ids and a list of world-space contact points between them.
-}
contacts : Contacts id -> List ( id, id, List (Point3d Meters WorldCoordinates) )
contacts (Types.Contacts c) =
    c.contactPoints


{-| Simulates one frame. Returns updated bodies and contacts.

Pass `result.contacts` back via the config's `contacts` field to enable warm
starting, which improves solver stability for stacked objects.

    ( simulated, newContacts ) =
        simulate onEarth bodies

    -- with warm starting:
    ( simulated, newContacts ) =
        simulate { onEarth | contacts = model.contacts } bodies

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
        , contactPoints = contactsFromWorldHelp dt gravityVec contactGroups solverBodies []
        }
    )


{-| A ready-to-use simulation config with Earth gravity pointing down (-Z)
at 60 fps. Customize it by updating individual fields,
see [Config](#Config) for more options.
-}
onEarth : Config id
onEarth =
    { gravity = Vector3d.withLength (Acceleration.gees 1) Direction3d.negativeZ
    , duration = Duration.seconds (1 / 60)
    , solverIterations = 20
    , contacts = emptyContacts
    , constrain = \_ -> Nothing
    , collide = \_ _ -> True
    }


{-| Empty contacts for the first simulation frame (no warm starting).
-}
emptyContacts : Contacts id
emptyContacts =
    Types.Contacts { lambdas = Dict.empty, iterations = 0, contactPoints = [] }


{-| Configures a simulation.

    onEarth =
        { gravity = Vector3d.withLength (Acceleration.gees 1)
            Direction3d.negativeZ
        , duration = Duration.seconds (1 / 60)
        , solverIterations = 20
        , contacts = emptyContacts
        , constrain = \_ -> Nothing
        , collide = \_ \_ -> True
        }

  - `gravity` — set the gravity vector, or `Vector3d.zero` for no gravity

  - `duration` — set to `Duration.seconds (1 / 60)` for 60 fps

  - `solverIterations` — balance between precision and performance, 20 is a sweet spot

  - `contacts` — pass `Contacts` from the previous frame for warm starting, or leave as default for cold start

  - `constrain` — limit body movement relative to each other, see `Physics.Constraint`

  - `collide` — decide which bodies can collide with each other

-}
type alias Config id =
    { gravity : Vector3d Acceleration.MetersPerSecondSquared WorldCoordinates
    , duration : Duration
    , solverIterations : Int
    , contacts : Contacts id
    , constrain : id -> Maybe (id -> List Constraint)
    , collide : id -> id -> Bool
    }


{-| Get the position and orientation of the body in the world as Frame3d.
Useful to transform points and directions between world and body coordinates.
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
velocityAt point (Types.Body body) =
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


{-| Get the center of mass of a body in body coordinates. Returns Nothing for static bodies.
-}
centerOfMass : Body -> Maybe (Point3d Meters BodyCoordinates)
centerOfMass (Types.Body body) =
    if body.mass > 0 then
        Just (Point3d.fromMeters (Transform3d.originPoint body.centerOfMassTransform3d))

    else
        Nothing


{-| Get the mass of a body. Returns Nothing if the body is static.
-}
mass : Body -> Maybe Mass
mass (Types.Body body) =
    if body.mass > 0 then
        Just (Mass.kilograms body.mass)

    else
        Nothing


{-| Get the net volume of a body: solid shapes minus void shapes.
-}
volume : Body -> Volume
volume (Types.Body body) =
    Volume.cubicMeters body.volume


{-| Finds the closest intersection of a ray against a list of bodies.

    - particles are always excluded because they have zero size
    - plane only intersects when the ray is facing the plane’s normal

You may want to prefilter the list of bodies to exclude certain bodies
from intersection.

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
applyForce : Quantity Float Newtons -> Direction3d WorldCoordinates -> Point3d Meters WorldCoordinates -> Body -> Body
applyForce (Quantity force) direction point (Types.Body body) =
    if body.mass > 0 then
        Types.Body
            (InternalBody.applyForce
                force
                (Direction3d.unwrap direction)
                (Point3d.toMeters point)
                body
            )

    else
        Types.Body body


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
applyImpulse : Quantity Float (Product Newtons Seconds) -> Direction3d WorldCoordinates -> Point3d Meters WorldCoordinates -> Body -> Body
applyImpulse (Quantity impulse) direction point (Types.Body body) =
    if body.mass > 0 then
        Types.Body
            (InternalBody.applyImpulse
                impulse
                (Direction3d.unwrap direction)
                (Point3d.toMeters point)
                body
            )

    else
        Types.Body body


{-| Scale a dynamic body to the given mass, adjusting the inverse inertia tensor
proportionally. The volume and center of mass are preserved. Has no effect on static bodies.
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
For common cases, see [angularAccelerationFromTorque](#angularAccelerationFromTorque)
and [angularVelocityDeltaFromAngularImpulse](#angularVelocityDeltaFromAngularImpulse).
-}
applyInverseInertia : Body -> Vector3d units WorldCoordinates -> Vector3d (Rate units (Product Kilograms SquareMeters)) WorldCoordinates
applyInverseInertia (Types.Body { invInertiaWorld }) vector =
    let
        { x, y, z } =
            Vector3d.unwrap vector
    in
    Vector3d.unsafe
        { x = invInertiaWorld.m11 * x + invInertiaWorld.m12 * y + invInertiaWorld.m13 * z
        , y = invInertiaWorld.m21 * x + invInertiaWorld.m22 * y + invInertiaWorld.m23 * z
        , z = invInertiaWorld.m31 * x + invInertiaWorld.m32 * y + invInertiaWorld.m33 * z
        }


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


contactsFromWorldHelp :
    Float
    -> Vec3.Vec3
    -> List InternalContact.ContactGroup
    -> Array (SolverBody.SolverBody id)
    -> List ( id, id, List (Point3d Meters WorldCoordinates) )
    -> List ( id, id, List (Point3d Meters WorldCoordinates) )
contactsFromWorldHelp dt gravity contactGroups solverBodies acc =
    case contactGroups of
        [] ->
            acc

        contactGroup :: remainingContactGroups ->
            case Array.get contactGroup.body1.id solverBodies of
                Just solverBody1 ->
                    case Array.get contactGroup.body2.id solverBodies of
                        Just solverBody2 ->
                            let
                                newBody1 =
                                    SolverBody.toBody dt gravity solverBody1

                                -- Transform contact points from pre-sim body1 frame to post-sim body1 frame
                                transform =
                                    Transform3d.atOrigin
                                        |> Transform3d.relativeTo contactGroup.body1.transform3d
                                        |> Transform3d.placeIn newBody1.transform3d

                                contactPoints =
                                    List.map
                                        (\{ contact } ->
                                            Point3d.fromMeters (Transform3d.pointPlaceIn transform contact.pi)
                                        )
                                        contactGroup.contacts
                            in
                            contactsFromWorldHelp dt
                                gravity
                                remainingContactGroups
                                solverBodies
                                (( solverBody1.extId, solverBody2.extId, contactPoints ) :: acc)

                        Nothing ->
                            contactsFromWorldHelp dt gravity remainingContactGroups solverBodies acc

                Nothing ->
                    contactsFromWorldHelp dt gravity remainingContactGroups solverBodies acc


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
