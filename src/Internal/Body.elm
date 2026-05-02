module Internal.Body exposing
    ( Body
    , Kind(..)
    , applyAngularImpulse
    , applyForce
    , applyImpulse
    , applyTorque
    , compound
    , lock
    , pointMass
    , raycast
    )

import Internal.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Internal.Lock as Lock exposing (Lock)
import Internal.Material exposing (Material)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Shape as Shape exposing (CenterOfMassCoordinates, Shape(..))
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Static: not moved by the engine, not moved by the user (an immobile wall).
Dynamic: moved by the engine in response to forces, gravity, and contacts.
Kinematic: moved by the engine according to the user-set velocity, but ignores
forces, gravity, and contacts. Other dynamic bodies see the kinematic's
velocity and respond with friction and contact forces accordingly.
-}
type Kind
    = Static
    | Dynamic
    | Kinematic


type alias Body =
    { id : Int -- ephemeral index assigned during simulation, -1 when not in a simulation
    , kind : Kind
    , transform3d : Transform3d WorldCoordinates { defines : CenterOfMassCoordinates }
    , centerOfMassTransform3d : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    , velocity : Vec3
    , angularVelocity : Vec3
    , mass : Float
    , volume : Float -- net volume: solid shapes minus void shapes (m³)
    , shapesWithMaterials : List ( Shape CenterOfMassCoordinates, Material )
    , worldShapesWithMaterials : List ( Shape WorldCoordinates, Material )
    , force : Vec3
    , torque : Vec3
    , boundingSphereRadius : Float

    -- damping
    , linearDamping : Float
    , angularDamping : Float

    -- mass props
    , invMass : Float
    , invInertia : Vec3
    , invInertiaWorld : Mat3

    -- world-axis DOF masks: 0 = locked, 1 = free
    , linearLock : Vec3
    , angularLock : Vec3
    }


{-| Accumulate total mass, net volume, and unscaled center-of-mass sum in one pass.
Divides the CoM sum by totalMass at the end; returns zero if totalMass is zero.
-}
accumulateMassProps :
    List ( Shape BodyCoordinates, Material, Float )
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> { totalMass : Float, totalVolume : Float, centerOfMassPoint : Vec3 }
accumulateMassProps shapes totalMass totalVolume comX comY comZ =
    case shapes of
        [] ->
            { totalMass = totalMass
            , totalVolume = totalVolume
            , centerOfMassPoint =
                if totalMass > 0 then
                    { x = comX / totalMass
                    , y = comY / totalMass
                    , z = comZ / totalMass
                    }

                else
                    Vec3.zero
            }

        ( shape, { density }, sign ) :: rest ->
            let
                signedVolume =
                    sign * Shape.volume shape

                signedMass =
                    signedVolume * density

                { x, y, z } =
                    Shape.centerOfMass shape
            in
            accumulateMassProps rest
                (totalMass + signedMass)
                (totalVolume + signedVolume)
                (comX + signedMass * x)
                (comY + signedMass * y)
                (comZ + signedMass * z)


{-| Move shapes to center-of-mass coordinates, accumulate inertia, net volume,
bounding sphere radius, and collect solid shapes.
-}
placeShapes :
    Transform3d CenterOfMassCoordinates { defines : BodyCoordinates }
    -> Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    -> List ( Shape BodyCoordinates, Material, Float )
    -> Mat3
    -> List ( Shape CenterOfMassCoordinates, Material )
    -> Float
    ->
        { inertia : Mat3
        , solidShapes : List ( Shape CenterOfMassCoordinates, Material )
        , boundingSphereRadius : Float
        }
placeShapes inverseCenterOfMassTransform3d centerOfMassTransform3d shapes inertia solidShapes boundingSphereRadius =
    case shapes of
        [] ->
            { inertia = inertia
            , solidShapes = solidShapes
            , boundingSphereRadius = boundingSphereRadius
            }

        ( shape, mat, sign ) :: rest ->
            let
                movedShape =
                    Shape.placeIn inverseCenterOfMassTransform3d shape

                volume =
                    Shape.volume movedShape

                resultInertia =
                    Transform3d.inertiaPlaceIn centerOfMassTransform3d
                        (Transform3d.pointPlaceIn centerOfMassTransform3d (Shape.centerOfMass movedShape))
                        volume
                        (Shape.inertia movedShape)
            in
            placeShapes inverseCenterOfMassTransform3d
                centerOfMassTransform3d
                rest
                (Mat3.add inertia (Mat3.scale (sign * mat.density) resultInertia))
                (if sign > 0 then
                    ( movedShape, mat ) :: solidShapes

                 else
                    solidShapes
                )
                (if sign > 0 then
                    Shape.expandBoundingSphereRadius movedShape boundingSphereRadius

                 else
                    boundingSphereRadius
                )


compound : Kind -> List ( Shape BodyCoordinates, Material, Float ) -> Body
compound kind rawShapesWithMaterials =
    let
        -- Static and kinematic bodies have infinite mass — strip user densities
        -- so totalMass and inertia come out zero regardless of the materials passed.
        shapesWithMaterials =
            case kind of
                Dynamic ->
                    rawShapesWithMaterials

                _ ->
                    List.map
                        (\( shape, mat, sign ) -> ( shape, { mat | density = 0 }, sign ))
                        rawShapesWithMaterials

        { totalMass, totalVolume, centerOfMassPoint } =
            accumulateMassProps shapesWithMaterials 0 0 0 0 0

        initialCenterOfMassTransform3d =
            Transform3d.atPoint centerOfMassPoint

        initialInverseCenterOfMassTransform3d =
            Transform3d.inverse initialCenterOfMassTransform3d

        initialPlaced =
            placeShapes initialInverseCenterOfMassTransform3d initialCenterOfMassTransform3d shapesWithMaterials Mat3.zero [] 0

        { eigenvalues, v1, v2, v3 } =
            Mat3.eigenDecomposition initialPlaced.inertia

        eigenRotation =
            Transform3d.fromOriginAndBasis Vec3.zero v1 v2 v3

        centerOfMassTransform3d =
            Transform3d.placeIn initialCenterOfMassTransform3d eigenRotation

        inverseCenterOfMassTransform3d =
            Transform3d.inverse centerOfMassTransform3d

        placed =
            placeShapes inverseCenterOfMassTransform3d centerOfMassTransform3d shapesWithMaterials Mat3.zero [] 0

        transform3d =
            Transform3d.placeIn Transform3d.atOrigin centerOfMassTransform3d

        invInertia =
            { x =
                if eigenvalues.x == 0 then
                    0

                else
                    1 / eigenvalues.x
            , y =
                if eigenvalues.y == 0 then
                    0

                else
                    1 / eigenvalues.y
            , z =
                if eigenvalues.z == 0 then
                    0

                else
                    1 / eigenvalues.z
            }
    in
    { id = -1
    , kind = kind
    , velocity = Vec3.zero
    , angularVelocity = Vec3.zero
    , transform3d = transform3d
    , centerOfMassTransform3d = centerOfMassTransform3d
    , mass = totalMass
    , volume = totalVolume
    , shapesWithMaterials = placed.solidShapes
    , worldShapesWithMaterials = List.map (\( s, m ) -> ( Shape.placeIn transform3d s, m )) placed.solidShapes
    , boundingSphereRadius = placed.boundingSphereRadius
    , linearDamping = 0.01
    , angularDamping = 0.01
    , invMass =
        if totalMass == 0 then
            0

        else
            1 / totalMass
    , invInertia = invInertia
    , invInertiaWorld = Transform3d.invertedInertiaRotateIn transform3d invInertia
    , force = Vec3.zero
    , torque = Vec3.zero
    , linearLock = { x = 1, y = 1, z = 1 }
    , angularLock = { x = 1, y = 1, z = 1 }
    }


pointMass : Vec3 -> Float -> Material -> Body
pointMass position mass { friction, bounciness } =
    let
        contactMaterial =
            { friction = friction
            , bounciness = bounciness
            , density = 0
            }
    in
    { id = -1
    , kind = Dynamic
    , velocity = Vec3.zero
    , angularVelocity = Vec3.zero
    , transform3d = Transform3d.atPoint position
    , centerOfMassTransform3d = Transform3d.atOrigin
    , mass = mass
    , volume = 0
    , shapesWithMaterials = [ ( Particle Vec3.zero, contactMaterial ) ]
    , worldShapesWithMaterials = [ ( Particle position, contactMaterial ) ]
    , boundingSphereRadius = 0
    , linearDamping = 0.01
    , angularDamping = 0.01
    , invMass = 1 / mass
    , invInertia = Vec3.zero
    , invInertiaWorld = Mat3.zero
    , force = Vec3.zero
    , torque = Vec3.zero
    , linearLock = Vec3.one
    , angularLock = Vec3.one
    }


applyImpulse : Vec3 -> Vec3 -> Body -> Body
applyImpulse impulse point body =
    let
        relativePoint =
            Vec3.sub point (Transform3d.originPoint body.transform3d)

        { x, y, z } =
            Vec3.cross relativePoint impulse

        { angularVelocity, invInertiaWorld, velocity, invMass } =
            body
    in
    { body
        | velocity =
            { x = velocity.x + invMass * impulse.x
            , y = velocity.y + invMass * impulse.y
            , z = velocity.z + invMass * impulse.z
            }
        , angularVelocity =
            { x = angularVelocity.x + invInertiaWorld.m11 * x + invInertiaWorld.m12 * y + invInertiaWorld.m13 * z
            , y = angularVelocity.y + invInertiaWorld.m21 * x + invInertiaWorld.m22 * y + invInertiaWorld.m23 * z
            , z = angularVelocity.z + invInertiaWorld.m31 * x + invInertiaWorld.m32 * y + invInertiaWorld.m33 * z
            }
    }


applyForce : Vec3 -> Vec3 -> Body -> Body
applyForce force point body =
    let
        relativePoint =
            Vec3.sub point (Transform3d.originPoint body.transform3d)

        torque =
            Vec3.cross relativePoint force
    in
    { body
        | force = Vec3.add body.force force
        , torque = Vec3.add body.torque torque
    }


applyTorque : Vec3 -> Body -> Body
applyTorque torque body =
    { body | torque = Vec3.add body.torque torque }


applyAngularImpulse : Vec3 -> Body -> Body
applyAngularImpulse angularImpulse body =
    let
        { x, y, z } =
            angularImpulse

        { angularVelocity, invInertiaWorld } =
            body
    in
    { body
        | angularVelocity =
            { x = angularVelocity.x + invInertiaWorld.m11 * x + invInertiaWorld.m12 * y + invInertiaWorld.m13 * z
            , y = angularVelocity.y + invInertiaWorld.m21 * x + invInertiaWorld.m22 * y + invInertiaWorld.m23 * z
            , z = angularVelocity.z + invInertiaWorld.m31 * x + invInertiaWorld.m32 * y + invInertiaWorld.m33 * z
            }
    }


{-| Replace the body’s locked degrees of freedom. The list fully describes
the lock state — calling `lock` again replaces the previous locks. An empty
list clears all locks. The body’s current velocities are left untouched; the
mask is enforced at the next simulation step.
-}
lock : List Lock -> Body -> Body
lock locks body =
    let
        ( linearLock, angularLock ) =
            Lock.masks locks
    in
    { body
        | linearLock = linearLock
        , angularLock = angularLock
    }


raycast :
    { from : Vec3, direction : Vec3 }
    -> Body
    -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray body =
    List.foldl
        (\( shape, _ ) maybeClosestRaycastResult ->
            case Shape.raycast ray shape of
                (Just raycastResult) as passThrough ->
                    case maybeClosestRaycastResult of
                        Just closestRaycastResult ->
                            if raycastResult.distance - closestRaycastResult.distance < 0 then
                                passThrough

                            else
                                maybeClosestRaycastResult

                        Nothing ->
                            passThrough

                Nothing ->
                    maybeClosestRaycastResult
        )
        Nothing
        body.worldShapesWithMaterials
