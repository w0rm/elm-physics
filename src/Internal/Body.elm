module Internal.Body exposing
    ( Body
    , Protected(..)
    , applyForce
    , applyImpulse
    , compound
    , particle
    , raycast
    )

import Internal.Material exposing (Material)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Shape as Shape exposing (CenterOfMassCoordinates, Shape(..))
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)


type Protected
    = Protected Body


type alias Body =
    { id : Int -- ephemeral index assigned during simulation, -1 when not in a simulation
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
    , invInertia : Mat3
    , invInertiaWorld : Mat3
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


compound : List ( Shape BodyCoordinates, Material, Float ) -> Body
compound shapesWithMaterials =
    let
        massProps =
            accumulateMassProps shapesWithMaterials 0 0 0 0 0

        centerOfMassTransform3d =
            Transform3d.atPoint massProps.centerOfMassPoint

        inverseCenterOfMassTransform3d =
            Transform3d.inverse centerOfMassTransform3d

        transform3d =
            Transform3d.placeIn Transform3d.atOrigin centerOfMassTransform3d

        placed =
            placeShapes inverseCenterOfMassTransform3d centerOfMassTransform3d shapesWithMaterials Mat3.zero [] 0

        invInertia =
            Mat3.inverse placed.inertia
    in
    { id = -1
    , velocity = Vec3.zero
    , angularVelocity = Vec3.zero
    , transform3d = transform3d
    , centerOfMassTransform3d = centerOfMassTransform3d
    , mass = massProps.totalMass
    , volume = massProps.totalVolume
    , shapesWithMaterials = placed.solidShapes
    , worldShapesWithMaterials = List.map (\( s, m ) -> ( Shape.placeIn transform3d s, m )) placed.solidShapes
    , boundingSphereRadius = placed.boundingSphereRadius
    , linearDamping = 0.01
    , angularDamping = 0.01
    , invMass =
        if massProps.totalMass == 0 then
            0

        else
            1 / massProps.totalMass
    , invInertia = invInertia
    , invInertiaWorld = Transform3d.invertedInertiaRotateIn transform3d invInertia
    , force = Vec3.zero
    , torque = Vec3.zero
    }


particle : Float -> Material -> Body
particle mass { friction, bounciness } =
    let
        contactMaterial =
            { friction = friction
            , bounciness = bounciness
            , density = 0
            }
    in
    { id = -1
    , velocity = Vec3.zero
    , angularVelocity = Vec3.zero
    , transform3d = Transform3d.atOrigin
    , centerOfMassTransform3d = Transform3d.atOrigin
    , mass = mass
    , volume = 0
    , shapesWithMaterials = [ ( Particle Vec3.zero, contactMaterial ) ]
    , worldShapesWithMaterials = [ ( Particle Vec3.zero, contactMaterial ) ]
    , boundingSphereRadius = 0
    , linearDamping = 0.01
    , angularDamping = 0.01
    , invMass =
        if mass > 0 then
            1 / mass

        else
            0
    , invInertia = Mat3.zero
    , invInertiaWorld = Mat3.zero
    , force = Vec3.zero
    , torque = Vec3.zero
    }


applyImpulse : Float -> Vec3 -> Vec3 -> Body -> Body
applyImpulse amount direction point body =
    let
        relativePoint =
            Vec3.sub point (Transform3d.originPoint body.transform3d)

        impulse =
            Vec3.scale amount direction

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


applyForce : Float -> Vec3 -> Vec3 -> Body -> Body
applyForce amount direction point body =
    let
        relativePoint =
            Vec3.sub point (Transform3d.originPoint body.transform3d)

        force =
            Vec3.scale amount direction

        torque =
            Vec3.cross relativePoint force
    in
    { body
        | force = Vec3.add body.force force
        , torque = Vec3.add body.torque torque
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
