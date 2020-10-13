module Internal.Body exposing
    ( Body
    , Protected(..)
    , applyForce
    , applyImpulse
    , centerOfMass
    , compound
    , raycast
    , updateMassProperties
    )

import Internal.Material as Material exposing (Material)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Shape as Shape exposing (CenterOfMassCoordinates, Shape(..))
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)


type Protected data
    = Protected (Body data)


type alias Body data =
    { id : Int
    , data : data
    , material : Material
    , transform3d : Transform3d WorldCoordinates { defines : CenterOfMassCoordinates }
    , centerOfMassTransform3d : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
    , velocity : Vec3
    , angularVelocity : Vec3
    , mass : Float
    , shapes : List (Shape CenterOfMassCoordinates)
    , worldShapes : List (Shape WorldCoordinates)
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


centerOfMass : List (Shape BodyCoordinates) -> Vec3
centerOfMass shapes =
    let
        totalVolume =
            List.foldl (\shape sum -> sum + Shape.volume shape) 0 shapes
    in
    if totalVolume > 0 then
        List.foldl
            (\shape ->
                Vec3.add
                    (Vec3.scale
                        (Shape.volume shape / totalVolume)
                        (case shape of
                            Convex { position } ->
                                position

                            Particle position ->
                                position

                            Sphere { position } ->
                                position

                            Plane { position } ->
                                position
                        )
                    )
            )
            Vec3.zero
            shapes

    else
        Vec3.zero


compound : List (Shape BodyCoordinates) -> data -> Body data
compound shapes data =
    let
        centerOfMassPoint =
            centerOfMass shapes

        centerOfMassTransform3d : Transform3d BodyCoordinates { defines : CenterOfMassCoordinates }
        centerOfMassTransform3d =
            Transform3d.atPoint centerOfMassPoint

        inverseCenterOfMassTransform3d : Transform3d CenterOfMassCoordinates { defines : BodyCoordinates }
        inverseCenterOfMassTransform3d =
            Transform3d.inverse centerOfMassTransform3d

        bodyTransform3d : Transform3d WorldCoordinates { defines : BodyCoordinates }
        bodyTransform3d =
            Transform3d.atOrigin

        transform3d : Transform3d WorldCoordinates { defines : CenterOfMassCoordinates }
        transform3d =
            Transform3d.placeIn bodyTransform3d centerOfMassTransform3d

        shapeTransform =
            Transform3d.placeIn transform3d inverseCenterOfMassTransform3d

        movedShapes : List (Shape CenterOfMassCoordinates)
        movedShapes =
            Shape.shapesPlaceIn inverseCenterOfMassTransform3d shapes
    in
    updateMassProperties
        { id = -1
        , data = data
        , material = Material.default
        , transform3d = transform3d
        , centerOfMassTransform3d = centerOfMassTransform3d
        , velocity = Vec3.zero
        , angularVelocity = Vec3.zero
        , mass = 0
        , shapes = movedShapes
        , worldShapes = Shape.shapesPlaceIn shapeTransform shapes
        , force = Vec3.zero
        , torque = Vec3.zero
        , boundingSphereRadius = List.foldl Shape.expandBoundingSphereRadius 0 movedShapes

        -- default damping
        , linearDamping = 0.01
        , angularDamping = 0.01

        -- mass props
        , invMass = 0
        , invInertia = Mat3.zero
        , invInertiaWorld = Mat3.zero
        }


{-| Should be called whenever you change the body shapes or mass.
-}
updateMassProperties : Body data -> Body data
updateMassProperties ({ mass, shapes } as body) =
    let
        totalVolume =
            List.foldl (\shape result -> Shape.volume shape + result) 0 shapes

        density =
            mass / totalVolume

        inertia =
            List.foldl
                (\shape ->
                    let
                        shapeInertia =
                            Shape.inertia shape

                        shapeCenterOfMass =
                            Shape.centerOfMass shape

                        shapeVolume =
                            Shape.volume shape

                        -- TODO: this undoes the center of mass transformation
                        -- to get the initial shape’s center of mass position.
                        -- This is needed to correctly transform the inertia
                        -- need to rethink the inertia and center of mass transformation
                        resultInertia =
                            Transform3d.inertiaPlaceIn body.centerOfMassTransform3d
                                (Transform3d.pointPlaceIn body.centerOfMassTransform3d shapeCenterOfMass)
                                shapeVolume
                                shapeInertia
                    in
                    Mat3.add resultInertia
                )
                Mat3.zero
                shapes
                |> Mat3.scale density

        invMass =
            if mass == 0 then
                0

            else
                1 / mass

        invInertia =
            Mat3.inverse inertia
    in
    { body
        | invMass = invMass
        , invInertia = invInertia
        , invInertiaWorld = Transform3d.invertedInertiaRotateIn body.transform3d invInertia
    }


applyImpulse : Float -> Vec3 -> Vec3 -> Body data -> Body data
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


applyForce : Float -> Vec3 -> Vec3 -> Body data -> Body data
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
    -> Body data
    -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray body =
    List.foldl
        (\shape maybeClosestRaycastResult ->
            case Shape.raycast ray shape of
                Just raycastResult ->
                    case maybeClosestRaycastResult of
                        Just closestRaycastResult ->
                            if raycastResult.distance - closestRaycastResult.distance < 0 then
                                Just raycastResult

                            else
                                maybeClosestRaycastResult

                        Nothing ->
                            Just raycastResult

                Nothing ->
                    maybeClosestRaycastResult
        )
        Nothing
        body.worldShapes
