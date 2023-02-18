module RaycastCar.Car exposing (CarSettings, Wheel, defaultWheel, simulate)

{-| This is a complex example implementing raycast vehicle enspired by:

  - bullet3: <https://github.com/bulletphysics/bullet3/blob/master/src/BulletDynamics/Vehicle/btRaycastVehicle.cpp>
  - cannon.js: <https://github.com/schteppe/cannon.js/blob/master/src/objects/RaycastVehicle.js>

This is a car with fake wheels, that shoots rays and applies
impulses to the car body where the rays hit the ground.

This allows to simulate suspension and results in smooth behavior.

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Vector3d


type alias CarSettings =
    { downDirection : Direction3d BodyCoordinates
    , rightDirection : Direction3d BodyCoordinates
    , forwardDirection : Direction3d BodyCoordinates
    , suspensionRestLength : Length
    , minSuspensionLength : Length
    , maxSuspensionLength : Length
    , radius : Length
    , suspensionStiffness : Float
    , dampingCompression : Float
    , dampingRelaxation : Float
    , frictionSlip : Float
    , rollInfluence : Float
    , maxSuspensionForce : Force
    }


type alias Wheel id =
    { chassisConnectionPoint : Point3d Meters BodyCoordinates
    , axis : Axis3d Meters BodyCoordinates
    , steering : Angle
    , rotation : Angle
    , deltaRotation : Angle
    , suspensionImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , suspensionLength : Length
    , engineForce : Force
    , brake : Force
    , contact :
        Maybe
            { point : Point3d Meters WorldCoordinates
            , normal : Direction3d WorldCoordinates
            , body : Body id
            }
    }


simulate :
    CarSettings
    -> Duration
    ->
        { worldWithoutCar : World id
        , speeding : Float
        , steering : Float
        , braking : Bool
        }
    -> List (Wheel id)
    -> Body id
    -> ( Body id, List (Wheel id) )
simulate carSettings dt { worldWithoutCar, steering, braking, speeding } wheels carBody =
    case wheels of
        [ w1, w2, w3, w4 ] ->
            let
                engineForce =
                    Force.newtons (5000 * speeding)

                brake =
                    if braking then
                        Force.newtons 10000

                    else
                        Quantity.zero

                wheel1 =
                    { w1 | steering = Angle.degrees (30 * steering), engineForce = engineForce, brake = brake }

                wheel2 =
                    { w2 | steering = Angle.degrees (30 * steering), engineForce = engineForce, brake = brake }

                wheel3 =
                    { w3 | engineForce = engineForce, brake = brake }

                wheel4 =
                    { w4 | engineForce = engineForce, brake = brake }
            in
            updateSuspension carSettings dt worldWithoutCar (Body.frame carBody) carBody [ wheel1, wheel2, wheel3, wheel4 ] carBody [] 0

        _ ->
            ( carBody, wheels )


updateSuspension : CarSettings -> Duration -> World id -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body id -> List (Wheel id) -> Body id -> List (Wheel id) -> Int -> ( Body id, List (Wheel id) )
updateSuspension carSettings dt world frame originalCar currentWheels updatedCar updatedWheels numWheelsOnGround =
    case currentWheels of
        [] ->
            updateFriction carSettings dt world frame updatedCar numWheelsOnGround updatedWheels [] [] False

        wheel :: remainingWheels ->
            let
                ray =
                    Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection
                        |> Axis3d.placeIn frame
            in
            case World.raycast ray world of
                Just { body, normal, point } ->
                    let
                        bodyFrame =
                            Body.frame body

                        contactPoint =
                            Point3d.placeIn bodyFrame point

                        contactNormal =
                            Direction3d.placeIn bodyFrame normal

                        distance =
                            Point3d.distanceFrom contactPoint (Axis3d.originPoint ray)

                        maxDistance =
                            Quantity.plus carSettings.suspensionRestLength carSettings.radius
                    in
                    if Quantity.lessThan maxDistance distance then
                        let
                            suspensionLength =
                                distance
                                    |> Quantity.minus carSettings.radius
                                    |> Quantity.clamp
                                        carSettings.minSuspensionLength
                                        carSettings.maxSuspensionLength

                            difference =
                                carSettings.suspensionRestLength
                                    |> Quantity.minus suspensionLength
                                    |> Length.inMeters

                            (Quantity projectedVelocity) =
                                Vector3d.dot
                                    (Direction3d.toVector contactNormal)
                                    (Body.velocityAt contactPoint originalCar)

                            (Quantity denominator) =
                                Vector3d.dot
                                    (Direction3d.toVector contactNormal)
                                    (Direction3d.toVector (Axis3d.direction ray))

                            ( suspensionRelativeVelocity, clippedInvContactDotSuspension ) =
                                if denominator >= -0.1 then
                                    ( 0, 1 / 0.1 )

                                else
                                    ( -projectedVelocity / denominator, -1 / denominator )

                            damping =
                                if suspensionRelativeVelocity < 0 then
                                    carSettings.dampingCompression

                                else
                                    carSettings.dampingRelaxation

                            suspensionImpulse =
                                ((carSettings.suspensionStiffness * difference * clippedInvContactDotSuspension)
                                    - (damping * suspensionRelativeVelocity)
                                )
                                    |> (*) (Body.mass originalCar |> Maybe.map Mass.inKilograms |> Maybe.withDefault 0)
                                    |> Force.newtons
                                    |> Quantity.clamp Quantity.zero carSettings.maxSuspensionForce
                                    |> Quantity.times dt
                        in
                        updateSuspension carSettings
                            dt
                            world
                            frame
                            originalCar
                            remainingWheels
                            (Body.applyImpulse
                                suspensionImpulse
                                contactNormal
                                contactPoint
                                updatedCar
                            )
                            ({ wheel
                                | contact =
                                    Just
                                        { point = contactPoint
                                        , normal = contactNormal
                                        , body = body
                                        }
                                , suspensionLength = suspensionLength
                                , suspensionImpulse = suspensionImpulse
                             }
                                :: updatedWheels
                            )
                            (numWheelsOnGround + 1)

                    else
                        updateSuspension carSettings
                            dt
                            world
                            frame
                            originalCar
                            remainingWheels
                            updatedCar
                            ({ wheel
                                | contact = Nothing
                                , suspensionLength = carSettings.suspensionRestLength
                             }
                                :: updatedWheels
                            )
                            numWheelsOnGround

                Nothing ->
                    updateSuspension carSettings
                        dt
                        world
                        frame
                        originalCar
                        remainingWheels
                        updatedCar
                        ({ wheel
                            | contact = Nothing
                            , suspensionLength = carSettings.suspensionRestLength
                         }
                            :: updatedWheels
                        )
                        numWheelsOnGround


type alias WheelFriction id =
    { forward : Direction3d WorldCoordinates
    , axle : Direction3d WorldCoordinates
    , sideImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , forwardImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , skidInfo : Float
    , contactPoint : Point3d Meters WorldCoordinates
    , contactBody : Body id
    }


updateFriction : CarSettings -> Duration -> World id -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body id -> Int -> List (Wheel id) -> List (WheelFriction id) -> List (Wheel id) -> Bool -> ( Body id, List (Wheel id) )
updateFriction carSettings dt world frame updatedCar numWheelsOnGround currentWheels wheelFrictions updatedWheels sliding =
    case currentWheels of
        [] ->
            applyImpulses carSettings dt world frame updatedCar updatedWheels sliding wheelFrictions

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal, body } ->
                    let
                        worldAxle =
                            carSettings.rightDirection
                                |> Direction3d.rotateAround (Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection) wheel.steering
                                |> Direction3d.placeIn frame

                        (Quantity proj) =
                            Vector3d.dot (Direction3d.toVector normal) (Direction3d.toVector worldAxle)

                        axle =
                            Direction3d.toVector worldAxle
                                |> Vector3d.minus (Vector3d.scaleBy proj (Direction3d.toVector normal))
                                |> Vector3d.direction
                                |> Maybe.withDefault normal

                        forward =
                            Vector3d.cross (Direction3d.toVector normal) (Direction3d.toVector axle)
                                |> Vector3d.direction
                                |> Maybe.withDefault normal

                        sideImpulse =
                            resolveSingleBilateral updatedCar body point axle

                        maxImpulse =
                            if wheel.brake == Quantity.zero then
                                -- TODO: think about default rolling friction impulse
                                Quantity.zero

                            else
                                Quantity.times dt wheel.brake

                        forwardImpulse =
                            Quantity.times dt wheel.engineForce
                                |> Quantity.plus (calcRollingFriction updatedCar body point forward maxImpulse numWheelsOnGround)

                        -- Switch between active rolling (throttle), braking and non-active rolling friction (nthrottle/break)
                        maximpSide =
                            Quantity.multiplyBy carSettings.frictionSlip wheel.suspensionImpulse

                        impulseSquared =
                            Quantity.times forwardImpulse forwardImpulse
                                |> Quantity.multiplyBy 0.25
                                |> Quantity.plus (Quantity.times sideImpulse sideImpulse)

                        isSliding =
                            Quantity.greaterThan (Quantity.times maximpSide maximpSide) impulseSquared

                        skidInfo =
                            if isSliding then
                                Quantity.ratio maximpSide (Quantity.sqrt impulseSquared)

                            else
                                1
                    in
                    updateFriction carSettings
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        ({ forward = forward
                         , axle = axle
                         , sideImpulse = sideImpulse
                         , forwardImpulse = forwardImpulse
                         , skidInfo = skidInfo
                         , contactPoint = point
                         , contactBody = body
                         }
                            :: wheelFrictions
                        )
                        (wheel :: updatedWheels)
                        (sliding || isSliding)

                Nothing ->
                    updateFriction carSettings
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        wheelFrictions
                        (wheel :: updatedWheels)
                        sliding


applyImpulses : CarSettings -> Duration -> World id -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body id -> List (Wheel id) -> Bool -> List (WheelFriction id) -> ( Body id, List (Wheel id) )
applyImpulses carSettings dt world frame carBody wheels sliding wheelFrictions =
    case wheelFrictions of
        [] ->
            rotateWheels carSettings dt frame carBody wheels []

        friction :: remainingFrictions ->
            let
                centerOfMass =
                    Body.centerOfMass carBody
                        |> Point3d.placeIn frame

                up =
                    Direction3d.reverse carSettings.downDirection
                        |> Direction3d.placeIn frame

                verticalDistance =
                    Vector3d.from friction.contactPoint centerOfMass
                        |> Vector3d.componentIn up
                        |> Quantity.multiplyBy (1 - carSettings.rollInfluence)

                closerToCenterOfMass =
                    Point3d.translateIn up verticalDistance friction.contactPoint

                forwardImpulse =
                    if sliding then
                        Quantity.multiplyBy friction.skidInfo friction.forwardImpulse

                    else
                        friction.forwardImpulse

                sideImpulse =
                    if sliding then
                        Quantity.multiplyBy friction.skidInfo friction.sideImpulse

                    else
                        friction.sideImpulse

                newCar =
                    carBody
                        |> Body.applyImpulse forwardImpulse friction.forward friction.contactPoint
                        |> Body.applyImpulse sideImpulse friction.axle closerToCenterOfMass

                -- TODO: apply the reverse of the sideImpulse on the ground object too, for now assume it is static
            in
            applyImpulses carSettings
                dt
                world
                frame
                newCar
                wheels
                sliding
                remainingFrictions


rotateWheels : CarSettings -> Duration -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body id -> List (Wheel id) -> List (Wheel id) -> ( Body id, List (Wheel id) )
rotateWheels carSettings dt frame carBody wheels updatedWheels =
    case wheels of
        [] ->
            ( carBody, List.reverse updatedWheels )

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal } ->
                    let
                        velocity =
                            Body.velocityAt point carBody

                        forward =
                            Direction3d.placeIn frame carSettings.forwardDirection

                        proj =
                            Direction3d.componentIn normal forward

                        (Quantity proj2) =
                            forward
                                |> Direction3d.toVector
                                |> Vector3d.minus (Vector3d.withLength (Quantity proj) normal)
                                |> Vector3d.dot velocity

                        deltaRotation =
                            Quantity (proj2 * Duration.inSeconds dt / Length.inMeters carSettings.radius)

                        newWheel =
                            { wheel
                                | deltaRotation = deltaRotation
                                , rotation = Quantity.plus wheel.rotation wheel.deltaRotation
                            }
                    in
                    rotateWheels carSettings dt frame carBody remainingWheels (newWheel :: updatedWheels)

                Nothing ->
                    let
                        deltaRotation =
                            Quantity.multiplyBy 0.99 wheel.deltaRotation

                        newWheel =
                            { wheel
                              -- damping when not in contact
                                | deltaRotation = Quantity.multiplyBy 0.99 wheel.deltaRotation
                                , rotation = Quantity.plus wheel.rotation deltaRotation
                            }
                    in
                    rotateWheels carSettings dt frame carBody remainingWheels (newWheel :: updatedWheels)


resolveSingleBilateral : Body id -> Body id -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
resolveSingleBilateral body1 body2 point direction =
    let
        velocity1 =
            Body.velocityAt point body1

        velocity2 =
            Body.velocityAt point body2

        (Quantity relativeVelocity) =
            Vector3d.dot (Vector3d.minus velocity2 velocity1) (Direction3d.toVector direction)

        contactDamping =
            0.2

        invMass1 =
            case Body.mass body1 of
                Just mass ->
                    1 / Mass.inKilograms mass

                Nothing ->
                    0

        invMass2 =
            case Body.mass body2 of
                Just mass ->
                    1 / Mass.inKilograms mass

                Nothing ->
                    0

        massTerm =
            1 / (invMass1 + invMass2)
    in
    Quantity (-contactDamping * relativeVelocity * massTerm)


calcRollingFriction : Body id -> Body id -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds) -> Int -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
calcRollingFriction body1 body2 point forward maxImpulse numWheelsOnGround =
    let
        velocity1 =
            Body.velocityAt point body1

        velocity2 =
            Body.velocityAt point body2

        (Quantity relativeVelocity) =
            Vector3d.dot (Vector3d.minus velocity2 velocity1) (Direction3d.toVector forward)

        denom1 =
            computeImpulseDenominator body1 point forward

        denom2 =
            computeImpulseDenominator body2 point forward
    in
    Quantity (-relativeVelocity / (denom1 + denom2) / toFloat numWheelsOnGround)
        |> Quantity.clamp (Quantity.negate maxImpulse) maxImpulse


computeImpulseDenominator : Body id -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Float
computeImpulseDenominator body point normal =
    let
        position =
            Point3d.placeIn (Body.frame body) (Body.centerOfMass body)

        r0 =
            Vector3d.from position point

        c0 =
            Vector3d.cross r0 (Direction3d.toVector normal)

        vec =
            Vector3d.cross (Body.transformWithInverseInertia body c0) r0

        (Quantity dot) =
            Vector3d.dot (Direction3d.toVector normal) vec
    in
    case Body.mass body of
        Just mass ->
            1 / Mass.inKilograms mass + dot

        Nothing ->
            dot


defaultWheel : Wheel id
defaultWheel =
    { chassisConnectionPoint = Point3d.origin -- set for different wheels
    , axis = Axis3d.x -- set for different wheels
    , steering = Quantity.zero
    , rotation = Quantity.zero
    , deltaRotation = Quantity.zero
    , suspensionImpulse = Quantity.zero
    , suspensionLength = Quantity.zero
    , engineForce = Quantity.zero
    , brake = Quantity.zero
    , contact = Nothing
    }
