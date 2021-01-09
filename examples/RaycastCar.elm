module RaycastCar exposing (main)

{-| This is a complex example implementing raycast vehicle enspired by:

  - bullet3: <https://github.com/bulletphysics/bullet3/blob/master/src/BulletDynamics/Vehicle/btRaycastVehicle.cpp>
  - cannon.js: <https://github.com/schteppe/cannon.js/blob/master/src/objects/RaycastVehicle.js>

This is a car with fake wheels, that shoots rays and applies
impulses to the car body where the rays hit the ground.

This allows to simulate suspension and results in smooth behavior.

-}

import Acceleration
import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Events
import Common.Camera as Camera exposing (Camera)
import Common.Events as Events
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Meshes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode
import Length exposing (Length, Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material as Material
import Physics.World as World exposing (World)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Sphere3d exposing (Sphere3d)
import Vector3d


{-| Give a name to each body, so that we can configure constraints
-}
type alias Data =
    { meshes : Meshes
    , id : Id
    }


type Id
    = Obstacle
    | Car (List Wheel)


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
    , customSlidingRotationalSpeed : Maybe Float
    }


carSettings : CarSettings
carSettings =
    { downDirection = Direction3d.negativeZ
    , rightDirection = Direction3d.y
    , forwardDirection = Direction3d.x
    , suspensionRestLength = Length.meters 0.3
    , minSuspensionLength = Length.meters 0
    , maxSuspensionLength = Length.meters 0.6
    , radius = Length.meters 0.5
    , suspensionStiffness = 30
    , dampingCompression = 4.4
    , dampingRelaxation = 2.3
    , frictionSlip = 5
    , rollInfluence = 0.01
    , maxSuspensionForce = Force.newtons 100000
    , customSlidingRotationalSpeed = Just -30
    }


type alias Wheel =
    { chassisConnectionPoint : Point3d Meters BodyCoordinates
    , steering : Angle
    , rotation : Angle
    , deltaRotation : Angle
    , suspensionImpulse :
        Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , suspensionLength : Length
    , engineForce : Force
    , brake : Force
    , contact :
        Maybe
            { point : Point3d Meters WorldCoordinates
            , normal : Direction3d WorldCoordinates
            , body : Body Data
            }
    }


defaultWheel : Wheel
defaultWheel =
    { chassisConnectionPoint = Point3d.origin -- set for different wheels
    , steering = Quantity.zero
    , rotation = Quantity.zero
    , deltaRotation = Quantity.zero
    , suspensionImpulse = Quantity.zero
    , suspensionLength = Quantity.zero
    , engineForce = Quantity.zero
    , brake = Quantity.zero
    , contact = Nothing
    }


type alias Model =
    { world : World Data
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , speeding : Float
    , steering : Float
    , braking : Bool
    }


type Command
    = Speed Float
    | Steer Float
    | Brake


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "ArrowRight" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "ArrowUp" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "ArrowDown" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    "b" ->
                        Json.Decode.succeed (toMsg Brake)

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart
    | KeyDown Command
    | KeyUp Command


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , fps = []
      , settings = settings
      , speeding = 0
      , steering = 0
      , braking = False
      , camera =
            Camera.camera
                { from = { x = -40, y = 40, z = 30 }
                , to = { x = 0, y = -7, z = 0 }
                }
      }
    , Events.measureSize Resize
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ForSettings settingsMsg ->
            { model
                | settings = Settings.update settingsMsg model.settings
            }

        Tick dt ->
            { model
                | fps = Fps.update dt model.fps
                , world =
                    model.world
                        |> World.update
                            (\body ->
                                case (Body.data body).id of
                                    Car wheels ->
                                        simulateCar (Duration.seconds (1 / 60)) model wheels body

                                    _ ->
                                        body
                            )
                        |> World.simulate (Duration.seconds (1 / 60))
            }

        Resize width height ->
            { model | camera = Camera.resize width height model.camera }

        Restart ->
            { model | world = initialWorld }

        KeyDown (Steer k) ->
            { model | steering = k }

        KeyDown (Speed k) ->
            { model | speeding = k }

        KeyUp (Steer k) ->
            { model
                | steering =
                    if k == model.steering then
                        0

                    else
                        model.steering
            }

        KeyUp (Speed k) ->
            { model
                | speeding =
                    if k == model.speeding then
                        0

                    else
                        model.speeding
            }

        KeyDown Brake ->
            { model | braking = True }

        KeyUp Brake ->
            { model | braking = False }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


view : Model -> Html Msg
view { settings, fps, world, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , world = addWheelsToWorld world
            , camera = camera
            , meshes = .meshes
            , maybeRaycastResult = Nothing
            , floorOffset = floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length (World.bodies world))

          else
            Html.text ""
        ]


addWheelsToWorld : World Data -> World Data
addWheelsToWorld world =
    let
        maybeCar =
            world
                |> World.bodies
                |> List.filterMap
                    (\b ->
                        case (Body.data b).id of
                            Car wheels ->
                                Just ( wheels, b )

                            _ ->
                                Nothing
                    )
                |> List.head
    in
    case maybeCar of
        Just ( wheels, car ) ->
            List.foldl
                (\wheel ->
                    let
                        frame =
                            Body.frame car

                        position =
                            wheel.chassisConnectionPoint
                                |> Point3d.placeIn (Body.frame car)

                        downDirection =
                            carSettings.downDirection
                                |> Direction3d.placeIn (Body.frame car)

                        rightDirection =
                            carSettings.rightDirection
                                |> Direction3d.placeIn (Body.frame car)

                        newPosition =
                            position |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                        newFrame =
                            frame
                                |> Frame3d.moveTo newPosition
                                |> Frame3d.rotateAround (Axis3d.through newPosition downDirection) wheel.steering
                                |> Frame3d.rotateAround (Axis3d.through newPosition rightDirection) wheel.rotation
                    in
                    World.add
                        (Body.sphere wheelShape
                            { id = Obstacle
                            , meshes = wheelMesh
                            }
                            |> Body.placeIn newFrame
                        )
                )
                world
                wheels

        Nothing ->
            world


initialWorld : World Data
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        |> World.add slope
        |> World.add (box (Point3d.meters 15 -15 -0.5))
        |> World.add (box (Point3d.meters 15 -16.5 -0.5))
        |> World.add (box (Point3d.meters 15 -18 -0.5))
        |> World.add (box (Point3d.meters 15 -16 0.5))
        |> World.add (box (Point3d.meters 15 -17.5 0.5))
        |> World.add (box (Point3d.meters 15 -16.5 1.5))
        |> World.add (Body.moveTo (Point3d.meters 0 0 5) base)


simulateCar : Duration -> Model -> List Wheel -> Body Data -> Body Data
simulateCar dt { world, steering, braking, speeding } wheels car =
    case wheels of
        [ w1, w2, w3, w4 ] ->
            let
                engineForce =
                    Force.newtons (500 * speeding)

                brake =
                    if braking then
                        Force.newtons 1000

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
            updateSuspension dt world (Body.frame car) car [ wheel1, wheel2, wheel3, wheel4 ] car [] 0

        _ ->
            car


updateSuspension : Duration -> World Data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> List Wheel -> Body Data -> List Wheel -> Int -> Body Data
updateSuspension dt world frame originalCar currentWheels updatedCar updatedWheels numWheelsOnGround =
    case currentWheels of
        [] ->
            updateFriction dt world frame updatedCar numWheelsOnGround updatedWheels [] [] False

        wheel :: remainingWheels ->
            let
                ray =
                    Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection
                        |> Axis3d.placeIn frame
            in
            case World.raycast ray (World.keepIf (\b -> (Body.data b).id == Obstacle) world) of
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
                        updateSuspension dt
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
                        updateSuspension dt
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
                    updateSuspension dt
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


type alias WheelFriction =
    { forward : Direction3d WorldCoordinates
    , axle : Direction3d WorldCoordinates
    , sideImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , forwardImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , skidInfo : Float
    , contactPoint : Point3d Meters WorldCoordinates
    , contactBody : Body Data
    }


updateFriction : Duration -> World Data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> Int -> List Wheel -> List WheelFriction -> List Wheel -> Bool -> Body Data
updateFriction dt world frame updatedCar numWheelsOnGround currentWheels wheelFrictions updatedWheels sliding =
    case currentWheels of
        [] ->
            applyImpulses dt world frame updatedCar updatedWheels sliding wheelFrictions

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
                    updateFriction
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
                    updateFriction
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        wheelFrictions
                        (wheel :: updatedWheels)
                        sliding


applyImpulses : Duration -> World Data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> List Wheel -> Bool -> List WheelFriction -> Body Data
applyImpulses dt world frame car wheels sliding wheelFrictions =
    case wheelFrictions of
        [] ->
            rotateWheels dt frame car wheels []

        friction :: remainingFrictions ->
            let
                centerOfMass =
                    Body.centerOfMass car
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
                    car
                        |> Body.applyImpulse forwardImpulse friction.forward friction.contactPoint
                        |> Body.applyImpulse sideImpulse friction.axle closerToCenterOfMass

                -- TODO: apply the reverse of the sideImpulse on the ground object too, for now assume it is static
            in
            applyImpulses
                dt
                world
                frame
                newCar
                wheels
                sliding
                remainingFrictions


rotateWheels : Duration -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> List Wheel -> List Wheel -> Body Data
rotateWheels dt frame car wheels updatedWheels =
    case wheels of
        [] ->
            Body.withData
                { id = Car (List.reverse updatedWheels)
                , meshes = (Body.data car).meshes
                }
                car

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal } ->
                    let
                        velocity =
                            Body.velocityAt point car

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
                    rotateWheels dt frame car remainingWheels (newWheel :: updatedWheels)

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
                    rotateWheels dt frame car remainingWheels (newWheel :: updatedWheels)


resolveSingleBilateral : Body Data -> Body Data -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
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


calcRollingFriction : Body Data -> Body Data -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds) -> Int -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
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


computeImpulseDenominator : Body Data -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Float
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


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body Data
floor =
    Body.plane { id = Obstacle, meshes = Meshes.fromTriangles [] }
        |> Body.moveTo (Point3d.fromMeters floorOffset)


{-| A slope to give a car the initial push.
-}
slope : Body Data
slope =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )
    in
    Body.block block3d
        { id = Obstacle
        , meshes = Meshes.fromTriangles (Meshes.block block3d)
        }
        |> Body.rotateAround Axis3d.x (Angle.radians (pi / 16))
        |> Body.moveTo (Point3d.meters 0 -2 0.5)


box : Point3d Meters WorldCoordinates -> Body Data
box position =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1
                , Length.meters 1
                , Length.meters 1
                )
    in
    Body.block block3d
        { id = Obstacle
        , meshes = Meshes.fromTriangles (Meshes.block block3d)
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 10))
        |> Body.moveTo position


wheelShape : Sphere3d Meters BodyCoordinates
wheelShape =
    Sphere3d.atOrigin carSettings.radius


wheelMesh : Meshes
wheelMesh =
    Meshes.fromTriangles (Meshes.sphere 2 wheelShape)


base : Body Data
base =
    let
        block =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 4, Length.meters 2, Length.meters 1 )

        wheels =
            [ { defaultWheel | chassisConnectionPoint = Point3d.meters 1 1 0 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters 1 -1 0 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters -1 1 0 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters -1 -1 0 }
            ]

        -- We don't want the car to stuck when bumping into the obstacles
        slippy =
            Material.custom { friction = -1, bounciness = 0.6 }
    in
    Body.block
        block
        { id = Car wheels
        , meshes = Meshes.fromTriangles (Meshes.block block)
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 150))
        |> Body.withMaterial slippy
