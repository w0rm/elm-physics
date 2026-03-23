module RaycastCar exposing (main)

{-| This demo implements a smooth car simulation.

  - `RaycastCar.Car` — raycast vehicle simulation algorithm,
  - `RaycastCar.Jeep` — load the 3d model using elm-obj-file.

Press arrow keys to drive, "b" to brake.

Try to add more obstacles to the demo by changing the initialBodies,
or tweak the settings in the `RaycastCar.Jeep` module!

-}

import Angle
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Meters)
import Mass
import Physics exposing (Body, onEarth)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material
import Physics.Shape
import Pixels exposing (Pixels)
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import RaycastCar.Car as Car exposing (Wheel)
import RaycastCar.Jeep as Jeep exposing (Jeep)
import Scene3d exposing (Entity)
import Scene3d.Material
import Task
import Vector3d


type Id
    = Ramp
    | Crate
    | Floor
    | Car (List (Wheel Id))


type alias Model =
    { dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , bodies : List ( Id, Body )
    , jeep : Maybe Jeep
    , speeding : Float
    , steering : Float
    , braking : Bool
    }


type Msg
    = Tick
    | Resize Int Int
    | KeyDown Command
    | KeyUp Command
    | JeepLoaded (Result String Jeep)


type Command
    = Speed Float
    | Steer Float
    | Brake


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dimensions = ( Pixels.int 0, Pixels.int 0 )
      , bodies = initialBodies
      , jeep = Nothing
      , speeding = 0
      , steering = 0
      , braking = False
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize (round viewport.width) (round viewport.height)
            )
            Browser.Dom.getViewport
        , Jeep.load { texture = "Jeep.png", mesh = "Jeep.obj.txt" } |> Task.attempt JeepLoaded
        ]
    )


initialBodies : List ( Id, Body )
initialBodies =
    ( Floor, Physics.plane Physics.Material.wood )
        :: ( Ramp
           , Physics.static [ ( Physics.Shape.block rampBlock, Physics.Material.wood ) ]
                |> Physics.rotateAround Axis3d.x (Angle.radians (pi / 16))
                |> Physics.moveTo (Point3d.meters 0 -2 1.5)
           )
        :: List.map crate
            [ Point3d.meters 15 -15 0.5
            , Point3d.meters 15 -16.5 0.5
            , Point3d.meters 15 -18 0.5
            , Point3d.meters 15 -16 1.5
            , Point3d.meters 15 -17.5 1.5
            , Point3d.meters 15 -16.5 2.5
            ]


rampBlock : Block3d Meters BodyCoordinates
rampBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 10
        , Length.meters 16
        , Length.meters 0.5
        )


crateBlock : Block3d Meters BodyCoordinates
crateBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 1
        , Length.meters 1
        , Length.meters 1
        )


crate : Point3d Meters WorldCoordinates -> ( Id, Body )
crate position =
    let
        innerBlock =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 0.98
                , Length.meters 0.98
                , Length.meters 0.98
                )
    in
    ( Crate
    , Physics.dynamic
        [ ( Physics.Shape.block crateBlock
                |> Physics.Shape.minus (Physics.Shape.block innerBlock)
          , Physics.Material.wood
          )
        ]
        |> Physics.moveTo position
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        JeepLoaded result ->
            case result of
                Ok jeep ->
                    { model
                        | jeep = Just jeep
                        , bodies =
                            ( Car (Jeep.wheels jeep)
                            , Physics.dynamic jeep.collider
                                |> Physics.scaleTo (Mass.kilograms 4000)
                                |> Physics.moveTo (Point3d.meters 0 0 6)
                            )
                                :: model.bodies
                    }

                Err _ ->
                    model

        Tick ->
            case model.jeep of
                Just loadedJeep ->
                    let
                        updatedBodies =
                            simulateCar model loadedJeep

                        ( simulated, _ ) =
                            Physics.simulate onEarth updatedBodies
                    in
                    { model | bodies = simulated }

                Nothing ->
                    model

        Resize width height ->
            { model | dimensions = ( Pixels.pixels width, Pixels.pixels height ) }

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


view : Model -> Html Msg
view { bodies, jeep, dimensions } =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.positiveZ
            , sunlightDirection = Direction3d.xyZ (Angle.degrees -15) (Angle.degrees -45)
            , shadows = True
            , camera = camera
            , dimensions = dimensions
            , background = Scene3d.transparentBackground
            , clipDepth = Length.meters 0.1
            , entities =
                case jeep of
                    Just loadedJeep ->
                        List.map (bodyToEntity loadedJeep) bodies

                    Nothing ->
                        []
            }
        ]


simulateCar : Model -> Jeep -> List ( Id, Body )
simulateCar model jeep =
    let
        notACar ( id, _ ) =
            case id of
                Car _ ->
                    False

                _ ->
                    True
    in
    List.map
        (\(( bodyId, body ) as passThrough) ->
            case bodyId of
                Car wheels ->
                    let
                        ( newBody, newWheels ) =
                            Car.simulate
                                { duration = onEarth.duration
                                , bodiesWithoutCar = List.filter notACar model.bodies
                                , speeding = model.speeding
                                , steering = model.steering
                                , braking = model.braking
                                , carSettings = Jeep.settings jeep
                                }
                                wheels
                                body
                    in
                    ( Car newWheels, newBody )

                _ ->
                    passThrough
        )
        model.bodies


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.lookAt
        { eyePoint = Point3d.meters -40 40 30
        , focalPoint = Point3d.meters 0 -7 0
        , upDirection = Direction3d.positiveZ
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 24)
        }


bodyToEntity : Jeep -> ( Id, Body ) -> Entity WorldCoordinates
bodyToEntity jeep ( id, body ) =
    Scene3d.placeIn (Physics.frame body) <|
        case id of
            Floor ->
                Scene3d.quad (Scene3d.Material.matte Color.white)
                    (Point3d.meters -100 -100 0)
                    (Point3d.meters -100 100 0)
                    (Point3d.meters 100 100 0)
                    (Point3d.meters 100 -100 0)

            Ramp ->
                Scene3d.blockWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = Color.lightGray
                        , roughness = 1
                        }
                    )
                    rampBlock

            Crate ->
                Scene3d.blockWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = Color.orange
                        , roughness = 1
                        }
                    )
                    crateBlock

            Car wheels ->
                let
                    { downDirection, rightDirection } =
                        Jeep.settings jeep
                in
                Scene3d.group
                    (List.foldl
                        (\wheel entities ->
                            let
                                axisDirection =
                                    Axis3d.direction wheel.axis

                                applyMirror =
                                    if Quantity.greaterThan Quantity.zero (Direction3d.angleFrom rightDirection axisDirection) then
                                        identity

                                    else
                                        Frame3d.mirrorAcross Plane3d.yz

                                wheelPosition =
                                    wheel.chassisConnectionPoint
                                        |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                                wheelFrame =
                                    Frame3d.atOrigin
                                        |> applyMirror
                                        |> Frame3d.rotateAround (Axis3d.through Point3d.origin (Direction3d.reverse rightDirection)) wheel.rotation
                                        |> Frame3d.rotateAround (Axis3d.through Point3d.origin downDirection) wheel.steering
                                        |> Frame3d.moveTo wheelPosition
                            in
                            (Scene3d.meshWithShadow jeep.material
                                jeep.wheel
                                jeep.wheelShadow
                                |> Scene3d.placeIn wheelFrame
                            )
                                :: entities
                        )
                        [ Scene3d.meshWithShadow jeep.material
                            jeep.chassis
                            jeep.chassisShadow
                        ]
                        wheels
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrameDelta (\_ -> Tick)
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.andThen
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
        (Json.Decode.field "key" Json.Decode.string)
