module Car exposing (main)

{-| This shows how hinge constrains can be used to assemble a car.
Use the arrow keys to steer and speed!
-}

import Acceleration
import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Direction3d
import Duration
import Force
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode
import Length exposing (Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape as Shape
import Physics.World as World exposing (World)
import Point3d exposing (Point3d)
import Sphere3d
import Task
import Vector3d
import WebGL exposing (Mesh)


{-| Give a name to each body, so that we can configure constraints
-}
type alias Data =
    { mesh : Mesh Attributes
    , name : String
    }


type alias Model =
    { world : World Data
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , speeding : Float -- -1, 0, 1
    , steering : Float -- -1, 0, 1
    }


type Command
    = Speed Float
    | Steer Float


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
      , camera =
            Camera.camera
                { from = { x = -60, y = 60, z = 40 }
                , to = { x = 0, y = -7, z = 0 }
                }
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
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
                    let
                        baseFrame =
                            model.world
                                |> World.bodies
                                |> List.filter (\b -> (Body.data b).name == "base")
                                |> List.head
                                |> Maybe.map (\b -> Body.frame b)
                                |> Maybe.withDefault Frame3d.atOrigin
                    in
                    model.world
                        |> World.constrain (constrainCar model.steering)
                        |> World.update (applySpeed model.speeding baseFrame)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        ]


view : Model -> Html Msg
view { settings, fps, world, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , world = world
            , camera = camera
            , mesh = .mesh
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


initialWorld : World Data
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        |> World.add slope
        |> addCar (Point3d.meters 0 0 5)


addCar : Point3d Meters WorldCoordinates -> World Data -> World Data
addCar offset world =
    world
        |> World.add (Body.moveTo offset base)
        |> World.add
            (wheel "wheel1"
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters 3 3 0)
            )
        |> World.add
            (wheel "wheel2"
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters -3 3 0)
            )
        |> World.add
            (wheel "wheel3"
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters -3 -3 0)
            )
        |> World.add
            (wheel "wheel4"
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters 3 -3 0)
            )


applySpeed : Float -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> Body Data
applySpeed speed baseFrame body =
    if speed /= 0 && ((Body.data body).name == "wheel1" || (Body.data body).name == "wheel2") then
        let
            forward =
                Frame3d.yDirection baseFrame

            up =
                Frame3d.zDirection baseFrame

            wheelPoint =
                Frame3d.originPoint (Body.frame body)

            pointOnTheWheel =
                wheelPoint |> Point3d.translateBy (Vector3d.withLength (Length.meters 1.2) up)

            pointUnderTheWheel =
                wheelPoint |> Point3d.translateBy (Vector3d.withLength (Length.meters 1.2) (Direction3d.reverse up))
        in
        body
            |> Body.applyForce (Force.newtons (-speed * 100)) forward pointOnTheWheel
            |> Body.applyForce (Force.newtons (-speed * 100)) (Direction3d.reverse forward) pointUnderTheWheel

    else
        body


constrainCar : Float -> Body Data -> Body Data -> List Constraint
constrainCar steering b1 b2 =
    let
        steeringAngle =
            steering * pi / 8

        dx =
            cos steeringAngle

        dy =
            sin steeringAngle

        hinge1 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 3 3 0)
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )
                (Axis3d.through
                    (Point3d.meters 0 0 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )

        hinge2 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -3 3 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge3 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -3 -3 0)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge4 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 3 -3 0)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
    in
    case ( (Body.data b1).name, (Body.data b2).name ) of
        ( "base", "wheel1" ) ->
            [ hinge1 ]

        ( "base", "wheel2" ) ->
            [ hinge2 ]

        ( "base", "wheel3" ) ->
            [ hinge3 ]

        ( "base", "wheel4" ) ->
            [ hinge4 ]

        _ ->
            []


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body Data
floor =
    Body.plane { name = "floor", mesh = Meshes.fromTriangles [] }
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
        { name = "slope"
        , mesh = Meshes.fromTriangles (Meshes.block block3d)
        }
        |> Body.rotateAround Axis3d.x (Angle.radians (pi / 16))
        |> Body.moveTo (Point3d.meters 0 -2 1)


base : Body Data
base =
    let
        bottom =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 3, Length.meters 6, Length.meters 1 )

        top =
            Block3d.centeredOn
                (Frame3d.atPoint (Point3d.meters 0 1 1))
                ( Length.meters 2, Length.meters 3, Length.meters 1.5 )
    in
    Body.compound
        [ Shape.block top, Shape.block bottom ]
        { name = "base"
        , mesh = Meshes.fromTriangles (Meshes.block bottom ++ Meshes.block top)
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 80))


wheel : String -> Body Data
wheel name =
    let
        sphere =
            Sphere3d.atOrigin (Length.meters 1.2)
    in
    Body.sphere sphere
        { name = name
        , mesh = Meshes.fromTriangles (Meshes.sphere 2 sphere)
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 2))
