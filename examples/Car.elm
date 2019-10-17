module Car exposing (main)

{-| This shows how hinge constrains can be used to assemble a car.
-}

import Angle
import Axis3d
import Browser
import Common.Camera as Camera exposing (Camera)
import Common.Events as Events
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Meshes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World as World exposing (World)
import Point3d exposing (Point3d)
import Vector3d


{-| Give a name to each body, so that we can configure constraints
-}
type alias Data =
    { meshes : Meshes
    , name : String
    }


type alias Model =
    { world : World Data
    , fps : List Float
    , settings : Settings
    , camera : Camera
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , fps = []
      , settings = settings
      , camera =
            Camera.camera
                { from = { x = -30, y = 30, z = 20 }
                , to = { x = 0, y = -7, z = 0 }
                }
      }
    , Events.measureSize Resize
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model
                | settings = Settings.update settingsMsg model.settings
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | fps = Fps.update dt model.fps
                , world =
                    model.world
                        |> World.constrain constrainCar
                        |> World.simulate (1000 / 60)
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model | world = initialWorld }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, world, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , world = world
            , camera = camera
            , meshes = .meshes
            , raycastResult = Nothing
            , floorOffset = Just floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length (World.getBodies world))

          else
            Html.text ""
        ]


initialWorld : World Data
initialWorld =
    World.empty
        |> World.setGravity { x = 0, y = 0, z = -10 }
        |> World.add floor
        |> World.add slope
        |> addCar (Point3d.fromMeters { x = 0, y = 0, z = 5 })


addCar : Point3d Meters WorldCoordinates -> World Data -> World Data
addCar offset world =
    let
        originFrame =
            Frame3d.atPoint offset
    in
    world
        |> World.add (Body.setFrame3d originFrame base)
        |> World.add
            (wheel "wheel1"
                |> Body.setFrame3d
                    (Frame3d.translateBy
                        (Vector3d.fromMeters { x = 3, y = 3, z = 0 })
                        originFrame
                    )
            )
        |> World.add
            (wheel "wheel2"
                |> Body.setFrame3d
                    (Frame3d.translateBy
                        (Vector3d.fromMeters { x = -3, y = 3, z = 0 })
                        originFrame
                    )
            )
        |> World.add
            (wheel "wheel3"
                |> Body.setFrame3d
                    (Frame3d.translateBy
                        (Vector3d.fromMeters { x = -3, y = -3, z = 0 })
                        originFrame
                    )
            )
        |> World.add
            (wheel "wheel4"
                |> Body.setFrame3d
                    (Frame3d.translateBy
                        (Vector3d.fromMeters { x = 3, y = -3, z = 0 })
                        originFrame
                    )
            )


constrainCar : Body Data -> Body Data -> List Constraint
constrainCar b1 b2 =
    let
        steeringAngle =
            0

        dx =
            cos steeringAngle

        dy =
            sin steeringAngle

        hinge1 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.fromMeters { x = 3, y = 3, z = 0 })
                    (Direction3d.unsafe { x = dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    (Point3d.fromMeters { x = 0, y = 0, z = 0 })
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )

        hinge2 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.fromMeters { x = -3, y = 3, z = 0 })
                    (Direction3d.unsafe { x = -dx, y = -dy, z = 0 })
                )
                (Axis3d.through
                    (Point3d.fromMeters { x = 0, y = 0, z = 0 })
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge3 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.fromMeters { x = -3, y = -3, z = 0 })
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
                (Axis3d.through
                    (Point3d.fromMeters { x = 0, y = 0, z = 0 })
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge4 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.fromMeters { x = 3, y = -3, z = 0 })
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )
                (Axis3d.through
                    (Point3d.fromMeters { x = 0, y = 0, z = 0 })
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
    in
    case ( (Body.getData b1).name, (Body.getData b2).name ) of
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
    Body.plane { name = "floor", meshes = Meshes.fromTriangles [] }
        |> Body.setFrame3d (Frame3d.atPoint (Point3d.fromMeters floorOffset))


{-| A slope to give a car the initial push.
-}
slope : Body Data
slope =
    let
        size =
            { x = 10, y = 16, z = 0.5 }

        meshes =
            Meshes.fromTriangles (Meshes.box size)
    in
    { name = "slope", meshes = meshes }
        |> Body.block (Length.meters size.x) (Length.meters size.y) (Length.meters size.z)
        |> Body.setFrame3d
            (Frame3d.atPoint Point3d.origin
                |> Frame3d.rotateAround Axis3d.x (Angle.radians (pi / 16))
                |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = -2, z = 1 })
            )


base : Body Data
base =
    let
        size =
            { x = 3, y = 6, z = 1 }

        meshes =
            Meshes.fromTriangles (Meshes.box size)
    in
    { name = "base", meshes = meshes }
        |> Body.block (Length.meters size.x) (Length.meters size.y) (Length.meters size.z)
        |> Body.setMass (Mass.kilograms 1)


wheel : String -> Body Data
wheel name =
    let
        radius =
            1.2

        meshes =
            Meshes.fromTriangles (Meshes.sphere 2 radius)
    in
    { name = name, meshes = meshes }
        |> Body.sphere (Length.meters radius)
        |> Body.setMass (Mass.kilograms 1)
