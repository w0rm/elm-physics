module CollisionTest exposing (main)

{-| This tests collisions between boxes and spheres.
Note that spheres don’t move, that’s because they have zero mass.
-}

import Acceleration
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
import Duration
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length
import Mass
import Physics.Body as Body exposing (Body)
import Physics.World as World exposing (World)
import Point3d


type alias Model =
    { world : World Meshes
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
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
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
                , world = World.simulate (Duration.seconds (1 / 60)) model.world
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
            , meshes = identity
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


initialWorld : World Meshes
initialWorld =
    World.empty
        |> World.setGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        -- corner:
        |> World.add sphere
        |> World.add
            (box
                |> Body.setFrame3d
                    (Frame3d.atPoint Point3d.origin
                        |> Frame3d.rotateAround
                            (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 }))
                            (Angle.radians (pi / 3))
                        |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 10 })
                    )
            )
        -- edge:
        |> World.add (Body.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = 4, y = 0, z = 0 })) sphere)
        |> World.add
            (box
                |> Body.setFrame3d
                    (Frame3d.atPoint Point3d.origin
                        |> Frame3d.rotateAround Axis3d.x (Angle.radians (pi / 3))
                        |> Frame3d.moveTo (Point3d.fromMeters { x = 4, y = 0, z = 10 })
                    )
            )
        -- face:
        |> World.add (Body.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = -4, y = 0, z = 0 })) sphere)
        |> World.add (Body.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = -4, y = 0, z = 10 })) box)


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body Meshes
floor =
    Body.plane (Meshes.fromTriangles [])
        |> Body.setFrame3d (Frame3d.atPoint (Point3d.fromMeters floorOffset))


box : Body Meshes
box =
    let
        size =
            { x = 2, y = 2, z = 2 }
    in
    Meshes.box size
        |> Meshes.fromTriangles
        |> Body.block (Length.meters size.x) (Length.meters size.y) (Length.meters size.z)
        |> Body.setMass (Mass.kilograms 5)


sphere : Body Meshes
sphere =
    let
        radius =
            1.2
    in
    Meshes.sphere 2 radius
        |> Meshes.fromTriangles
        |> Body.sphere (Length.meters radius)
