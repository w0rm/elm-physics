module Randomize exposing (main)

{-| This demo drops random bodies.
It also shows how to make a compound body out of multiple shapes.
If you click too fast, the bodies may be spawned inside each other.
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
import Length
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Shape as Shape
import Physics.World as World exposing (World)
import Point3d
import Random
import Vector3d


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
    | Random
    | AddRandom (Body Meshes)


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
      , settings = { settings | showSettings = True }
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
                , world = World.simulate (1000 / 60) model.world
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model | world = initialWorld }, Cmd.none )

        Random ->
            ( model, Random.generate AddRandom randomBody )

        AddRandom body ->
            ( { model | world = World.add body model.world }, Cmd.none )


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
            [ Html.button [ onClick Random ]
                [ Html.text "Drop a random body" ]
            , Html.button [ onClick Restart ]
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
        |> World.setGravity { x = 0, y = 0, z = -10 }
        |> World.add floor
        |> World.add
            (box
                |> Body.setFrame3d
                    (Frame3d.atPoint Point3d.origin
                        |> Frame3d.rotateAround Axis3d.y (Angle.radians (-pi / 5))
                        |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 2 })
                    )
            )
        |> World.add
            (sphere
                |> Body.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = 0.5, y = 0, z = 8 }))
            )
        |> World.add
            (compound
                |> Body.setFrame3d
                    (Frame3d.atPoint Point3d.origin
                        |> Frame3d.rotateAround (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 })) (Angle.radians (pi / 5))
                        |> Frame3d.moveTo (Point3d.fromMeters { x = -1.2, y = 0, z = 5 })
                    )
            )


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
        |> Body.setMass (Mass.kilograms 5)


{-| A compound body made of three boxes
-}
compound : Body Meshes
compound =
    let
        size =
            { x = 1, y = 1, z = 1 }

        boxTriangles =
            Meshes.box size

        boxShape =
            Shape.block (Length.meters size.x) (Length.meters size.y) (Length.meters size.z)
    in
    [ Meshes.moveBy { x = -0.5, y = 0, z = -0.5 } boxTriangles
    , Meshes.moveBy { x = -0.5, y = 0, z = 0.5 } boxTriangles
    , Meshes.moveBy { x = 0.5, y = 0, z = 0.5 } boxTriangles
    ]
        |> List.concat
        |> Meshes.fromTriangles
        |> Body.compound
            [ Shape.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = -0.5, y = 0, z = -0.5 })) boxShape
            , Shape.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = -0.5, y = 0, z = 0.5 })) boxShape
            , Shape.setFrame3d (Frame3d.atPoint (Point3d.fromMeters { x = 0.5, y = 0, z = 0.5 })) boxShape
            ]
        |> Body.setMass (Mass.kilograms 5)


{-| A random body raised above the plane, shifted or rotated to a random 3d angle
-}
randomBody : Random.Generator (Body Meshes)
randomBody =
    Random.map5
        (\angle x y z body ->
            (case body of
                0 ->
                    box

                1 ->
                    sphere

                _ ->
                    compound
            )
                |> Body.setFrame3d
                    (Frame3d.atPoint Point3d.origin
                        |> Frame3d.rotateAround (Axis3d.through Point3d.origin (Maybe.withDefault Direction3d.x (Vector3d.direction (Vector3d.from Point3d.origin (Point3d.fromMeters { x = x, y = y, z = z })))))
                            (Angle.radians angle)
                        |> Frame3d.moveTo (Point3d.fromMeters { x = 0, y = 0, z = 10 })
                    )
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.int 0 2)
