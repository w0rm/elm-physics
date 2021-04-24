module UnsafeConvex exposing (main)

{-| This is used to demonstrate loading `unsafeConvex` shape from the OBJ file!
-}

import Acceleration
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
import Obj.Decode
import Physics.Body as Body exposing (Body)
import Physics.Material as Material
import Physics.Shape as Shape
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
      , settings = { settings | showFpsMeter = True }
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


initialWorld : World Meshes
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        |> World.add (cube |> Body.moveTo (Point3d.meters 0 0 8))
        |> World.add (icoSphere |> Body.moveTo (Point3d.meters 0.3 0 5))


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
        |> Body.moveTo (Point3d.fromMeters floorOffset)


icoSphere : Body Meshes
icoSphere =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) icoSphereObj of
        Ok triangularMesh ->
            Body.compound [ Shape.unsafeConvex triangularMesh ] (Meshes.fromTriangles (Meshes.triangularMesh triangularMesh))
                |> Body.withMaterial (Material.custom { friction = 0.4, bounciness = 0.5 })
                |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))

        Err _ ->
            Body.compound [] (Meshes.fromTriangles [])


cube : Body Meshes
cube =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) cubeObj of
        Ok triangularMesh ->
            Body.compound [ Shape.unsafeConvex triangularMesh ] (Meshes.fromTriangles (Meshes.triangularMesh triangularMesh))
                |> Body.withMaterial (Material.custom { friction = 0.4, bounciness = 0.5 })
                |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))

        Err _ ->
            Body.compound [] (Meshes.fromTriangles [])


cubeObj : String
cubeObj =
    """# Blender v2.83.3 OBJ File: 'cube'
# www.blender.org
v 1.000000 1.000000 -1.000000
v 1.000000 -1.000000 -1.000000
v 1.000000 1.000000 1.000000
v 1.000000 -1.000000 1.000000
v -1.000000 1.000000 -1.000000
v -1.000000 -1.000000 -1.000000
v -1.000000 1.000000 1.000000
v -1.000000 -1.000000 1.000000
f 1 5 7 3
f 4 3 7 8
f 8 7 5 6
f 6 2 4 8
f 2 1 3 4
f 6 5 1 2
"""


icoSphereObj : String
icoSphereObj =
    """# Blender v2.83.3 OBJ File: 'icosphere'
# www.blender.org
v 0.000000 0.000000 -1.000000
v 0.723607 -0.525725 -0.447220
v -0.276388 -0.850649 -0.447220
v -0.894426 0.000000 -0.447216
v -0.276388 0.850649 -0.447220
v 0.723607 0.525725 -0.447220
v 0.276388 -0.850649 0.447220
v -0.723607 -0.525725 0.447220
v -0.723607 0.525725 0.447220
v 0.276388 0.850649 0.447220
v 0.894426 0.000000 0.447216
v 0.000000 0.000000 1.000000
v -0.162456 -0.499995 -0.850654
v 0.425323 -0.309011 -0.850654
v 0.262869 -0.809012 -0.525738
v 0.850648 0.000000 -0.525736
v 0.425323 0.309011 -0.850654
v -0.525730 0.000000 -0.850652
v -0.688189 -0.499997 -0.525736
v -0.162456 0.499995 -0.850654
v -0.688189 0.499997 -0.525736
v 0.262869 0.809012 -0.525738
v 0.951058 -0.309013 0.000000
v 0.951058 0.309013 0.000000
v 0.000000 -1.000000 0.000000
v 0.587786 -0.809017 0.000000
v -0.951058 -0.309013 0.000000
v -0.587786 -0.809017 0.000000
v -0.587786 0.809017 0.000000
v -0.951058 0.309013 0.000000
v 0.587786 0.809017 0.000000
v 0.000000 1.000000 0.000000
v 0.688189 -0.499997 0.525736
v -0.262869 -0.809012 0.525738
v -0.850648 0.000000 0.525736
v -0.262869 0.809012 0.525738
v 0.688189 0.499997 0.525736
v 0.162456 -0.499995 0.850654
v 0.525730 0.000000 0.850652
v -0.425323 -0.309011 0.850654
v -0.425323 0.309011 0.850654
v 0.162456 0.499995 0.850654
s off
f 1 14 13
f 2 14 16
f 1 13 18
f 1 18 20
f 1 20 17
f 2 16 23
f 3 15 25
f 4 19 27
f 5 21 29
f 6 22 31
f 2 23 26
f 3 25 28
f 4 27 30
f 5 29 32
f 6 31 24
f 7 33 38
f 8 34 40
f 9 35 41
f 10 36 42
f 11 37 39
f 39 42 12
f 39 37 42
f 37 10 42
f 42 41 12
f 42 36 41
f 36 9 41
f 41 40 12
f 41 35 40
f 35 8 40
f 40 38 12
f 40 34 38
f 34 7 38
f 38 39 12
f 38 33 39
f 33 11 39
f 24 37 11
f 24 31 37
f 31 10 37
f 32 36 10
f 32 29 36
f 29 9 36
f 30 35 9
f 30 27 35
f 27 8 35
f 28 34 8
f 28 25 34
f 25 7 34
f 26 33 7
f 26 23 33
f 23 11 33
f 31 32 10
f 31 22 32
f 22 5 32
f 29 30 9
f 29 21 30
f 21 4 30
f 27 28 8
f 27 19 28
f 19 3 28
f 25 26 7
f 25 15 26
f 15 2 26
f 23 24 11
f 23 16 24
f 16 6 24
f 17 22 6
f 17 20 22
f 20 5 22
f 20 21 5
f 20 18 21
f 18 4 21
f 18 19 4
f 18 13 19
f 13 3 19
f 16 17 6
f 16 14 17
f 14 1 17
f 13 15 3
f 13 14 15
f 14 2 15
"""
