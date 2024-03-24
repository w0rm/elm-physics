module CompoundVsLock exposing (main)

{-| This demo shows two possible ways to create complex objects.
One way is through a compound body out of multiple shapes.
The second way is by using the lock constraint.
-}

import Acceleration
import Block3d exposing (Block3d)
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
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Shape as Shape
import Physics.World as World exposing (World)
import Point3d exposing (Point3d)
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
      , settings = { settings | showSettings = True }
      , camera =
            Camera.camera
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
                }
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
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
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
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
    let
        lockedPosition =
            Frame3d.atPoint (Point3d.meters -2 0 5)

        compoundPosition =
            Point3d.meters 2 0 5
    in
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        |> World.add
            (compound
                |> Body.moveTo compoundPosition
            )
        |> World.add
            (box "first"
                |> Body.moveTo (Point3d.placeIn lockedPosition pos1)
            )
        |> World.add
            (box "second"
                |> Body.moveTo (Point3d.placeIn lockedPosition pos2)
            )
        |> World.add
            (box "third"
                |> Body.moveTo (Point3d.placeIn lockedPosition pos3)
            )
        |> World.constrain lockBlocks


lockBlocks : Body Data -> Body Data -> List Constraint
lockBlocks b1 b2 =
    case ( (Body.data b1).name, (Body.data b2).name ) of
        ( "first", "second" ) ->
            [ lockTwoBodies b1 b2 ]

        ( "second", "third" ) ->
            [ lockTwoBodies b1 b2 ]

        _ ->
            []


lockTwoBodies : Body Data -> Body Data -> Constraint
lockTwoBodies b1 b2 =
    let
        center1 =
            Body.originPoint b1

        center2 =
            Body.originPoint b2

        middle =
            Point3d.midpoint center1 center2

        frame1 =
            Frame3d.atPoint (Point3d.relativeTo (Body.frame b1) middle)

        frame2 =
            Frame3d.atPoint (Point3d.relativeTo (Body.frame b2) middle)
    in
    Constraint.lock frame1 frame2


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body Data
floor =
    Body.plane { mesh = Meshes.fromTriangles [], name = "floor" }
        |> Body.moveTo (Point3d.fromMeters floorOffset)


{-| A single box
-}
box : String -> Body Data
box name =
    Body.block block3d { mesh = Meshes.fromTriangles (Meshes.block block3d), name = name }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))


{-| A compound body made of three boxes
-}
compound : Body Data
compound =
    let
        blocks =
            List.map
                (\center ->
                    Block3d.placeIn (Frame3d.atPoint center) block3d
                )
                [ pos1, pos2, pos3 ]
    in
    Body.compound
        (List.map Shape.block blocks)
        { mesh = Meshes.fromTriangles (List.concatMap Meshes.block blocks), name = "compound" }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))


block3d : Block3d Meters BodyCoordinates
block3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 1
        , Length.meters 1
        , Length.meters 1
        )


pos1 : Point3d Meters BodyCoordinates
pos1 =
    Point3d.meters -0.5 0 -0.5


pos2 : Point3d Meters BodyCoordinates
pos2 =
    Point3d.meters -0.5 0 0.5


pos3 : Point3d Meters BodyCoordinates
pos3 =
    Point3d.meters 0.5 0 0.5
