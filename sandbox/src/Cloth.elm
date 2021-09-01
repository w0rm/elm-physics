module Cloth exposing (main)

{-| Cloth simulation built using many particle bodies,
connected with distance constraints.
-}

import Acceleration
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
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.World as World exposing (World)
import Point3d
import Sphere3d
import WebGL exposing (Mesh)


particlesPerDimension : Int
particlesPerDimension =
    10


distanceBetweenParticles : Float
distanceBetweenParticles =
    0.5


type BodyKind
    = Particle Int Int -- x and y for constraints
    | Other


type alias Data =
    { kind : BodyKind
    , mesh : Mesh Attributes
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
      , settings = { settings | showFpsMeter = True }
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
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floor
        |> World.add (Body.moveTo (Point3d.meters 0 0 1) sphere)
        |> addCloth
        |> World.constrain constrainCloth


addCloth : World Data -> World Data
addCloth world =
    let
        dimensions =
            List.range 0 (particlesPerDimension - 1)
    in
    List.foldl
        (\x world1 ->
            List.foldl
                (\y ->
                    particle x y
                        |> Body.moveTo
                            (Point3d.meters
                                ((toFloat x - (toFloat particlesPerDimension - 1) / 2) * distanceBetweenParticles)
                                ((toFloat y - (toFloat particlesPerDimension - 1) / 2) * distanceBetweenParticles)
                                8
                            )
                        |> World.add
                )
                world1
                dimensions
        )
        world
        dimensions


{-| Set up constraints between adjacent particles
-}
constrainCloth : Body Data -> Body Data -> List Constraint
constrainCloth body1 body2 =
    case ( (Body.data body1).kind, (Body.data body2).kind ) of
        ( Particle x1 y1, Particle x2 y2 ) ->
            if x1 == x2 && y2 - y1 == 1 || y1 == y2 && x2 - x1 == 1 then
                [ Constraint.distance (Length.meters distanceBetweenParticles) ]
                -- Uncomment to add diagonal connections,
                -- that make the cloth stiffer:
                -- else if abs (x2 - x1) == 1 && y2 - y1 == 1 then
                --     [ Constraint.distance (Length.meters (sqrt distanceBetweenParticles)) ]

            else
                []

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
    Body.plane { kind = Other, mesh = Meshes.fromTriangles [] }
        |> Body.moveTo (Point3d.fromMeters floorOffset)


sphere : Body Data
sphere =
    let
        sphere3d =
            Sphere3d.atOrigin (Length.meters 2)
    in
    Body.sphere sphere3d
        { mesh = Meshes.fromTriangles (Meshes.sphere 3 sphere3d)
        , kind = Other
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))


particle : Int -> Int -> Body Data
particle x y =
    let
        sphere3d =
            Sphere3d.atOrigin (Length.meters 0.1)
    in
    Body.particle
        { mesh = Meshes.fromTriangles (Meshes.sphere 1 sphere3d)
        , kind = Particle x y
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))
