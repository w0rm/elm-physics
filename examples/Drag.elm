module Drag exposing (main)

{-| This demo allows dragging bodies with mouse!

1.  Uses `World.raycast` on mouse down to pick a body
2.  Creates a temporary body at the mouse position
3.  Connects the temporary body with the selected body using a point to point constraint
4.  Moves the temporary body on mouse move
5.  Removes the temporary body on mouse up

-}

import Acceleration
import Angle
import Axis3d
import Block3d
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
import Physics.Constraint as Constraint
import Physics.World as World exposing (RaycastResult, World)
import Point3d
import Sphere3d


{-| Each body should have a unique id,
so that we can later tell which one was selected!
-}
type Id
    = Mouse
    | Floor
    | Box Int


type alias Data =
    { meshes : Meshes
    , id : Id
    }


type alias Model =
    { world : World Data
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , maybeRaycastResult : Maybe (RaycastResult Data)
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart
    | MouseDown { x : Float, y : Float, z : Float }
    | MouseMove { x : Float, y : Float, z : Float }
    | MouseUp { x : Float, y : Float, z : Float }


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
      , maybeRaycastResult = Nothing
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
                        |> World.simulate (Duration.seconds (1 / 60))
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model | world = initialWorld }, Cmd.none )

        MouseDown direction ->
            let
                maybeRaycastResult =
                    model.world
                        |> World.raycast
                            (Axis3d.through
                                (Point3d.fromMeters model.camera.from)
                                (Direction3d.unsafe direction)
                            )
                        |> Maybe.andThen
                            -- only allow clicks on boxes
                            (\result ->
                                case (Body.data result.body).id of
                                    Box _ ->
                                        Just result

                                    _ ->
                                        Nothing
                            )
            in
            case maybeRaycastResult of
                Just raycastResult ->
                    -- create temporary body and constrain it
                    -- with selected body
                    let
                        worldPosition =
                            Point3d.placeIn (Body.frame raycastResult.body) raycastResult.point
                    in
                    ( { model
                        | maybeRaycastResult = maybeRaycastResult
                        , world =
                            model.world
                                |> World.add (Body.moveTo worldPosition mouse)
                                |> World.constrain
                                    (\b1 b2 ->
                                        if (Body.data b1).id == Mouse && (Body.data b2).id == (Body.data raycastResult.body).id then
                                            [ Constraint.pointToPoint
                                                Point3d.origin
                                                raycastResult.point
                                            ]

                                        else
                                            []
                                    )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseMove newDirection ->
            case model.maybeRaycastResult of
                -- move the mouse
                Just raycastResult ->
                    let
                        -- the new position is an intersection
                        -- of the newDirection from the camera and a plane,
                        -- that is defined by the camera direction
                        -- and a point from the raycastResult
                        -- https://samsymons.com/blog/math-notes-ray-plane-intersection/
                        r0 =
                            model.camera.from

                        p0 =
                            -- Transform local point on body into world coordinates
                            raycastResult.point
                                |> Point3d.placeIn (Body.frame raycastResult.body)
                                |> Point3d.toMeters

                        n =
                            Direction3d.from (Point3d.fromMeters model.camera.from) (Point3d.fromMeters model.camera.to)
                                |> Maybe.withDefault Direction3d.z
                                |> Direction3d.unwrap

                        t =
                            ((n.x * (p0.x - r0.x)) + (n.y * (p0.y - r0.y)) + (n.z * (p0.z - r0.z)))
                                / ((n.x * newDirection.x) + (n.y * newDirection.y) + (n.z * newDirection.z))

                        intersection =
                            { x = r0.x + newDirection.x * t
                            , y = r0.y + newDirection.y * t
                            , z = r0.z + newDirection.z * t
                            }
                    in
                    ( { model
                        | world =
                            World.update
                                (\b ->
                                    if (Body.data b).id == Mouse then
                                        Body.moveTo (Point3d.fromMeters intersection) b

                                    else
                                        b
                                )
                                model.world
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseUp _ ->
            -- remove temporary body on mouse up
            ( { model
                | maybeRaycastResult = Nothing
                , world =
                    World.keepIf
                        (Body.data >> .id >> (/=) Mouse)
                        model.world
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, world, camera, maybeRaycastResult } =
    Html.div
        [ Events.onMouseDown camera MouseDown
        , Events.onMouseMove camera MouseMove
        , Events.onMouseUp camera MouseUp
        ]
        [ Scene.view
            { settings = settings
            , world = world
            , camera = camera
            , meshes = .meshes
            , maybeRaycastResult = maybeRaycastResult
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
        |> World.add
            (box 1
                |> Body.rotateAround Axis3d.y (Angle.radians (-pi / 5))
                |> Body.moveTo (Point3d.meters 0 0 2)
            )
        |> World.add
            (box 2
                |> Body.moveTo (Point3d.meters 0.5 0 8)
            )
        |> World.add
            (box 3
                |> Body.rotateAround
                    (Axis3d.through Point3d.origin (Direction3d.unsafe { x = 0.7071, y = 0.7071, z = 0 }))
                    (Angle.radians (pi / 5))
                |> Body.moveTo (Point3d.meters -1.2 0 5)
            )


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body Data
floor =
    { id = Floor, meshes = Meshes.fromTriangles [] }
        |> Body.plane
        |> Body.moveTo (Point3d.fromMeters floorOffset)


{-| One of the boxes on the scene
-}
box : Int -> Body Data
box id =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 2
                , Length.meters 2
                , Length.meters 2
                )
    in
    Body.block block3d
        { id = Box id
        , meshes = Meshes.fromTriangles (Meshes.block block3d)
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 10))


{-| An empty body with zero mass, rendered as a sphere.
This is a temporary body used to drag selected bodies.
-}
mouse : Body Data
mouse =
    let
        sphere3d =
            Sphere3d.atOrigin (Length.meters 0.2)
    in
    Body.compound []
        { id = Mouse
        , meshes = Meshes.fromTriangles (Meshes.sphere 2 sphere3d)
        }
