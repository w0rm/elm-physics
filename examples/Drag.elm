module Drag exposing (main)

{-| This demo allows dragging bodies with mouse!

1.  Uses `World.raycast` on mouse down to pick a body
2.  Creates a temporary body at the mouse position
3.  Connects the temporary body with the selected body using a point to point constraint
4.  Moves the temporary body on mouse move
5.  Removes the temporary body on mouse up

-}

import Browser
import Common.Camera as Camera exposing (Camera)
import Common.Events as Events
import Common.Fps as Fps
import Common.Math as Math
import Common.Meshes as Meshes exposing (Meshes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint
import Physics.World as World exposing (RaycastResult, World)


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
    , selection : Maybe Selection
    }


type alias Selection =
    { raycastResult : RaycastResult Data
    , direction : { x : Float, y : Float, z : Float }
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
      , selection = Nothing
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

        MouseDown direction ->
            case
                World.raycast
                    { from = model.camera.from
                    , direction = direction
                    }
                    model.world
            of
                Just raycastResult ->
                    -- create temporary body and constrain it
                    -- with selected body
                    let
                        worldPosition =
                            raycastResult.point
                                |> Vec3.fromRecord
                                |> Mat4.transform (Mat4.fromRecord (Body.getTransformation raycastResult.body))
                                |> Vec3.toRecord
                    in
                    ( { model
                        | selection =
                            Just
                                { raycastResult = raycastResult
                                , direction = direction
                                }
                        , world =
                            model.world
                                |> World.add (Body.setPosition worldPosition mouse)
                                |> World.constrain
                                    (\b1 b2 ->
                                        if (Body.getData b1).id == Mouse && (Body.getData b2).id == (Body.getData raycastResult.body).id then
                                            [ Constraint.pointToPoint
                                                { pivot1 = { x = 0, y = 0, z = 0 }
                                                , pivot2 = raycastResult.point
                                                }
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
            case model.selection of
                -- move the mouse
                Just { raycastResult, direction } ->
                    let
                        -- the new position is an intersection
                        -- of the newDirection from the camera and a plane,
                        -- that is defined by a normal = previous mouse direction
                        -- and a point from the raycastResult
                        -- https://samsymons.com/blog/math-notes-ray-plane-intersection/
                        r0 =
                            model.camera.from

                        p0 =
                            -- Transform local point on body into world coordinates
                            Mat4.transform
                                (Mat4.fromRecord (Body.getTransformation raycastResult.body))
                                (Vec3.fromRecord raycastResult.point)
                                |> Vec3.toRecord

                        n =
                            direction

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
                                    if (Body.getData b).id == Mouse then
                                        Body.setPosition intersection b

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
                | selection = Nothing
                , world =
                    World.keepIf
                        (Body.getData >> .id >> (/=) Mouse)
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
view { settings, fps, world, camera, selection } =
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
            , raycastResult = Maybe.map .raycastResult selection
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
        |> World.add
            (box 1
                |> Body.moveBy { x = 0, y = 0, z = 2 }
                |> Body.rotateBy (-pi / 5) { x = 0, y = 1, z = 0 }
            )
        |> World.add
            (box 2
                |> Body.moveBy { x = 0.5, y = 0, z = 8 }
            )
        |> World.add
            (box 3
                |> Body.moveBy { x = -1.2, y = 0, z = 5 }
                |> Body.rotateBy (pi / 5) { x = 1, y = 1, z = 0 }
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
        |> Body.setPosition floorOffset


{-| One of the boxes on the scene
-}
box : Int -> Body Data
box id =
    let
        size =
            { x = 2, y = 2, z = 2 }

        meshes =
            Meshes.fromTriangles (Meshes.box size)
    in
    { id = Box id, meshes = meshes }
        |> Body.box size
        |> Body.setMass 10


{-| An empty body with zero mass, rendered as a sphere.
This is a temporary body used to drag selected bodies.
-}
mouse : Body Data
mouse =
    let
        radius =
            0.2

        meshes =
            Meshes.fromTriangles (Meshes.sphere 2 radius)
    in
    { id = Mouse, meshes = meshes }
        |> Body.compound []
