module Duckling exposing (main)

{-| This demo loads a convex shape and a mesh from the same OBJ file.

  - elm-physics is used for simulation
  - elm-3d-scene is used for rendering

It is important to keep the convex shape as small as possible, because
this affects the simulation performance.

-}

import Acceleration
import Angle
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Duration
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Http
import Length exposing (Meters)
import Mass
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Shape
import Physics.World exposing (World)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Shadow, Textured)
import Task
import Viewpoint3d
import WebGL.Texture


type Data
    = MeshWithShadow (Textured BodyCoordinates) (Shadow BodyCoordinates)
    | Floor


bodyFrame : Frame3d Meters BodyCoordinates { defines : ObjCoordinates }
bodyFrame =
    Frame3d.atOrigin


{-| Decode a mesh together with the shadow.
-}
meshWithShadow : Decoder Data
meshWithShadow =
    Obj.Decode.map
        (\texturedFaces ->
            let
                mesh =
                    Scene3d.Mesh.texturedFaces texturedFaces
                        |> Scene3d.Mesh.cullBackFaces
            in
            MeshWithShadow mesh (Scene3d.Mesh.shadow mesh)
        )
        (Obj.Decode.texturedFacesIn bodyFrame)


{-| Maps three decoders to get a decoder of the required meshes.
-}
meshes : Decoder (Body Data)
meshes =
    Obj.Decode.map2
        (\convex mesh ->
            Physics.Body.compound
                [ Physics.Shape.unsafeConvex convex ]
                mesh
                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 1))
        )
        (Obj.Decode.object "convex" (Obj.Decode.trianglesIn bodyFrame))
        (Obj.Decode.object "mesh" meshWithShadow)


floorBlock : Block3d Meters BodyCoordinates
floorBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 25, Length.meters 25, Length.millimeters 10 )


type alias Model =
    { material : Maybe (Scene3d.Material.Textured BodyCoordinates)
    , world : World Data
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    }


type Msg
    = LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMeshes (Result Http.Error (Body Data))
    | Resize Int Int
    | Tick Float


init : () -> ( Model, Cmd Msg )
init () =
    ( { material = Nothing
      , dimensions = ( Pixels.int 0, Pixels.int 0 )
      , world =
            Physics.World.empty
                |> Physics.World.withGravity
                    (Acceleration.metersPerSecondSquared 9.80665)
                    Direction3d.negativeZ
                |> Physics.World.add
                    (Physics.Body.plane Floor
                        |> Physics.Body.moveTo (Point3d.meters 0 0 -3)
                    )
      }
    , Cmd.batch
        [ Scene3d.Material.load "Duckling.png"
            |> Task.attempt LoadedTexture
        , Http.get
            { url = "Duckling.obj.txt" -- .txt is required to work with `elm reactor`
            , expect = Obj.Decode.expectObj LoadedMeshes Length.meters meshes
            }
        , Task.perform
            (\{ viewport } -> Resize (round viewport.width) (round viewport.height))
            Browser.Dom.getViewport
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedTexture result ->
            ( { model
                | material =
                    result
                        |> Result.map Scene3d.Material.texturedMatte
                        |> Result.toMaybe
              }
            , Cmd.none
            )

        LoadedMeshes result ->
            case result of
                Ok body ->
                    ( { model
                        | world =
                            model.world
                                |> Physics.World.add
                                    (body
                                        |> Physics.Body.rotateAround Axis3d.x (Angle.degrees 45)
                                        |> Physics.Body.moveTo (Point3d.meters 0 0 6)
                                    )
                                |> Physics.World.add
                                    (body
                                        |> Physics.Body.rotateAround Axis3d.y (Angle.degrees -35)
                                        |> Physics.Body.moveTo (Point3d.meters 0.1 -0.5 4)
                                    )
                                |> Physics.World.add
                                    (body
                                        |> Physics.Body.rotateAround Axis3d.y (Angle.degrees 35)
                                        |> Physics.Body.moveTo (Point3d.meters 0 0.5 8)
                                    )
                                |> Physics.World.add
                                    (body
                                        |> Physics.Body.rotateAround Axis3d.x (Angle.degrees -45)
                                        |> Physics.Body.moveTo (Point3d.meters 0 0 10)
                                    )
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        Tick _ ->
            ( { model
                | world =
                    Physics.World.simulate (Duration.milliseconds 16) model.world
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | dimensions = ( Pixels.int width, Pixels.int height ) }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0 0 0
                        , azimuth = Angle.degrees 45
                        , elevation = Angle.degrees 25
                        , distance = Length.meters 25
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    case model.material of
        Just material ->
            Scene3d.sunny
                { upDirection = Direction3d.z
                , sunlightDirection = Direction3d.negativeZ
                , shadows = True
                , camera = camera
                , dimensions = model.dimensions
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 0.1
                , entities =
                    model.world
                        |> Physics.World.bodies
                        |> List.map
                            (\body ->
                                let
                                    frame3d =
                                        Physics.Body.frame body
                                in
                                case Physics.Body.data body of
                                    MeshWithShadow mesh shadow ->
                                        Scene3d.meshWithShadow material mesh shadow
                                            |> Scene3d.placeIn frame3d

                                    Floor ->
                                        Scene3d.block (Scene3d.Material.matte Color.darkCharcoal) floorBlock
                                            |> Scene3d.placeIn frame3d
                            )
                }

        _ ->
            Html.text "Loading texture and meshes…"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions =
            always
                (Sub.batch
                    [ Browser.Events.onAnimationFrameDelta Tick
                    , Browser.Events.onResize Resize
                    ]
                )
        }
