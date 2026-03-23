module Duckling exposing (main)

{-| This demo loads a convex shape and a mesh from the same OBJ file.

  - elm-physics is used for simulation
  - elm-3d-scene is used for rendering
  - elm-obj-file is used for loading OBJ file

It is important to keep the convex shape as small as possible, because
this affects the simulation performance.

-}

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Frame3d
import Html exposing (Html)
import Http
import Length
import Obj.Decode exposing (Decoder)
import Physics exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Material
import Physics.Shape exposing (Shape)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Shadow, Textured)
import Task
import WebGL.Texture


type Id
    = Duckling
    | Floor


type alias Model =
    { material : Maybe (Scene3d.Material.Textured BodyCoordinates)
    , meshData : Maybe { mesh : Textured BodyCoordinates, shadow : Shadow BodyCoordinates }
    , bodies : List ( Id, Body )
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    }


type Msg
    = LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMeshes (Result Http.Error ( Textured BodyCoordinates, Shape ))
    | Resize Int Int
    | Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { material = Nothing
      , meshData = Nothing
      , dimensions = ( Pixels.int 0, Pixels.int 0 )
      , bodies = []
      }
    , Cmd.batch
        [ Scene3d.Material.load "Duckling.png"
            |> Task.attempt LoadedTexture
        , Http.get
            { url = "Duckling.obj.txt" -- .txt is required to work with `elm reactor`
            , expect = Obj.Decode.expectObj LoadedMeshes Length.meters meshAndCollider
            }
        , Task.perform
            (\{ viewport } -> Resize (round viewport.width) (round viewport.height))
            Browser.Dom.getViewport
        ]
    )


{-| Decode a render mesh and physics collider pair from the OBJ file.
-}
meshAndCollider : Decoder ( Textured BodyCoordinates, Shape )
meshAndCollider =
    Obj.Decode.map2
        (\mesh collider ->
            ( Scene3d.Mesh.texturedFaces mesh
            , Physics.Shape.unsafeConvex collider
            )
        )
        (Obj.Decode.object "mesh" (Obj.Decode.texturedFacesIn Frame3d.atOrigin))
        (Obj.Decode.object "convex" (Obj.Decode.trianglesIn Frame3d.atOrigin))


update : Msg -> Model -> Model
update msg model =
    case msg of
        LoadedTexture (Ok texture) ->
            { model | material = Just (Scene3d.Material.texturedMatte texture) }

        LoadedTexture (Err _) ->
            model

        LoadedMeshes (Ok ( mesh, shape )) ->
            { model
                | meshData = Just { mesh = mesh, shadow = Scene3d.Mesh.shadow mesh }
                , bodies =
                    let
                        ducklingAt ( axis, degrees, position ) =
                            ( Duckling
                            , Physics.dynamic [ ( shape, Physics.Material.rubber ) ]
                                |> Physics.rotateAround axis degrees
                                |> Physics.moveTo position
                            )
                    in
                    List.foldl (\location bodies -> ducklingAt location :: bodies)
                        [ ( Floor
                          , Physics.plane Physics.Material.wood
                                |> Physics.moveTo (Point3d.meters 0 0 -3)
                          )
                        ]
                        [ ( Axis3d.x, Angle.degrees 45, Point3d.meters 0 0 6 )
                        , ( Axis3d.y, Angle.degrees -35, Point3d.meters 0.1 -0.5 4 )
                        , ( Axis3d.y, Angle.degrees 35, Point3d.meters 0 0.5 8 )
                        , ( Axis3d.x, Angle.degrees -45, Point3d.meters 0 0 10 )
                        ]
            }

        LoadedMeshes (Err _) ->
            model

        Tick ->
            let
                ( simulated, _ ) =
                    Physics.simulate Physics.onEarth model.bodies
            in
            { model | bodies = simulated }

        Resize width height ->
            { model | dimensions = ( Pixels.int width, Pixels.int height ) }


view : Model -> Html Msg
view model =
    let
        camera =
            Camera3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0
                , azimuth = Angle.degrees 45
                , elevation = Angle.degrees 25
                , distance = Length.meters 25
                , projection = Camera3d.Perspective
                , fov = Camera3d.angle (Angle.degrees 30)
                }
    in
    case ( model.material, model.meshData ) of
        ( Just material, Just { mesh, shadow } ) ->
            Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.negativeZ
                , shadows = True
                , camera = camera
                , dimensions = model.dimensions
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 0.1
                , entities =
                    List.map
                        (\( data, body ) ->
                            Scene3d.placeIn (Physics.frame body) <|
                                case data of
                                    Duckling ->
                                        Scene3d.meshWithShadow material mesh shadow

                                    Floor ->
                                        ( Length.meters 25, Length.meters 25, Length.millimeters 10 )
                                            |> Block3d.centeredOn Frame3d.atOrigin
                                            |> Scene3d.block (Scene3d.Material.matte Color.darkCharcoal)
                        )
                        model.bodies
                }

        _ ->
            Html.text "Loading texture and meshes…"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrame (\_ -> Tick)
        ]
