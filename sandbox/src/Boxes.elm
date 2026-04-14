module Boxes exposing (main)

{-| This demo is used to test performance. It drops 5×5×5 boxes.
Try changing `boxesPerDimension` to drop even more!
-}

import Array exposing (Array)
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics exposing (Body, WorldCoordinates, onEarth)
import Physics.Material as Material
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d exposing (Point3d)
import Task
import WebGL exposing (Mesh)


boxesPerDimension : number
boxesPerDimension =
    5


type alias Model =
    { bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : Contacts Int
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
    ( { bodies = initialBodies
      , meshes = initialMeshes
      , contacts = Physics.emptyContacts
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
            let
                ( newBodies, newContacts ) =
                    Physics.simulate
                        { onEarth | contacts = model.contacts }
                        model.bodies
            in
            ( { model
                | fps = Fps.update dt model.fps
                , bodies = newBodies
                , contacts = newContacts
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Restart ->
            ( { model | bodies = initialBodies, meshes = initialMeshes, contacts = Physics.emptyContacts }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, bodies, meshes, contacts, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , bodies = List.filterMap (\( id, body ) -> Maybe.map (\mesh -> ( mesh, body )) (Array.get id meshes)) bodies
            , contacts = List.concatMap (\( _, _, c ) -> c) (Physics.contactPoints (\_ _ -> True) contacts)
            , camera = camera
            , floorOffset = floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            let
                (Contacts c) =
                    contacts
            in
            Fps.view fps (List.length bodies) c.iterations

          else
            Html.text ""
        ]


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


initialBodies : List ( Int, Body )
initialBodies =
    let
        -- id=0 is the floor
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        dimensions =
            List.map toFloat (List.range 0 (boxesPerDimension - 1))

        distance =
            1

        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1, Length.meters 1, Length.meters 1 )

        boxBody =
            Physics.block block3d Material.wood
                |> Physics.scaleMassTo (Mass.kilograms 5)

        boxes =
            List.indexedMap
                (\idx ( x, y, z ) ->
                    ( idx + 1
                    , boxBody
                        |> Physics.moveTo
                            (Point3d.meters
                                ((x - (boxesPerDimension - 1) / 2) * distance)
                                ((y - (boxesPerDimension - 1) / 2) * distance)
                                ((z + (2 * boxesPerDimension + 1) / 2) * distance)
                            )
                    )
                )
                (List.concatMap
                    (\x ->
                        List.concatMap
                            (\y -> List.map (\z -> ( x, y, z )) dimensions)
                            dimensions
                    )
                    dimensions
                )
    in
    ( 0, floorBody ) :: boxes


initialMeshes : Array (Mesh Attributes)
initialMeshes =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1, Length.meters 1, Length.meters 1 )

        -- id=0 is floor (empty mesh)
        floorMesh =
            Meshes.fromTriangles []

        boxMesh =
            Meshes.fromTriangles (Meshes.block block3d)

        boxCount =
            boxesPerDimension ^ 3
    in
    Array.fromList (floorMesh :: List.repeat boxCount boxMesh)
