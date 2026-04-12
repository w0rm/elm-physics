module CompoundVsLock exposing (main)

{-| This demo shows two possible ways to create complex objects.
One way is through a compound body out of multiple shapes.
The second way is by using the lock constraint.
-}

import Block3d exposing (Block3d)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Dict exposing (Dict)
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics exposing (Body, BodyCoordinates, WorldCoordinates, onEarth)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d exposing (Point3d)
import Task
import WebGL exposing (Mesh)


type alias Model =
    { bodies : List ( String, Body )
    , meshes : Dict String (Mesh Attributes)
    , contacts : Physics.Contacts String
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
            let
                bodyDict =
                    Dict.fromList model.bodies

                constrain id1 =
                    case id1 of
                        "first" ->
                            Just
                                (\id2 ->
                                    case id2 of
                                        "second" ->
                                            case ( Dict.get "first" bodyDict, Dict.get "second" bodyDict ) of
                                                ( Just b1, Just b2 ) ->
                                                    [ lockTwoBodies b1 b2 ]

                                                _ ->
                                                    []

                                        _ ->
                                            []
                                )

                        "second" ->
                            Just
                                (\id2 ->
                                    case id2 of
                                        "third" ->
                                            case ( Dict.get "second" bodyDict, Dict.get "third" bodyDict ) of
                                                ( Just b1, Just b2 ) ->
                                                    [ lockTwoBodies b1 b2 ]

                                                _ ->
                                                    []

                                        _ ->
                                            []
                                )

                        _ ->
                            Nothing

                ( newBodies, newContacts ) =
                    Physics.simulate
                        { onEarth
                            | constrain = constrain
                            , collide =
                                \one two ->
                                    not
                                        (List.member one [ "first", "second", "third" ]
                                            && List.member two [ "first", "second", "third" ]
                                        )
                            , contacts = model.contacts
                        }
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
            ( { model | bodies = initialBodies, contacts = Physics.emptyContacts }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, bodies, contacts, meshes, camera } =
    Html.div []
        [ Scene.view
            { settings = settings
            , bodies =
                List.filterMap
                    (\( id, body ) ->
                        Maybe.map (\mesh -> ( mesh, body )) (Dict.get id meshes)
                    )
                    bodies
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


lockTwoBodies : Body -> Body -> Constraint
lockTwoBodies b1 b2 =
    let
        center1 =
            Physics.originPoint b1

        center2 =
            Physics.originPoint b2

        middle =
            Point3d.midpoint center1 center2

        frame1 =
            Frame3d.atPoint (Point3d.relativeTo (Physics.frame b1) middle)

        frame2 =
            Frame3d.atPoint (Point3d.relativeTo (Physics.frame b2) middle)
    in
    Constraint.lock frame1 frame2


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


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


initialBodies : List ( String, Body )
initialBodies =
    let
        lockedPosition =
            Frame3d.atPoint (Point3d.meters -2 0 5)

        compoundPosition =
            Point3d.meters 2 0 5

        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        blocks =
            List.map
                (\center ->
                    Block3d.placeIn (Frame3d.atPoint center) block3d
                )
                [ pos1, pos2, pos3 ]

        compoundBody =
            Physics.dynamic
                (List.map (\b -> ( Shape.block b, Material.wood )) blocks)
                |> Physics.scaleTo (Mass.kilograms 5)
                |> Physics.moveTo compoundPosition

        boxBody =
            Physics.block block3d Material.wood
                |> Physics.scaleTo (Mass.kilograms 5)
    in
    [ ( "floor", floorBody )
    , ( "compound", compoundBody )
    , ( "first", boxBody |> Physics.moveTo (Point3d.placeIn lockedPosition pos1) )
    , ( "second", boxBody |> Physics.moveTo (Point3d.placeIn lockedPosition pos2) )
    , ( "third", boxBody |> Physics.moveTo (Point3d.placeIn lockedPosition pos3) )
    ]


initialMeshes : Dict String (Mesh Attributes)
initialMeshes =
    let
        blocks =
            List.map
                (\center ->
                    Block3d.placeIn (Frame3d.atPoint center) block3d
                )
                [ pos1, pos2, pos3 ]

        boxMesh =
            Meshes.fromTriangles (Meshes.block block3d)
    in
    Dict.fromList
        [ ( "floor", Meshes.fromTriangles [] )
        , ( "compound", Meshes.fromTriangles (List.concatMap Meshes.block blocks) )
        , ( "first", boxMesh )
        , ( "second", boxMesh )
        , ( "third", boxMesh )
        ]
