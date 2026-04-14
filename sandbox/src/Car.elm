module Car exposing (main)

{-| This shows how hinge constrains can be used to assemble a car.
Use the arrow keys to steer and speed!
-}

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Dict exposing (Dict)
import Direction3d
import Force
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode
import Length exposing (Meters)
import Mass
import Physics exposing (Body, BodyCoordinates, WorldCoordinates, onEarth)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d exposing (Point3d)
import Sphere3d
import Task
import Vector3d
import WebGL exposing (Mesh)


type Command
    = Speed Float
    | Steer Float


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "ArrowRight" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "ArrowUp" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "ArrowDown" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )


type alias Model =
    { bodies : List ( String, Body )
    , meshes : Dict String (Mesh Attributes)
    , contacts : Physics.Contacts String
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , speeding : Float -- -1, 0, 1
    , steering : Float -- -1, 0, 1
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart
    | KeyDown Command
    | KeyUp Command


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bodies = initialBodies
      , meshes = initialMeshes
      , contacts = Physics.emptyContacts
      , fps = []
      , settings = settings
      , speeding = 0
      , steering = 0
      , camera =
            Camera.camera
                { from = { x = -60, y = 60, z = 40 }
                , to = { x = 0, y = -7, z = 0 }
                }
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ForSettings settingsMsg ->
            { model
                | settings = Settings.update settingsMsg model.settings
            }

        Tick dt ->
            let
                baseFrame =
                    model.bodies
                        |> List.filterMap
                            (\( id, body ) ->
                                if id == "base" then
                                    Just (Physics.frame body)

                                else
                                    Nothing
                            )
                        |> List.head
                        |> Maybe.withDefault Frame3d.atOrigin

                bodiesWithForce =
                    List.map
                        (\( id, body ) ->
                            if model.speeding /= 0 && (id == "wheel1" || id == "wheel2") then
                                ( id, applySpeed model.speeding baseFrame body )

                            else
                                ( id, body )
                        )
                        model.bodies

                ( newBodies, newContacts ) =
                    Physics.simulate
                        { onEarth | constrain = constrainCar model.steering, contacts = model.contacts }
                        bodiesWithForce
            in
            { model
                | fps = Fps.update dt model.fps
                , bodies = newBodies
                , contacts = newContacts
            }

        Resize width height ->
            { model | camera = Camera.resize width height model.camera }

        Restart ->
            { model | bodies = initialBodies, contacts = Physics.emptyContacts }

        KeyDown (Steer k) ->
            { model | steering = k }

        KeyDown (Speed k) ->
            { model | speeding = k }

        KeyUp (Steer k) ->
            { model
                | steering =
                    if k == model.steering then
                        0

                    else
                        model.steering
            }

        KeyUp (Speed k) ->
            { model
                | speeding =
                    if k == model.speeding then
                        0

                    else
                        model.speeding
            }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
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


applySpeed : Float -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body -> Body
applySpeed speed baseFrame body =
    let
        forward =
            Frame3d.yDirection baseFrame

        up =
            Frame3d.zDirection baseFrame

        wheelPoint =
            Frame3d.originPoint (Physics.frame body)

        pointOnTheWheel =
            wheelPoint |> Point3d.translateBy (Vector3d.withLength (Length.meters 1.2) up)

        pointUnderTheWheel =
            wheelPoint |> Point3d.translateBy (Vector3d.withLength (Length.meters 1.2) (Direction3d.reverse up))

        force =
            Vector3d.withLength (Force.newtons (speed * 100)) forward
    in
    body
        |> Physics.applyForce force pointUnderTheWheel
        |> Physics.applyForce (Vector3d.reverse force) pointOnTheWheel


constrainCar : Float -> String -> Maybe (String -> List Constraint)
constrainCar steering id1 =
    let
        steeringAngle =
            steering * pi / 8

        dx =
            cos steeringAngle

        dy =
            sin steeringAngle

        hinge1 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 3 3 0)
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )
                (Axis3d.through
                    (Point3d.meters 0 0 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )

        hinge2 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -3 3 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge3 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -3 -3 0)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge4 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 3 -3 0)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
    in
    if id1 == "base" then
        Just
            (\id2 ->
                case id2 of
                    "wheel1" ->
                        [ hinge1 ]

                    "wheel2" ->
                        [ hinge2 ]

                    "wheel3" ->
                        [ hinge3 ]

                    "wheel4" ->
                        [ hinge4 ]

                    _ ->
                        []
            )

    else
        Nothing


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


initialBodies : List ( String, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        slopeBlock3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )

        slopeBody =
            Physics.static [ ( Shape.block slopeBlock3d, Material.wood ) ]
                |> Physics.rotateAround Axis3d.x (Angle.radians (pi / 16))
                |> Physics.moveTo (Point3d.meters 0 -2 1)

        bottom =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 3, Length.meters 6, Length.meters 1 )

        top =
            Block3d.centeredOn
                (Frame3d.atPoint (Point3d.meters 0 1 1))
                ( Length.meters 2, Length.meters 3, Length.meters 1.5 )

        baseBody =
            Physics.dynamic
                [ ( Shape.sum [ Shape.block top, Shape.block bottom ], Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 80)
                |> Physics.moveTo (Point3d.meters 0 0 5)

        sphere3d =
            Sphere3d.atOrigin (Length.meters 1.2)

        wheelBody =
            Physics.sphere sphere3d Material.rubber
                |> Physics.scaleMassTo (Mass.kilograms 2)

        offset =
            Point3d.meters 0 0 5
    in
    [ ( "floor", floorBody )
    , ( "slope", slopeBody )
    , ( "base", baseBody )
    , ( "wheel1"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters 3 3 0)
      )
    , ( "wheel2"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters -3 3 0)
      )
    , ( "wheel3"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters -3 -3 0)
      )
    , ( "wheel4"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters 3 -3 0)
      )
    ]


initialMeshes : Dict String (Mesh Attributes)
initialMeshes =
    let
        slopeBlock3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )

        bottom =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 3, Length.meters 6, Length.meters 1 )

        top =
            Block3d.centeredOn
                (Frame3d.atPoint (Point3d.meters 0 1 1))
                ( Length.meters 2, Length.meters 3, Length.meters 1.5 )

        sphere3d =
            Sphere3d.atOrigin (Length.meters 1.2)

        wheelMesh =
            Meshes.fromTriangles (Meshes.sphere 2 sphere3d)
    in
    Dict.fromList
        [ ( "floor", Meshes.fromTriangles [] )
        , ( "slope", Meshes.fromTriangles (Meshes.block slopeBlock3d) )
        , ( "base", Meshes.fromTriangles (Meshes.block bottom ++ Meshes.block top) )
        , ( "wheel1", wheelMesh )
        , ( "wheel2", wheelMesh )
        , ( "wheel3", wheelMesh )
        , ( "wheel4", wheelMesh )
        ]
