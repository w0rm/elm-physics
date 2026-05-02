module Character2D exposing (main)

{-| 2D character controller demo. Motion is locked to the world XZ
plane: Y translation is locked, and all rotation is locked. The camera
looks straight down the -Y axis, so the simulation reads as a classic
side-scrolling platformer.

Left/Right arrows to move, Space to jump.

-}

import Angle
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Density
import Dict exposing (Dict)
import Duration
import Force
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode
import Length exposing (Meters)
import Mass
import Physics exposing (Body, BodyCoordinates, onEarth)
import Physics.Lock as Lock
import Physics.Material as Material exposing (Material)
import Physics.Shape as Shape
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d exposing (Point3d)
import Quantity
import Sphere3d exposing (Sphere3d)
import Task
import Vector3d
import WebGL exposing (Mesh)


moveAcceleration : Float
moveAcceleration =
    35


jumpSpeed : Float
jumpSpeed =
    4


playerMaterial : Material Material.Dense
playerMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.2
        , bounciness = 0
        }


stairMaterial : Material Material.Dense
stairMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.02
        , bounciness = 0
        }


boxMaterial : Material Material.Dense
boxMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.4
        , bounciness = 0
        }


{-| Lock the sphere to the XZ plane: no Y motion, no rotation.
-}
planarLock : List Lock.Lock
planarLock =
    Lock.translateY :: Lock.allRotation


type alias Model =
    { bodies : List ( String, Body )
    , meshes : Dict String (Mesh Attributes)
    , contacts : Physics.Contacts String
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , rightInput : Float
    , grounded : Bool
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart
    | KeyDown Key
    | KeyUp Key


type Key
    = KeyLeft
    | KeyRight
    | KeyJump


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
      , settings = { settings | showSettings = True }
      , camera =
            Camera.camera
                { from = { x = 0, y = -14, z = 2 }
                , to = { x = 0, y = 0, z = 1 }
                }
      , rightInput = 0
      , grounded = False
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ForSettings settingsMsg ->
            { model | settings = Settings.update settingsMsg model.settings }

        Tick dt ->
            let
                bodiesWithInput =
                    if model.grounded then
                        List.map
                            (\( id, body ) ->
                                if id == "player" then
                                    ( id, drivePlayer dt model.rightInput body )

                                else
                                    ( id, body )
                            )
                            model.bodies

                    else
                        model.bodies

                ( newBodies, newContacts ) =
                    Physics.simulate
                        { onEarth | contacts = model.contacts }
                        bodiesWithInput
            in
            { model
                | fps = Fps.update dt model.fps
                , bodies = newBodies
                , contacts = newContacts
                , grounded = playerGrounded newBodies newContacts
                , camera = followPlayer newBodies model.camera
            }

        Resize width height ->
            { model | camera = Camera.resize width height model.camera }

        Restart ->
            { model
                | bodies = initialBodies
                , contacts = Physics.emptyContacts
            }

        KeyDown KeyLeft ->
            { model | rightInput = -1 }

        KeyDown KeyRight ->
            { model | rightInput = 1 }

        KeyDown KeyJump ->
            if model.grounded then
                { model
                    | bodies =
                        List.map
                            (\( id, body ) ->
                                if id == "player" then
                                    ( id, jumpPlayer body )

                                else
                                    ( id, body )
                            )
                            model.bodies
                    , grounded = False
                }

            else
                model

        KeyUp KeyLeft ->
            { model
                | rightInput =
                    if model.rightInput == -1 then
                        0

                    else
                        model.rightInput
            }

        KeyUp KeyRight ->
            { model
                | rightInput =
                    if model.rightInput == 1 then
                        0

                    else
                        model.rightInput
            }

        KeyUp KeyJump ->
            model


drivePlayer : Float -> Float -> Body -> Body
drivePlayer dtMs right body =
    if right == 0 then
        body

    else
        let
            duration =
                Duration.milliseconds dtMs

            xImpulse =
                Quantity.times duration (Force.newtons (right * moveAcceleration * playerMass))

            impulse =
                Vector3d.xyz xImpulse Quantity.zero Quantity.zero
        in
        Physics.applyImpulse impulse (Physics.originPoint body) body


jumpPlayer : Body -> Body
jumpPlayer body =
    let
        impulse =
            Vector3d.xyz
                Quantity.zero
                Quantity.zero
                (Quantity.times (Duration.seconds 1) (Force.newtons (playerMass * jumpSpeed)))
    in
    Physics.applyImpulse impulse (Physics.originPoint body) body


{-| Track the player's X with the camera, keeping the Y offset and Z
height that were configured in `init`.
-}
followPlayer : List ( String, Body ) -> Camera -> Camera
followPlayer bodies currentCamera =
    case List.filter (\( id, _ ) -> id == "player") bodies of
        [] ->
            currentCamera

        ( _, player ) :: _ ->
            let
                origin =
                    Physics.originPoint player

                playerX =
                    Length.inMeters (Point3d.xCoordinate origin)

                from =
                    currentCamera.from

                to =
                    currentCamera.to

                rebuilt =
                    Camera.camera
                        { from = { from | x = playerX }
                        , to = { to | x = playerX }
                        }
            in
            Camera.resize currentCamera.width currentCamera.height rebuilt


playerGrounded : List ( String, Body ) -> Physics.Contacts String -> Bool
playerGrounded bodies contacts =
    case List.filter (\( id, _ ) -> id == "player") bodies of
        [] ->
            False

        ( _, player ) :: _ ->
            let
                playerZ =
                    Length.inMeters
                        (Point3d.zCoordinate (Physics.originPoint player))

                threshold =
                    playerZ - 0.1
            in
            Physics.contactPoints
                (\a b -> a == "player" || b == "player")
                contacts
                |> List.concatMap (\( _, _, pts ) -> pts)
                |> List.any
                    (\pt -> Length.inMeters (Point3d.zCoordinate pt) < threshold)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        ]


keyDecoder : (Key -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case String.toLower key of
                    "arrowleft" ->
                        Json.Decode.succeed (toMsg KeyLeft)

                    "arrowright" ->
                        Json.Decode.succeed (toMsg KeyRight)

                    " " ->
                        Json.Decode.succeed (toMsg KeyJump)

                    _ ->
                        Json.Decode.fail "ignored key"
            )


view : Model -> Html Msg
view { settings, fps, bodies, meshes, contacts, camera } =
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


floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = 0 }


playerMass : Float
playerMass =
    5


playerSphere : Sphere3d Meters BodyCoordinates
playerSphere =
    Sphere3d.atOrigin (Length.meters 0.4)


boxBlock : Block3d Meters BodyCoordinates
boxBlock =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 0.5, Length.meters 0.5, Length.meters 0.3 )


{-| A long, thin board — a wooden plank laid flat along the body's X
axis. Rotated around Y in world space to form a ramp.
-}
plankBlock : Block3d Meters BodyCoordinates
plankBlock =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 4, Length.meters 0.6, Length.meters 0.1 )


{-| Tilt angle of the plank ramp. Negative so the left end (where the
player approaches) rests near the floor and the right end rises.
-}
plankTilt : Angle.Angle
plankTilt =
    Angle.degrees -20


{-| Step blocks for a linear staircase, in body-local coordinates.
Each block is a slab that runs from x = level to x = level + 1 along
the X axis, with Y thickness centered at 0. Walking right (+X) takes
you up to the top platform.
-}
stairBlocks : List (Block3d Meters BodyCoordinates)
stairBlocks =
    let
        stepHeight =
            0.2

        stepDepth =
            0.5

        width =
            1.5

        topPlatformDepth =
            1.2

        numSteps =
            4
    in
    List.map
        (\level ->
            let
                xMin =
                    toFloat level * stepDepth

                xMax =
                    xMin
                        + (if level == numSteps then
                            topPlatformDepth

                           else
                            stepDepth
                          )

                zMax =
                    toFloat (level + 1) * stepHeight
            in
            Block3d.from
                (Point3d.meters xMin (-width / 2) 0)
                (Point3d.meters xMax (width / 2) zMax)
        )
        (List.range 0 4)


initialBodies : List ( String, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        player =
            Physics.sphere playerSphere playerMaterial
                |> Physics.scaleMassTo (Mass.kilograms playerMass)
                |> Physics.moveTo (Point3d.meters -4 0 0.4)
                |> Physics.lock planarLock

        boxAt x z =
            Physics.block boxBlock boxMaterial
                |> Physics.scaleMassTo (Mass.kilograms 5)
                |> Physics.moveTo (Point3d.meters x 0 z)
                |> Physics.lock planarLock

        boxes =
            List.indexedMap
                (\idx ( x, z ) -> ( "box-" ++ String.fromInt idx, boxAt x z ))
                [ ( -1.5, 0.15 )
                , ( 5, 0.15 )
                , ( 5, 0.45 )
                , ( 11, 0.15 )
                ]

        stairsBody =
            Physics.static
                (List.map (\b -> ( Shape.block b, stairMaterial )) stairBlocks)
                |> Physics.moveTo (Point3d.meters 0 0 0)

        plankBody =
            Physics.static [ ( Shape.block plankBlock, Material.wood ) ]
                |> Physics.rotateAround Axis3d.y plankTilt
                |> Physics.moveTo (Point3d.meters 8 0 0.8)
    in
    ( "floor", floorBody )
        :: ( "player", player )
        :: ( "stairs", stairsBody )
        :: ( "plank", plankBody )
        :: boxes


initialMeshes : Dict String (Mesh Attributes)
initialMeshes =
    let
        playerMesh =
            Meshes.fromTriangles (Meshes.sphere 3 playerSphere)

        boxMesh =
            Meshes.fromTriangles (Meshes.block boxBlock)

        floorMesh =
            Meshes.fromTriangles []

        stairsMesh =
            Meshes.fromTriangles (List.concatMap Meshes.block stairBlocks)

        plankMesh =
            Meshes.fromTriangles (Meshes.block plankBlock)

        boxKeys =
            List.range 0 3
                |> List.map (\i -> "box-" ++ String.fromInt i)
    in
    Dict.fromList
        (( "floor", floorMesh )
            :: ( "player", playerMesh )
            :: ( "stairs", stairsMesh )
            :: ( "plank", plankMesh )
            :: List.map (\k -> ( k, boxMesh )) boxKeys
        )
