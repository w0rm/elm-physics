module Character exposing (main)

{-| Character controller demo, modelled directly on cannon-es's
PointerLockControlsCannon: sphere body, mild friction, high linear
damping, additive velocity from input. Rotation is locked via
[Physics.lock](Physics#lock) so the sphere slides instead of rolling.

Arrow keys to move, Space to jump.

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
import Density
import Dict exposing (Dict)
import Direction3d
import Duration
import Force
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode
import Length exposing (Meters)
import Mass
import Physics exposing (Body, BodyCoordinates, WorldCoordinates, onEarth)
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


{-| Acceleration applied per held input direction, in m/s². With
`linearDamping = 0.9` this settles around 3 m/s — a brisk walking pace
at this scene scale.
-}
moveAcceleration : Float
moveAcceleration =
    35


{-| Initial vertical speed when jumping, in m/s. Under `onEarth` gravity
(~9.8 m/s²), this peaks about 0.8 m above takeoff — enough to clear two
steps of the staircase.
-}
jumpSpeed : Float
jumpSpeed =
    4


{-| Player material with mild friction so the sphere can grip steps and
edges instead of sliding back. Combined with the floor's friction via
the geometric mean (√(0.3·0.4) ≈ 0.35), this is enough to hold position
on stairs but still slippery enough to feel responsive.
-}
playerMaterial : Material Material.Dense
playerMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.2
        , bounciness = 0
        }


{-| Slippery material for the staircase — combined with the player's 0.2
friction via √(0.2·0.02) ≈ 0.063, so the sphere can slide up step edges
without getting caught by tangential friction at the corner contact.
-}
stairMaterial : Material Material.Dense
stairMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.02
        , bounciness = 0
        }


{-| Crate material with the same friction as the floor.
-}
boxMaterial : Material Material.Dense
boxMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.4
        , bounciness = 0
        }


type alias Model =
    { bodies : List ( String, Body )
    , meshes : Dict String (Mesh Attributes)
    , contacts : Physics.Contacts String
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , forwardInput : Float
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
    = KeyForward
    | KeyBack
    | KeyLeft
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
                { from = { x = 0, y = -12, z = 8 }
                , to = { x = 0, y = 0, z = 0.5 }
                }
      , forwardInput = 0
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
                                    ( id, drivePlayer dt model.rightInput model.forwardInput body )

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
            }

        Resize width height ->
            { model | camera = Camera.resize width height model.camera }

        Restart ->
            { model
                | bodies = initialBodies
                , contacts = Physics.emptyContacts
            }

        KeyDown KeyForward ->
            { model | forwardInput = 1 }

        KeyDown KeyBack ->
            { model | forwardInput = -1 }

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

        KeyUp KeyForward ->
            { model
                | forwardInput =
                    if model.forwardInput == 1 then
                        0

                    else
                        model.forwardInput
            }

        KeyUp KeyBack ->
            { model
                | forwardInput =
                    if model.forwardInput == -1 then
                        0

                    else
                        model.forwardInput
            }

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


{-| Add horizontal velocity each frame via an impulse, matching cannon's
`velocity.x += inputVelocity.x` pattern. Vertical velocity is left to
gravity and contacts so contact rebounds don't get fed back into input.
-}
drivePlayer : Float -> Float -> Float -> Body -> Body
drivePlayer dtMs right forward body =
    if right == 0 && forward == 0 then
        body

    else
        let
            duration =
                Duration.milliseconds dtMs

            xImpulse =
                Quantity.times duration (Force.newtons (right * moveAcceleration * playerMass))

            yImpulse =
                Quantity.times duration (Force.newtons (forward * moveAcceleration * playerMass))

            impulse =
                Vector3d.xyz xImpulse yImpulse Quantity.zero
        in
        Physics.applyImpulse impulse (Physics.originPoint body) body


{-| Apply an upward impulse that produces `jumpSpeed` of vertical velocity
on a body at rest (impulse = mass × Δv).
-}
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


{-| The player is grounded if any of its contact points sits below its
center. Works for flat floor AND step edges (which contact higher up on
the sphere than floor contacts). A small 0.1 m offset keeps pure-wall
contacts (at the sphere's equator) from falsely grounding the player.
-}
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
                    "arrowup" ->
                        Json.Decode.succeed (toMsg KeyForward)

                    "arrowdown" ->
                        Json.Decode.succeed (toMsg KeyBack)

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
        ( Length.meters 0.5, Length.meters 0.5, Length.meters 0.8 )


{-| Step blocks for a linear staircase, in body-local coordinates.
Each block is a slab that runs from y = level to y = level + 1 and is
tall enough to support every step in front of it. Walking forward (+Y)
takes you up to the top platform.
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
                yMin =
                    toFloat level * stepDepth

                yMax =
                    yMin
                        + (if level == numSteps then
                            topPlatformDepth

                           else
                            stepDepth
                          )

                zMax =
                    toFloat (level + 1) * stepHeight
            in
            Block3d.from
                (Point3d.meters (-width / 2) yMin 0)
                (Point3d.meters (width / 2) yMax zMax)
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
                |> Physics.moveTo (Point3d.meters 0 -4 0.4)
                |> Physics.lock Lock.allRotation

        boxAt x y =
            Physics.block boxBlock boxMaterial
                |> Physics.scaleMassTo (Mass.kilograms 5)
                |> Physics.moveTo (Point3d.meters x y 0.4)

        boxes =
            List.indexedMap
                (\idx ( x, y ) -> ( "box-" ++ String.fromInt idx, boxAt x y ))
                [ ( -3, -2 )
                , ( -3, 0 )
                , ( -3, 2 )
                , ( -4, 1 )
                , ( 3, -2 )
                , ( 3, 0 )
                , ( 3, 2 )
                , ( 4, 1 )
                ]

        stairsBody =
            Physics.static
                (List.map (\b -> ( Shape.block b, stairMaterial )) stairBlocks)
                |> Physics.moveTo (Point3d.meters 0 1 0)
    in
    ( "floor", floorBody )
        :: ( "player", player )
        :: ( "stairs", stairsBody )
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

        boxKeys =
            List.range 0 7
                |> List.map (\i -> "box-" ++ String.fromInt i)
    in
    Dict.fromList
        (( "floor", floorMesh )
            :: ( "player", playerMesh )
            :: ( "stairs", stairsMesh )
            :: List.map (\k -> ( k, boxMesh )) boxKeys
        )
