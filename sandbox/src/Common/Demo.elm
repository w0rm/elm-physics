module Common.Demo exposing
    ( Config
    , Model
    , Msg
    , Vec3R
    , button
    , defaults
    , floorAtZero
    , floorZ
    , program
    )

{-| A `Browser.element` builder for sandbox demos. Every demo follows
the same shape:

  - drop bodies into a 3D scene
  - render with `Common.Scene`
  - tick at a fixed timestep with `Physics.simulate`
  - show settings + FPS + SPS overlays
  - orbit the camera with the mouse

This module wires all of that up. Per-demo customization happens through
the `Config` record: every field has a sensible default via `defaults`,
and demos override only the bits they care about.

Conventions baked in:

  - left mouse drags orbit the camera, wheel zooms
  - the settings panel hosts a "Restart the demo" button followed by
    any demo-specific buttons
  - FPS/SPS show in the top-left so they don't collide with the
    right-anchored settings panel

-}

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.ContactLabels as ContactLabels
import Common.Contacts as Contacts
import Common.Fps as Fps
import Common.Meshes exposing (Meshes)
import Common.Orbit as Orbit exposing (Orbit)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Common.Sps as Sps exposing (Sps)
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events
import Physics exposing (Body, onEarth)
import Physics.Constraint exposing (Constraint)
import Physics.Types exposing (Contacts(..))
import Speed
import Task
import Timestep exposing (Timestep)
import Vector3d


type alias Vec3R =
    { x : Float, y : Float, z : Float }



-- CONFIG


{-| The demo configuration. Build one with `defaults` and then override
the fields you need.
-}
type alias Config id state msg =
    { initialBodies : List ( id, Body )
    , lookupMesh : state -> id -> Maybe Meshes
    , camera : { from : Vec3R, to : Vec3R }
    , floorOffset : Vec3R
    , contactRadius : Float
    , timestep : { duration : Duration, maxSteps : Int }
    , solverIterations : Int
    , initialState : state
    , constrain :
        state
        -> List ( id, Body )
        -> id
        -> Maybe (id -> List Constraint)
    , collide : state -> id -> id -> Bool
    , preSimulate :
        Duration
        -> state
        -> List ( id, Body )
        -> ( state, List ( id, Body ) )
    , postSimulate :
        Duration
        -> List ( id, Body )
        -> Physics.Contacts id
        -> state
        -> state
    , cameraTarget : state -> List ( id, Body ) -> Maybe Vec3R
    , reset : state -> state
    , restartBodies : state -> List ( id, Body )
    , settingsInit : Settings -> Settings
    , update :
        msg
        -> state
        -> List ( id, Body )
        -> ( state, List ( id, Body ), Cmd msg )
    , subscriptions : state -> Sub msg
    , buttons : state -> List (Html msg)
    , overlay :
        state -> List ( id, Body ) -> Physics.Contacts id -> Html msg
    , controls : List String
    }


{-| Default configuration. Pass in the four "essential" fields; every
other hook is a no-op.

The defaults set `showFpsMeter = True` so visitors immediately see the
counter rather than having to open settings to find it.

-}
defaults :
    { initialBodies : List ( id, Body )
    , lookupMesh : state -> id -> Maybe Meshes
    , camera : { from : Vec3R, to : Vec3R }
    , initialState : state
    }
    -> Config id state msg
defaults base =
    { initialBodies = base.initialBodies
    , lookupMesh = base.lookupMesh
    , camera = base.camera
    , floorOffset = floorZ
    , contactRadius = 0.07
    , timestep = { duration = Duration.seconds (1 / 120), maxSteps = 2 }
    , solverIterations = 10
    , initialState = base.initialState
    , constrain = \_ _ _ -> Nothing
    , collide = \_ _ _ -> True
    , preSimulate = \_ s b -> ( s, b )
    , postSimulate = \_ _ _ s -> s
    , cameraTarget = \_ _ -> Nothing
    , reset = \_ -> base.initialState
    , restartBodies = \_ -> base.initialBodies
    , settingsInit = identity
    , update = \_ s b -> ( s, b, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , buttons = \_ -> []
    , overlay = \_ _ _ -> Html.text ""
    , controls = []
    }


floorZ : Vec3R
floorZ =
    { x = 0, y = 0, z = -1 }


floorAtZero : Vec3R
floorAtZero =
    { x = 0, y = 0, z = 0 }



-- MODEL


type alias Model id state =
    { prevBodies : List ( id, Body )
    , bodies : List ( id, Body )
    , contacts : Physics.Contacts id
    , fps : List Float
    , settings : Settings
    , camera : Camera
    , orbit : Orbit
    , timestep : Timestep
    , sps : Sps
    , frame : Int
    , state : state
    }


type Msg msg
    = ForSettings SettingsMsg
    | Tick Duration
    | Resize Float Float
    | Restart
    | ForOrbit Orbit.Msg
    | Custom msg



-- PROGRAM


program : Config id state msg -> Program () (Model id state) (Msg msg)
program config =
    Browser.element
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        , view = view config
        }


init : Config id state msg -> () -> ( Model id state, Cmd (Msg msg) )
init config _ =
    ( { prevBodies = config.initialBodies
      , bodies = config.initialBodies
      , contacts = Physics.emptyContacts
      , fps = []
      , settings = config.settingsInit settings
      , camera = Camera.camera config.camera
      , orbit = Orbit.fromCartesian config.camera
      , timestep = Timestep.init config.timestep
      , sps = Sps.init
      , frame = 0
      , state = config.initialState
      }
    , Task.perform (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )



-- UPDATE


update :
    Config id state msg
    -> Msg msg
    -> Model id state
    -> ( Model id state, Cmd (Msg msg) )
update config msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model | settings = Settings.update settingsMsg model.settings }
            , Cmd.none
            )

        Tick dt ->
            let
                next =
                    Timestep.advance (simulateStep config) dt model
            in
            ( { next
                | fps = Fps.update dt next.fps
                , sps = Sps.update dt (Timestep.steps next.timestep) next.sps
              }
            , Cmd.none
            )

        Resize w h ->
            ( { model | camera = Camera.resize w h model.camera }
            , Cmd.none
            )

        Restart ->
            let
                -- Reset state (demos that track a selection, e.g. a scene
                -- dropdown, keep it here), then reload the bodies that match
                -- the reset state. Defaults reset to the initial state and
                -- bodies, so this restarts whatever the demo currently shows.
                newState =
                    config.reset model.state

                bodies =
                    config.restartBodies newState
            in
            ( { model
                | prevBodies = bodies
                , bodies = bodies
                , contacts = Physics.emptyContacts
                , state = newState
                , frame = 0
              }
            , Cmd.none
            )

        ForOrbit orbitMsg ->
            let
                newOrbit =
                    Orbit.update orbitMsg model.orbit
            in
            ( { model
                | orbit = newOrbit
                , camera = Orbit.toCamera newOrbit model.camera
              }
            , Cmd.none
            )

        Custom userMsg ->
            let
                ( newState, newBodies, cmd ) =
                    config.update userMsg model.state model.bodies
            in
            ( { model
                | state = newState
                , bodies = newBodies
                , prevBodies = newBodies
              }
            , Cmd.map Custom cmd
            )


simulateStep : Config id state msg -> Model id state -> Model id state
simulateStep config model =
    let
        d =
            Timestep.duration model.timestep

        ( preState, preBodies ) =
            config.preSimulate d model.state model.bodies

        ( newBodies, newContacts ) =
            Physics.simulate
                { onEarth
                    | duration = d
                    , contacts = model.contacts
                    , constrain = config.constrain preState preBodies
                    , collide = config.collide preState
                    , solverIterations = config.solverIterations
                }
                preBodies

        newState =
            config.postSimulate d newBodies newContacts preState

        ( newOrbit, newCamera ) =
            case config.cameraTarget newState newBodies of
                Just target ->
                    let
                        updatedOrbit =
                            Orbit.setTarget target model.orbit
                    in
                    ( updatedOrbit, Orbit.toCamera updatedOrbit model.camera )

                Nothing ->
                    ( model.orbit, model.camera )
    in
    { model
        | prevBodies = model.bodies
        , bodies = newBodies
        , contacts = newContacts
        , state = newState
        , orbit = newOrbit
        , camera = newCamera
        , frame = model.frame + 1
    }



-- SUBSCRIPTIONS


subscriptions : Config id state msg -> Model id state -> Sub (Msg msg)
subscriptions config model =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , Orbit.subscriptions ForOrbit model.orbit
        , Sub.map Custom (config.subscriptions model.state)
        ]



-- VIEW


view : Config id state msg -> Model id state -> Html (Msg msg)
view config model =
    Html.div
        [ Html.Events.preventDefaultOn "wheel" (Orbit.wheelDecoder ForOrbit) ]
        [ Scene.view
            { settings = model.settings
            , bodies =
                Scene.interpolatedBodies
                    (Timestep.progress model.timestep)
                    model.prevBodies
                    model.bodies
                    (config.lookupMesh model.state)
            , contacts =
                List.concatMap (\( _, _, c ) -> c)
                    (Physics.contactPoints (\_ _ -> True) model.contacts)
            , camera = model.camera
            , contactRadius = config.contactRadius
            , floorOffset = config.floorOffset
            }
        , Html.map Custom
            (config.overlay model.state model.bodies model.contacts)
        , if model.settings.debugContacts && model.settings.debugContactIds then
            ContactLabels.view model.camera (Contacts.worldPoints model.contacts)

          else
            Html.text ""
        , let
            (Contacts c) =
                model.contacts
          in
          Settings.view ForSettings
            model.settings
            [ statsPanel
                { fps = Fps.fps model.fps
                , sps = Sps.sps model.sps
                , bodies = List.length model.bodies
                , iterations = c.iterations
                , frame = model.frame
                , maxV = maxBodySpeed model.bodies
                }
            ]
            (List.map (Html.map Custom) (config.buttons model.state)
                ++ (button Restart "Restart the demo" :: controlsView config.controls)
            )
        ]


statsPanel :
    { fps : Float, sps : Int, bodies : Int, iterations : Int, frame : Int, maxV : Float }
    -> Html msg
statsPanel { fps, sps, bodies, iterations, frame, maxV } =
    Html.div
        [ Attributes.style "display" "grid"
        , Attributes.style "grid-template-columns" "auto 1fr"
        , Attributes.style "column-gap" "24px"
        , Attributes.style "row-gap" "2px"
        , Attributes.style "font-variant-numeric" "tabular-nums"
        , Attributes.style "opacity" "0.85"
        ]
        [ statsPair "f" (String.fromInt frame)
        , statsPair "bodies" (String.fromInt bodies)
        , statsPair "fps" (String.fromInt (round fps))
        , statsPair "iterations" (String.fromInt iterations)
        , statsPair "sps" (String.fromInt sps)
        , statsPair "maxV" (formatSpeed maxV)
        ]


{-| Largest linear speed (m/s) over the dynamic bodies, for the stats panel.
-}
maxBodySpeed : List ( id, Body ) -> Float
maxBodySpeed bodies =
    maxBodySpeedHelp bodies 0


maxBodySpeedHelp : List ( id, Body ) -> Float -> Float
maxBodySpeedHelp bodies acc =
    case bodies of
        [] ->
            acc

        ( _, body ) :: rest ->
            case Physics.mass body of
                Just _ ->
                    maxBodySpeedHelp rest
                        (max acc (Speed.inMetersPerSecond (Vector3d.length (Physics.velocity body))))

                Nothing ->
                    maxBodySpeedHelp rest acc


{-| Fixed 5-decimal speed, always x.xxxxx (keeps trailing zeros) so the
readout doesn't jitter in width as the value settles.
-}
formatSpeed : Float -> String
formatSpeed v =
    let
        scaled =
            round (abs v * 100000)

        intPart =
            scaled // 100000

        fracPart =
            modBy 100000 scaled

        sign =
            if v < 0 then
                "-"

            else
                ""
    in
    sign
        ++ String.fromInt intPart
        ++ "."
        ++ String.padLeft 5 '0' (String.fromInt fracPart)


statsPair : String -> String -> Html msg
statsPair label value =
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "justify-content" "space-between"
        , Attributes.style "gap" "8px"
        ]
        [ Html.span [ Attributes.style "opacity" "0.7" ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


controlsView : List String -> List (Html msg)
controlsView lines =
    case lines of
        [] ->
            []

        _ ->
            [ Html.div
                [ Attributes.style "margin" "10px 0 0"
                , Attributes.style "opacity" "0.8"
                ]
                (List.map (\l -> Html.div [] [ Html.text l ]) lines)
            ]



-- BUTTON HELPERS


{-| Settings-panel button with the shared look. Use for any demo
control that fires a single message on click.
-}
button : msg -> String -> Html msg
button msg label =
    Html.button
        [ Html.Events.onClick msg
        , Attributes.style "display" "block"
        , Attributes.style "width" "100%"
        , Attributes.style "padding" "6px"
        , Attributes.style "margin" "0"
        , Attributes.style "border" "none"
        , Attributes.style "color" "inherit"
        , Attributes.style "font" "inherit"
        , Attributes.style "text-align" "center"
        , Attributes.style "background" "rgb(61, 61, 61)"
        , Attributes.style "cursor" "pointer"
        ]
        [ Html.text label ]
