module Cloth exposing (main)

{-| Cloth simulation built using many particle bodies,
connected with distance constraints.
-}

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics exposing (Body, onEarth)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.Material as Material
import Point3d exposing (Point3d)
import Sphere3d
import Task
import WebGL exposing (Mesh)


particlesPerDimension : Int
particlesPerDimension =
    10


distanceBetweenParticles : Float
distanceBetweenParticles =
    0.5


{-| IDs:

  - 0 = floor
  - 1 = sphere
  - 2 + x \* particlesPerDimension + y = particle at (x, y)

-}
particleId : Int -> Int -> Int
particleId x y =
    2 + x * particlesPerDimension + y


type alias Model =
    { bodies : List ( Int, Body )
    , meshes : Array (Mesh Attributes)
    , contacts : List ( Int, Int, List (Point3d Meters WorldCoordinates) )
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
      , contacts = []
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
                        { onEarth | constrain = constrainCloth }
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
            ( { model | bodies = initialBodies }, Cmd.none )


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
            , bodies = List.filterMap (\( id, body ) -> Maybe.map (\mesh -> ( mesh, body )) (Array.get id meshes)) bodies
            , contacts = List.concatMap (\( _, _, c ) -> c) contacts
            , camera = camera
            , floorOffset = floorOffset
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length bodies)

          else
            Html.text ""
        ]


{-| Set up constraints between adjacent particles using id arithmetic.
Particles have IDs: 2 + x \* n + y where n = particlesPerDimension.
Two particles are horizontally adjacent if they share the same x and differ by 1 in y.
Two particles are vertically adjacent if they share the same y and differ by n in id.
-}
constrainCloth : Int -> Maybe (Int -> List Constraint)
constrainCloth id1 =
    let
        n =
            particlesPerDimension

        firstParticleId =
            2

        lastParticleId =
            2 + n * n - 1
    in
    if id1 < firstParticleId || id1 > lastParticleId then
        Nothing

    else
        Just
            (\id2 ->
                if id2 < firstParticleId || id2 > lastParticleId then
                    []

                else
                    let
                        -- Convert IDs back to (x, y) coordinates
                        idx1 =
                            id1 - firstParticleId

                        idx2 =
                            id2 - firstParticleId

                        x1 =
                            idx1 // n

                        y1 =
                            modBy n idx1

                        x2 =
                            idx2 // n

                        y2 =
                            modBy n idx2
                    in
                    if x1 == x2 && y2 - y1 == 1 || y1 == y2 && x2 - x1 == 1 then
                        [ Constraint.distance (Length.meters distanceBetweenParticles) ]

                    else
                        []
            )


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        sphere3d =
            Sphere3d.atOrigin (Length.meters 2)

        sphereBody =
            Physics.sphere sphere3d Material.wood
                |> Physics.scaleTo (Mass.kilograms 5)
                |> Physics.moveTo (Point3d.meters 0 0 1)

        dimensions =
            List.range 0 (particlesPerDimension - 1)

        particleBody =
            Physics.particle (Mass.kilograms 5) (Material.surface { friction = 0.3, bounciness = 0 })

        particles =
            List.concatMap
                (\x ->
                    List.map
                        (\y ->
                            ( particleId x y
                            , particleBody
                                |> Physics.moveTo
                                    (Point3d.meters
                                        ((toFloat x - (toFloat particlesPerDimension - 1) / 2) * distanceBetweenParticles)
                                        ((toFloat y - (toFloat particlesPerDimension - 1) / 2) * distanceBetweenParticles)
                                        8
                                    )
                            )
                        )
                        dimensions
                )
                dimensions
    in
    ( 0, floorBody ) :: ( 1, sphereBody ) :: particles


initialMeshes : Array (Mesh Attributes)
initialMeshes =
    let
        sphere3d =
            Sphere3d.atOrigin (Length.meters 2)

        particleSphere3d =
            Sphere3d.atOrigin (Length.meters 0.1)

        -- Total size: 2 (floor + sphere) + n*n particles
        particleCount =
            particlesPerDimension * particlesPerDimension

        particleMesh =
            Meshes.fromTriangles (Meshes.sphere 1 particleSphere3d)
    in
    Array.fromList
        (Meshes.fromTriangles []
            :: Meshes.fromTriangles (Meshes.sphere 3 sphere3d)
            :: List.repeat particleCount particleMesh
        )
