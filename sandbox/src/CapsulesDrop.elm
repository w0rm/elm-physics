module CapsulesDrop exposing (main)

{-| Capsule-vs-Convex collision tester. The scene is laid out as three
rows of independent (box, capsule) pairs:

  - Front row (y = -7) exercises the cap-on-feature branches
    (capsule axis nearly aligned with the separating axis):
    A face, B edge, C vertex, D 30° tilted, E 60° tilted,
    F diagonal cap on vertex (tilted in two axes).

  - Middle row (y = 0) is two STATIC "diamond" blocks rotated so a
    vertex points up: L vertical capsule (cap on vertex) and
    M horizontal capsule (cylinder body on vertex).

  - Back row (y = +7) exercises the cylinder-body branches
    (capsule axis perpendicular to the separating axis):
    G face support, H parallel-edge fallback, I perpendicular over
    edge, J cylinder over a vertex, K face support on a rotated box.

Each capsule is dropped from z = 6 onto its own box so the scenarios
don't interact. The Restart button resets the scene.

-}

import Angle
import Array exposing (Array)
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
import Cylinder3d
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters)
import Mass
import Physics exposing (Body, WorldCoordinates, onEarth)
import Physics.Material as Material
import Physics.Shape as Shape
import Physics.Types exposing (Contacts(..))
import Plane3d
import Point3d exposing (Point3d)
import Task
import WebGL exposing (Mesh)


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
                { from = { x = 0, y = 28, z = 22 }
                , to = { x = 0, y = 0, z = 1 }
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
            ( { model
                | bodies = initialBodies
                , meshes = initialMeshes
                , contacts = Physics.emptyContacts
              }
            , Cmd.none
            )


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


boxBlock3d : Block3d.Block3d Length.Meters Physics.BodyCoordinates
boxBlock3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 2, Length.meters 2, Length.meters 2 )


capsule3d : Cylinder3d.Cylinder3d Length.Meters Physics.BodyCoordinates
capsule3d =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = Length.meters 0.5, length = Length.meters 2.5 }


boxBody : Body
boxBody =
    Physics.block boxBlock3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


staticBoxBody : Body
staticBoxBody =
    Physics.static [ ( Shape.block boxBlock3d, Material.wood ) ]


capsuleBody : Body
capsuleBody =
    Physics.capsule capsule3d Material.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)


{-| Each scenario is a (box, capsule) pair. The two are independent
"islands" — boxes are spaced wide enough that none of the dropped
capsules can interact with a neighbour's box.
-}
type alias Scenario =
    { label : String
    , box : Body
    , capsule : Body
    }


scenarios : List Scenario
scenarios =
    let
        -- Place the dynamic box at (x, y, 0) with no rotation.
        boxAt x y =
            boxBody |> Physics.moveTo (Point3d.meters x y 0)

        -- Place the dynamic box at (x, y, 0) rotated `angleDeg` around +Z
        -- (top face stays flat).
        boxAtRotated x y angleDeg =
            boxBody
                |> Physics.rotateAround Axis3d.z (Angle.degrees angleDeg)
                |> Physics.moveTo (Point3d.meters x y 0)

        -- Static "diamond" block: rotate around +X by 45° then around +Y
        -- by -arctan(1/√2) ≈ -35.264° so the (1,1,1) vertex of the unit
        -- cube ends up pointing along +Z (body diagonal vertical).
        staticBoxVertexUp x y =
            staticBoxBody
                |> Physics.rotateAround Axis3d.x (Angle.degrees 45)
                |> Physics.rotateAround Axis3d.y (Angle.degrees -35.264)
                |> Physics.moveTo (Point3d.meters x y 1)

        -- Drop a vertical capsule (axis +Z) at (x, y, 6).
        capAt x y =
            capsuleBody |> Physics.moveTo (Point3d.meters x y 6)

        -- Drop a capsule rotated `angleDeg` around +X, then placed at (x, y, 6).
        -- A 90° X rotation makes the capsule axis horizontal (along -Y).
        capTiltedX x y angleDeg =
            capsuleBody
                |> Physics.rotateAround Axis3d.x (Angle.degrees angleDeg)
                |> Physics.moveTo (Point3d.meters x y 6)

        -- Drop a capsule rotated 90° around +Y → axis becomes +X.
        capAxisX x y =
            capsuleBody
                |> Physics.rotateAround Axis3d.y (Angle.degrees 90)
                |> Physics.moveTo (Point3d.meters x y 6)
    in
    [ ----------------------------------------------------------------
      -- Row y = -7: cap (endpoint) contacts. Capsule axis ≈ aligned
      -- with the separating axis so the SAT minimum runs through one
      -- of the caps and the contact is generated at that cap.
      ----------------------------------------------------------------
      { label = "A vertical cap → face center"
      , box = boxAt -10 -7
      , capsule = capAt -10 -7
      }
    , { label = "B vertical cap → +x top edge (cap straddles edge)"
      , box = boxAt -6 -7
      , capsule = capAt (-6 + 1) -7
      }
    , { label = "C vertical cap → +x +y top corner (cap on vertex)"
      , box = boxAt -2 -7
      , capsule = capAt (-2 + 1) (-7 + 1)
      }
    , { label = "D 30° tilted cap → face"
      , box = boxAt 2 -7
      , capsule = capTiltedX 2 -7 30
      }
    , { label = "E 60° tilted cap → face (steep tilt, cap heavily off-axis)"
      , box = boxAt 6 -7
      , capsule = capTiltedX 6 -7 60
      }
    , { label = "F diagonal cap (X 35°, Y 25°) → +x +y top corner"

      -- Two-axis tilt so the capsule axis is off both global axes;
      -- positioned over the +x +y top corner so the lower cap lands
      -- on the vertex from a slanted incoming direction.
      , box = boxAt 10 -7
      , capsule =
            capsuleBody
                |> Physics.rotateAround Axis3d.x (Angle.degrees 35)
                |> Physics.rotateAround Axis3d.y (Angle.degrees 25)
                |> Physics.moveTo (Point3d.meters (10 + 1) (-7 + 1) 6)
      }

    ----------------------------------------------------------------
    -- Row y = +7: cylinder-body contacts. Capsule axis ≈ perpendicular
    -- to the separating axis so the cylinder side is what touches the
    -- convex.
    ----------------------------------------------------------------
    , { label = "G horizontal capsule → face center (Face Support, 2 contacts)"
      , box = boxAt -8 7
      , capsule = capTiltedX -8 7 90
      }
    , { label = "H axis ∥ +y top edge, just outside the face (parallel-edge fallback)"
      , box = boxAt -4 7
      , capsule = capAxisX -4 (7 + 1.3)
      }
    , { label = "I axis ⊥ +x top edge, half the cylinder overhanging"
      , box = boxAt 0 7
      , capsule = capTiltedX 0.5 7 90
      }
    , { label = "J horizontal capsule centered over +x +y top corner"
      , box = boxAt 4 7
      , capsule = capTiltedX 5 8 90
      }
    , { label = "K horizontal capsule on box rotated 45° around z (rotated-face support)"
      , box = boxAtRotated 8 7 45
      , capsule = capTiltedX 8 7 90
      }

    ----------------------------------------------------------------
    -- Row y = 0: STATIC vertex-up "diamond" blocks. Tests collision
    -- against a single point feature on a static convex.
    ----------------------------------------------------------------
    , { label = "L vertical capsule (cap) → upward vertex of static diamond"
      , box = staticBoxVertexUp -3 0
      , capsule = capAt -3 0
      }
    , { label = "M horizontal capsule (cylinder) → upward vertex of static diamond"
      , box = staticBoxVertexUp 3 0
      , capsule = capTiltedX 3 0 90
      }
    ]


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters floorOffset)

        n =
            List.length scenarios
    in
    -- Layout: id 0 floor, then n (box, capsule) pairs.
    -- Box ids are 1..n, capsule ids are n+1..2n, paired by index.
    ( 0, floorBody )
        :: List.indexedMap (\i s -> ( i + 1, s.box )) scenarios
        ++ List.indexedMap (\i s -> ( i + 1 + n, s.capsule )) scenarios


initialMeshes : Array (Mesh Attributes)
initialMeshes =
    let
        floorMesh =
            Meshes.fromTriangles []

        boxMesh =
            Meshes.fromTriangles (Meshes.block boxBlock3d)

        capsuleMesh =
            Meshes.fromTriangles (Meshes.capsule 12 capsule3d)

        n =
            List.length scenarios
    in
    Array.fromList
        (floorMesh
            :: List.repeat n boxMesh
            ++ List.repeat n capsuleMesh
        )
