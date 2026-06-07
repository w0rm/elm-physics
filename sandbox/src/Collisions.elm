module Collisions exposing (main)

{-| Manual collision tester. A static box sits at the origin and a
"controlled" shape (capsule, box, sphere, or cylinder) is moved with
the keyboard. Each frame the world-space shapes are fed straight into
the appropriate `Collision.*` function and the resulting contacts are
rendered as red dots (one at `pi`, one at `pj`).

Controls:

  - W/A/S/D translates the controlled shape in the screen-space plane
  - Q/E rotates it around the view ray
  - Buttons in the top-right switch the controlled shape
  - Camera: drag with the left mouse to orbit, wheel to zoom

-}

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Collision.CapsuleConvex as CapsuleConvex
import Collision.ConvexConvex as ConvexConvex
import Collision.SphereConvex as SphereConvex
import Common.Camera as Camera exposing (Camera)
import Common.ContactLabels as ContactLabels
import Common.Demo as Demo
import Common.Meshes as Meshes
import Common.Orbit as Orbit exposing (Orbit)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Cylinder3d
import Direction3d
import Duration exposing (Duration)
import File.Download
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Transform3d as Transform3d
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Obj.Decode
import Physics exposing (Body, BodyCoordinates, WorldCoordinates)
import Physics.Material as Material
import Physics.Shape as PhysicsShape
import Point3d
import Set exposing (Set)
import Shapes.Capsule as ShapesCapsule
import Shapes.Convex as ShapesConvex
import Shapes.Sphere as ShapesSphere
import Sphere3d
import Task
import TriangularMesh
import Vector3d



-- Shape options


type ControlledShape
    = ShapeCapsule
    | ShapeBox
    | ShapeSphere
    | ShapeCylinder
    | ShapeUnsafeConvexBox
    | ShapeUnsafeConvexSphere
    | ShapeOverCount


allShapes : List ControlledShape
allShapes =
    [ ShapeCapsule, ShapeBox, ShapeSphere, ShapeCylinder, ShapeUnsafeConvexBox, ShapeUnsafeConvexSphere, ShapeOverCount ]


shapeName : ControlledShape -> String
shapeName s =
    case s of
        ShapeCapsule ->
            "Capsule × Box"

        ShapeBox ->
            "Box × Box"

        ShapeSphere ->
            "Sphere × Box"

        ShapeCylinder ->
            "Cylinder × Box"

        ShapeUnsafeConvexBox ->
            "Convex Box × Box"

        ShapeUnsafeConvexSphere ->
            "Convex Sphere × Box"

        ShapeOverCount ->
            "Box × Box (over-count)"


shapeFromName : String -> Maybe ControlledShape
shapeFromName name =
    allShapes |> List.filter (\s -> shapeName s == name) |> List.head



-- Geometry constants


targetBoxSize : Float
targetBoxSize =
    2.0


capsuleRadius : Float
capsuleRadius =
    0.5


capsuleHalfLength : Float
capsuleHalfLength =
    1.25


controlledBoxSize : Float
controlledBoxSize =
    1.5


sphereRadius : Float
sphereRadius =
    0.8


cylinderRadius : Float
cylinderRadius =
    0.5


cylinderLength : Float
cylinderLength =
    2.5


cylinderSubdivisions : Int
cylinderSubdivisions =
    16



-- Model


type alias Pose =
    Frame3d Meters WorldCoordinates { defines : BodyCoordinates }


initialPose : Pose
initialPose =
    -- Lower endpoint just past the +y face — exercises the closest-edge
    -- fallback in CapsuleConvex (single `c-e-fF` contact).
    Frame3d.atOrigin
        |> Frame3d.translateBy (Vector3d.fromMeters { x = 0, y = 1.05, z = 2.25 })


{-| A controlled box stacked on the (same-size) target at the exact relative pose
where a settling stack first hits the coplanar over-count: faces near-coplanar
with ~1e-4 settling drift, so incident vertices straddle the reference edge
planes (the Sutherland-Hodgman on-plane degenerate). Captured from a 2.0-box
stack at the frame its manifold went from 4 to 6 points. Against the target at
origin the contact sits at the camera focus.
-}
overCountPose : Pose
overCountPose =
    Frame3d.unsafe
        { originPoint = Point3d.fromMeters { x = -0.00009961040112628581, y = -0.000092327265447298, z = 1.9999167249387462 }
        , xDirection = Direction3d.unsafe { x = 0.9999999948374431, y = -6.088964405683033e-7, z = 0.00010161074327523221 }
        , yDirection = Direction3d.unsafe { x = 5.995018051587648e-7, y = 0.9999999957256615, z = 0.00009245711229986758 }
        , zDirection = Direction3d.unsafe { x = -0.00010161079913772008, y = -0.00009245705090672844, z = 0.9999999905634696 }
        }


initialPoseFor : ControlledShape -> Pose
initialPoseFor shape =
    case shape of
        ShapeOverCount ->
            overCountPose

        _ ->
            initialPose


type alias Model =
    { settings : Settings
    , camera : Camera
    , orbit : Orbit
    , pressedKeys : Set String
    , pose : Pose
    , shape : ControlledShape
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Duration
    | Resize Float Float
    | Reset
    | SelectShape String
    | KeyDown String
    | KeyUp String
    | ForOrbit Orbit.Msg
    | DownloadFixture


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
    let
        cameraConfig =
            { from = { x = 7, y = -12.1, z = 6.9 }
            , to = { x = 0, y = 0, z = 1 }
            }
    in
    ( { settings = { settings | debugContacts = True, debugContactIds = True }
      , camera = Camera.camera cameraConfig
      , orbit = Orbit.fromCartesian cameraConfig
      , pressedKeys = Set.empty
      , pose = initialPose
      , shape = ShapeCapsule
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model | settings = Settings.update settingsMsg model.settings }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | pose = applyHeldKeys (Duration.inMilliseconds dt) model.pressedKeys model.camera model.pose
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Reset ->
            ( { model | pose = initialPoseFor model.shape }, Cmd.none )

        SelectShape s ->
            let
                newShape =
                    Maybe.withDefault model.shape (shapeFromName s)
            in
            ( { model | shape = newShape, pose = initialPoseFor newShape }
            , Cmd.none
            )

        KeyDown key ->
            ( { model | pressedKeys = Set.insert key model.pressedKeys }
            , Cmd.none
            )

        KeyUp key ->
            ( { model | pressedKeys = Set.remove key model.pressedKeys }
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

        DownloadFixture ->
            ( model
            , File.Download.string
                (fixtureFilename model.shape)
                "text/plain"
                (fixtureSnippet model.shape model.pose)
            )


applyHeldKeys : Float -> Set String -> Camera -> Pose -> Pose
applyHeldKeys dtMs keys camera pose =
    let
        dt =
            dtMs * 0.001

        m =
            3.0 * dt

        r =
            1.5 * dt

        ( right, up, forward ) =
            cameraBasis camera

        bump : String -> Pose -> Pose
        bump key p =
            case key of
                "w" ->
                    p |> Frame3d.translateBy (vec (m * up.x) (m * up.y) (m * up.z))

                "s" ->
                    p |> Frame3d.translateBy (vec (-m * up.x) (-m * up.y) (-m * up.z))

                "a" ->
                    p |> Frame3d.translateBy (vec (-m * right.x) (-m * right.y) (-m * right.z))

                "d" ->
                    p |> Frame3d.translateBy (vec (m * right.x) (m * right.y) (m * right.z))

                "q" ->
                    p |> Frame3d.rotateAround (rollAxis p forward) (Angle.radians r)

                "e" ->
                    p |> Frame3d.rotateAround (rollAxis p forward) (Angle.radians -r)

                _ ->
                    p
    in
    Set.foldl bump pose keys


vec : Float -> Float -> Float -> Vector3d.Vector3d Meters WorldCoordinates
vec x y z =
    Vector3d.fromMeters { x = x, y = y, z = z }


cameraBasis :
    Camera
    ->
        ( { x : Float, y : Float, z : Float }
        , { x : Float, y : Float, z : Float }
        , { x : Float, y : Float, z : Float }
        )
cameraBasis camera =
    let
        fxRaw =
            camera.to.x - camera.from.x

        fyRaw =
            camera.to.y - camera.from.y

        fzRaw =
            camera.to.z - camera.from.z

        fLen =
            sqrt (fxRaw * fxRaw + fyRaw * fyRaw + fzRaw * fzRaw)

        forward =
            if fLen < 1.0e-6 then
                { x = 1, y = 0, z = 0 }

            else
                { x = fxRaw / fLen, y = fyRaw / fLen, z = fzRaw / fLen }

        rxRaw =
            forward.y

        ryRaw =
            -forward.x

        rLen =
            sqrt (rxRaw * rxRaw + ryRaw * ryRaw)

        right =
            if rLen < 1.0e-6 then
                { x = 1, y = 0, z = 0 }

            else
                { x = rxRaw / rLen, y = ryRaw / rLen, z = 0 }

        up =
            { x = right.y * forward.z - right.z * forward.y
            , y = right.z * forward.x - right.x * forward.z
            , z = right.x * forward.y - right.y * forward.x
            }
    in
    ( right, up, forward )


rollAxis : Pose -> { x : Float, y : Float, z : Float } -> Axis3d.Axis3d Meters WorldCoordinates
rollAxis pose forward =
    Axis3d.through
        (Frame3d.originPoint pose)
        (Direction3d.unsafe forward)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        , Orbit.subscriptions ForOrbit model.orbit
        ]


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.map (String.toLower >> toMsg)



-- Pose / collision shapes


frame3dToTransform3d :
    Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
    -> Transform3d.Transform3d coords defines
frame3dToTransform3d frame3d =
    Transform3d.fromOriginAndBasis
        (Point3d.toMeters (Frame3d.originPoint frame3d))
        (Direction3d.unwrap (Frame3d.xDirection frame3d))
        (Direction3d.unwrap (Frame3d.yDirection frame3d))
        (Direction3d.unwrap (Frame3d.zDirection frame3d))


targetConvex : ShapesConvex.Convex
targetConvex =
    ShapesConvex.fromBlock targetBoxSize targetBoxSize targetBoxSize
        |> ShapesConvex.placeIn Transform3d.atOrigin


computeContacts : Int -> ControlledShape -> Pose -> List Contact
computeContacts idPrefix shape pose =
    let
        transform =
            frame3dToTransform3d pose
    in
    case shape of
        ShapeCapsule ->
            let
                cap =
                    ShapesCapsule.atOrigin capsuleRadius capsuleHalfLength
                        |> ShapesCapsule.placeIn transform
            in
            CapsuleConvex.addContacts idPrefix identity cap targetConvex []

        ShapeBox ->
            let
                conv =
                    ShapesConvex.fromBlock controlledBoxSize controlledBoxSize controlledBoxSize
                        |> ShapesConvex.placeIn transform
            in
            ConvexConvex.addContacts idPrefix conv targetConvex []

        ShapeSphere ->
            let
                sph =
                    ShapesSphere.atOrigin sphereRadius
                        |> ShapesSphere.placeIn transform
            in
            SphereConvex.addContacts idPrefix identity sph targetConvex []

        ShapeCylinder ->
            let
                conv =
                    ShapesConvex.fromCylinder cylinderSubdivisions cylinderRadius cylinderLength
                        |> ShapesConvex.placeIn transform
            in
            ConvexConvex.addContacts idPrefix conv targetConvex []

        ShapeUnsafeConvexBox ->
            case unsafeConvexFromObj cubeObj of
                Just baseConv ->
                    ConvexConvex.addContacts idPrefix (ShapesConvex.placeIn transform baseConv) targetConvex []

                Nothing ->
                    []

        ShapeUnsafeConvexSphere ->
            case unsafeConvexFromObj icoSphereObj of
                Just baseConv ->
                    ConvexConvex.addContacts idPrefix (ShapesConvex.placeIn transform baseConv) targetConvex []

                Nothing ->
                    []

        ShapeOverCount ->
            let
                conv =
                    ShapesConvex.fromBlock targetBoxSize targetBoxSize targetBoxSize
                        |> ShapesConvex.placeIn transform
            in
            ConvexConvex.addContacts idPrefix conv targetConvex []



-- Bodies


targetBox3d : Block3d.Block3d Meters BodyCoordinates
targetBox3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters targetBoxSize, Length.meters targetBoxSize, Length.meters targetBoxSize )


capsule3d : Cylinder3d.Cylinder3d Meters BodyCoordinates
capsule3d =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = Length.meters capsuleRadius
        , length = Length.meters (2 * capsuleHalfLength)
        }


controlledBox3d : Block3d.Block3d Meters BodyCoordinates
controlledBox3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters controlledBoxSize, Length.meters controlledBoxSize, Length.meters controlledBoxSize )


sphere3d : Sphere3d.Sphere3d Meters BodyCoordinates
sphere3d =
    Sphere3d.atOrigin (Length.meters sphereRadius)


controlledCylinder3d : Cylinder3d.Cylinder3d Meters BodyCoordinates
controlledCylinder3d =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = Length.meters cylinderRadius
        , length = Length.meters cylinderLength
        }


targetBody : Body
targetBody =
    Physics.static [ ( PhysicsShape.block targetBox3d, Material.wood ) ]


controlledBody : ControlledShape -> Pose -> Body
controlledBody shape pose =
    let
        base =
            case shape of
                ShapeCapsule ->
                    Physics.capsule capsule3d Material.wood

                ShapeBox ->
                    Physics.block controlledBox3d Material.wood

                ShapeSphere ->
                    Physics.sphere sphere3d Material.wood

                ShapeCylinder ->
                    Physics.cylinder controlledCylinder3d Material.wood

                ShapeUnsafeConvexBox ->
                    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) cubeObj of
                        Ok mesh ->
                            Physics.dynamic [ ( PhysicsShape.unsafeConvex mesh, Material.wood ) ]

                        Err _ ->
                            Physics.dynamic []

                ShapeUnsafeConvexSphere ->
                    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) icoSphereObj of
                        Ok mesh ->
                            Physics.dynamic [ ( PhysicsShape.unsafeConvex mesh, Material.wood ) ]

                        Err _ ->
                            Physics.dynamic []

                ShapeOverCount ->
                    Physics.block targetBox3d Material.wood
    in
    base |> Physics.place pose



-- Meshes


type alias ShapeMeshes =
    { targetBox : Meshes.Meshes
    , capsule : Meshes.Meshes
    , box : Meshes.Meshes
    , sphere : Meshes.Meshes
    , cylinder : Meshes.Meshes
    , unsafeConvexBox : Meshes.Meshes
    , unsafeConvexSphere : Meshes.Meshes
    }


shapeMeshes : ShapeMeshes
shapeMeshes =
    { targetBox = Meshes.fromTriangles (Meshes.block targetBox3d)
    , capsule = Meshes.fromTriangles (Meshes.capsule 16 capsule3d)
    , box = Meshes.fromTriangles (Meshes.block controlledBox3d)
    , sphere = Meshes.fromTriangles (Meshes.sphere 3 sphere3d)
    , cylinder = Meshes.fromTriangles (Meshes.cylinder 16 controlledCylinder3d)
    , unsafeConvexBox =
        case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) cubeObj of
            Ok mesh ->
                Meshes.fromTriangles (Meshes.triangularMesh mesh)

            Err _ ->
                Meshes.fromTriangles []
    , unsafeConvexSphere =
        case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) icoSphereObj of
            Ok mesh ->
                Meshes.fromTriangles (Meshes.triangularMesh mesh)

            Err _ ->
                Meshes.fromTriangles []
    }


controlledMesh : ControlledShape -> Meshes.Meshes
controlledMesh shape =
    case shape of
        ShapeCapsule ->
            shapeMeshes.capsule

        ShapeBox ->
            shapeMeshes.box

        ShapeSphere ->
            shapeMeshes.sphere

        ShapeCylinder ->
            shapeMeshes.cylinder

        ShapeUnsafeConvexBox ->
            shapeMeshes.unsafeConvexBox

        ShapeUnsafeConvexSphere ->
            shapeMeshes.unsafeConvexSphere

        ShapeOverCount ->
            shapeMeshes.targetBox



-- View


view : Model -> Html Msg
view model =
    let
        contactList =
            computeContacts 0 model.shape model.pose

        contactPoints =
            List.concatMap
                (\c ->
                    [ Point3d.fromMeters c.pi
                    , Point3d.fromMeters c.pj
                    ]
                )
                contactList
    in
    Html.div
        [ Html.Events.preventDefaultOn "wheel" (Orbit.wheelDecoder ForOrbit) ]
        [ Scene.view
            { settings = model.settings
            , bodies =
                let
                    controlled =
                        controlledBody model.shape model.pose
                in
                [ ( controlledMesh model.shape, controlled, Physics.frame controlled )
                , ( shapeMeshes.targetBox, targetBody, Physics.frame targetBody )
                ]
            , contacts = contactPoints
            , camera = model.camera
            , contactRadius = 0.07
            , floorOffset = floorOffset
            }
        , if model.settings.debugContacts && model.settings.debugContactIds then
            ContactLabels.view model.camera
                (List.map
                    (\c -> { id = ContactId.toString c.shapeKey c.featureKey, point = Point3d.fromMeters c.pi })
                    contactList
                )

          else
            Html.text ""
        , Settings.view ForSettings
            model.settings
            []
            [ shapeSelect model.shape
            , Demo.button Reset "Reset pose"
            , Demo.button DownloadFixture "Download fixture"
            , controlsHint
            ]
        ]


controlsHint : Html msg
controlsHint =
    Html.div
        [ Html.Attributes.style "margin" "10px 0 0"
        , Html.Attributes.style "opacity" "0.8"
        ]
        [ Html.text "WASD move · QE roll" ]


shapeSelect : ControlledShape -> Html Msg
shapeSelect current =
    Html.select
        [ Html.Events.onInput SelectShape
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "box-sizing" "border-box"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "padding" "6px 22px 6px 6px"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "border" "none"
        , Html.Attributes.style "color" "inherit"
        , Html.Attributes.style "font" "inherit"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "appearance" "none"
        , Html.Attributes.style "-webkit-appearance" "none"
        , Html.Attributes.style "background-color" "rgb(61, 61, 61)"
        , Html.Attributes.style "background-image"
            "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12'><path fill='white' d='M2 4 L6 8 L10 4 Z'/></svg>\")"
        , Html.Attributes.style "background-repeat" "no-repeat"
        , Html.Attributes.style "background-position" "right 6px center"
        , Html.Attributes.style "background-size" "12px 12px"
        ]
        (List.map
            (\s ->
                let
                    name =
                        shapeName s
                in
                Html.option
                    [ Html.Attributes.value name
                    , Html.Attributes.selected (s == current)
                    ]
                    [ Html.text name ]
            )
            allShapes
        )


floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -10 }


unsafeConvexFromObj : String -> Maybe ShapesConvex.Convex
unsafeConvexFromObj obj =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) obj of
        Ok mesh ->
            Just
                (ShapesConvex.fromTriangularMesh
                    (TriangularMesh.faceIndices mesh)
                    (mesh |> TriangularMesh.mapVertices Point3d.toMeters |> TriangularMesh.vertices)
                )

        Err _ ->
            Nothing


cubeObj : String
cubeObj =
    """v 1.000000 1.000000 -1.000000
v 1.000000 -1.000000 -1.000000
v 1.000000 1.000000 1.000000
v 1.000000 -1.000000 1.000000
v -1.000000 1.000000 -1.000000
v -1.000000 -1.000000 -1.000000
v -1.000000 1.000000 1.000000
v -1.000000 -1.000000 1.000000
f 1 5 7 3
f 4 3 7 8
f 8 7 5 6
f 6 2 4 8
f 2 1 3 4
f 6 5 1 2
"""


icoSphereObj : String
icoSphereObj =
    """v 0.000000 0.000000 -1.000000
v 0.723607 -0.525725 -0.447220
v -0.276388 -0.850649 -0.447220
v -0.894426 0.000000 -0.447216
v -0.276388 0.850649 -0.447220
v 0.723607 0.525725 -0.447220
v 0.276388 -0.850649 0.447220
v -0.723607 -0.525725 0.447220
v -0.723607 0.525725 0.447220
v 0.276388 0.850649 0.447220
v 0.894426 0.000000 0.447216
v 0.000000 0.000000 1.000000
v -0.162456 -0.499995 -0.850654
v 0.425323 -0.309011 -0.850654
v 0.262869 -0.809012 -0.525738
v 0.850648 0.000000 -0.525736
v 0.425323 0.309011 -0.850654
v -0.525730 0.000000 -0.850652
v -0.688189 -0.499997 -0.525736
v -0.162456 0.499995 -0.850654
v -0.688189 0.499997 -0.525736
v 0.262869 0.809012 -0.525738
v 0.951058 -0.309013 0.000000
v 0.951058 0.309013 0.000000
v 0.000000 -1.000000 0.000000
v 0.587786 -0.809017 0.000000
v -0.951058 -0.309013 0.000000
v -0.587786 -0.809017 0.000000
v -0.587786 0.809017 0.000000
v -0.951058 0.309013 0.000000
v 0.587786 0.809017 0.000000
v 0.000000 1.000000 0.000000
v 0.688189 -0.499997 0.525736
v -0.262869 -0.809012 0.525738
v -0.850648 0.000000 0.525736
v -0.262869 0.809012 0.525738
v 0.688189 0.499997 0.525736
v 0.162456 -0.499995 0.850654
v 0.525730 0.000000 0.850652
v -0.425323 -0.309011 0.850654
v -0.425323 0.309011 0.850654
v 0.162456 0.499995 0.850654
s off
f 1 14 13
f 2 14 16
f 1 13 18
f 1 18 20
f 1 20 17
f 2 16 23
f 3 15 25
f 4 19 27
f 5 21 29
f 6 22 31
f 2 23 26
f 3 25 28
f 4 27 30
f 5 29 32
f 6 31 24
f 7 33 38
f 8 34 40
f 9 35 41
f 10 36 42
f 11 37 39
f 39 42 12
f 39 37 42
f 37 10 42
f 42 41 12
f 42 36 41
f 36 9 41
f 41 40 12
f 41 35 40
f 35 8 40
f 40 38 12
f 40 34 38
f 34 7 38
f 38 39 12
f 38 33 39
f 33 11 39
f 24 37 11
f 24 31 37
f 31 10 37
f 32 36 10
f 32 29 36
f 29 9 36
f 30 35 9
f 30 27 35
f 27 8 35
f 28 34 8
f 28 25 34
f 25 7 34
f 26 33 7
f 26 23 33
f 23 11 33
f 31 32 10
f 31 22 32
f 22 5 32
f 29 30 9
f 29 21 30
f 21 4 30
f 27 28 8
f 27 19 28
f 19 3 28
f 25 26 7
f 25 15 26
f 15 2 26
f 23 24 11
f 23 16 24
f 16 6 24
f 17 22 6
f 17 20 22
f 20 5 22
f 20 21 5
f 20 18 21
f 18 4 21
f 18 19 4
f 18 13 19
f 13 3 19
f 16 17 6
f 16 14 17
f 14 1 17
f 13 15 3
f 13 14 15
f 14 2 15
"""



-- Fixture generator


fixtureFilename : ControlledShape -> String
fixtureFilename shape =
    case shape of
        ShapeCapsule ->
            "CapsuleBoxFixture.elm"

        ShapeBox ->
            "BoxBoxFixture.elm"

        ShapeSphere ->
            "SphereBoxFixture.elm"

        ShapeCylinder ->
            "CylinderBoxFixture.elm"

        ShapeUnsafeConvexBox ->
            "UnsafeConvexBoxBoxFixture.elm"

        ShapeUnsafeConvexSphere ->
            "UnsafeConvexSphereBoxFixture.elm"

        ShapeOverCount ->
            "BoxBoxOverCountFixture.elm"


fixtureSnippet : ControlledShape -> Pose -> String
fixtureSnippet shape pose =
    let
        contacts =
            computeContacts 0 shape pose
    in
    fixtureHeader shape (List.length contacts)
        ++ fixtureImports shape
        ++ "\n\nfixture : Test\nfixture =\n    let\n"
        ++ targetBoxBindings
        ++ controlledBindings shape pose
        ++ "    in\n    test \"TODO describe scenario\" <|\n        \\_ ->\n            "
        ++ callExpression shape
        ++ "\n                |> Expect.contactsWithIds\n"
        ++ formatContactList contacts


fixtureHeader : ControlledShape -> Int -> String
fixtureHeader shape contactCount =
    "module Fixture exposing (fixture)\n\n"
        ++ "{-| Generated from the Collisions sandbox demo.\n\n"
        ++ "Shape: "
        ++ shapeName shape
        ++ "\nContacts: "
        ++ String.fromInt contactCount
        ++ "\n\nPaste into a test module — rename, prettify the floats, and tighten the\n"
        ++ "scenario description. Generated floats print as e.g. 0.7071067811865475\n"
        ++ "where the existing tests prefer symbolic forms like `1 / sqrt 2`.\n\n-}\n\n"


fixtureImports : ControlledShape -> String
fixtureImports shape =
    let
        common =
            [ "import Extra.Expect as Expect"
            , "import Internal.Transform3d as Transform3d"
            , "import Shapes.Convex as Convex"
            , "import Test exposing (Test, test)"
            ]

        extras =
            case shape of
                ShapeCapsule ->
                    [ "import Collision.CapsuleConvex", "import Shapes.Capsule as Capsule" ]

                ShapeBox ->
                    [ "import Collision.ConvexConvex" ]

                ShapeSphere ->
                    [ "import Collision.SphereConvex", "import Shapes.Sphere as Sphere" ]

                ShapeCylinder ->
                    [ "import Collision.ConvexConvex" ]

                ShapeUnsafeConvexBox ->
                    [ "import Collision.ConvexConvex" ]

                ShapeUnsafeConvexSphere ->
                    [ "import Collision.ConvexConvex" ]

                ShapeOverCount ->
                    [ "import Collision.ConvexConvex" ]
    in
    String.join "\n" (List.sort (common ++ extras))


targetBoxBindings : String
targetBoxBindings =
    "        boxSize =\n            "
        ++ formatFloat targetBoxSize
        ++ "\n\n        box =\n            Convex.fromBlock boxSize boxSize boxSize\n                |> Convex.placeIn Transform3d.atOrigin\n\n"


controlledBindings : ControlledShape -> Pose -> String
controlledBindings shape pose =
    case shape of
        ShapeCapsule ->
            "        radius =\n            "
                ++ formatFloat capsuleRadius
                ++ "\n\n        halfLength =\n            "
                ++ formatFloat capsuleHalfLength
                ++ "\n\n        capsule =\n            Capsule.atOrigin radius halfLength\n                |> Capsule.placeIn\n"
                ++ formatPose pose
                ++ "\n"

        ShapeBox ->
            "        controlledSize =\n            "
                ++ formatFloat controlledBoxSize
                ++ "\n\n        controlledBox =\n            Convex.fromBlock controlledSize controlledSize controlledSize\n                |> Convex.placeIn\n"
                ++ formatPose pose
                ++ "\n"

        ShapeSphere ->
            "        radius =\n            "
                ++ formatFloat sphereRadius
                ++ "\n\n        sphere =\n            Sphere.atOrigin radius\n                |> Sphere.placeIn\n"
                ++ formatPose pose
                ++ "\n"

        ShapeCylinder ->
            "        subdivisions =\n            "
                ++ String.fromInt cylinderSubdivisions
                ++ "\n\n        radius =\n            "
                ++ formatFloat cylinderRadius
                ++ "\n\n        length =\n            "
                ++ formatFloat cylinderLength
                ++ "\n\n        cylinder =\n            Convex.fromCylinder subdivisions radius length\n                |> Convex.placeIn\n"
                ++ formatPose pose
                ++ "\n"

        ShapeUnsafeConvexBox ->
            "        -- The cubeObj in Collisions.elm is a 2x2x2 cube; the line below is\n"
                ++ "        -- collision-equivalent. To exercise the OBJ-derived construction\n"
                ++ "        -- specifically, replace with `unsafeConvexFromObj cubeObj`-style\n"
                ++ "        -- code (see Collisions.elm).\n"
                ++ "        controlledBox =\n            Convex.fromBlock 2 2 2\n                |> Convex.placeIn\n"
                ++ formatPose pose
                ++ "\n"

        ShapeUnsafeConvexSphere ->
            "        -- The icoSphereObj in Collisions.elm is a faceted approximation of a\n"
                ++ "        -- unit sphere. There is no native Convex constructor that produces\n"
                ++ "        -- the same hull, so reproduce the OBJ-derived construction from\n"
                ++ "        -- Collisions.elm here:\n"
                ++ "        controlledConvex =\n            Debug.todo \"reproduce icoSphereObj construction\"\n"
                ++ "                |> Convex.placeIn\n"
                ++ formatPose pose
                ++ "\n"

        ShapeOverCount ->
            "        controlledBox =\n            Convex.fromBlock boxSize boxSize boxSize\n                |> Convex.placeIn\n"
                ++ formatPose pose
                ++ "\n"


callExpression : ControlledShape -> String
callExpression shape =
    case shape of
        ShapeCapsule ->
            "Collision.CapsuleConvex.addContacts \"\" identity capsule box []"

        ShapeBox ->
            "Collision.ConvexConvex.addContacts \"\" controlledBox box []"

        ShapeSphere ->
            "Collision.SphereConvex.addContacts \"\" identity sphere box []"

        ShapeCylinder ->
            "Collision.ConvexConvex.addContacts \"\" cylinder box []"

        ShapeUnsafeConvexBox ->
            "Collision.ConvexConvex.addContacts \"\" controlledBox box []"

        ShapeUnsafeConvexSphere ->
            "Collision.ConvexConvex.addContacts \"\" controlledConvex box []"

        ShapeOverCount ->
            "Collision.ConvexConvex.addContacts \"\" controlledBox box []"


formatPose : Pose -> String
formatPose pose =
    let
        origin =
            Point3d.toMeters (Frame3d.originPoint pose)

        xDir =
            Direction3d.unwrap (Frame3d.xDirection pose)

        yDir =
            Direction3d.unwrap (Frame3d.yDirection pose)

        zDir =
            Direction3d.unwrap (Frame3d.zDirection pose)
    in
    "                    (Transform3d.fromOriginAndBasis\n"
        ++ "                        "
        ++ formatVec3 origin
        ++ "\n                        "
        ++ formatVec3 xDir
        ++ "\n                        "
        ++ formatVec3 yDir
        ++ "\n                        "
        ++ formatVec3 zDir
        ++ "\n                    )"


formatContactList : List Contact -> String
formatContactList contacts =
    case contacts of
        [] ->
            "                    []\n"

        first :: rest ->
            let
                head =
                    "                    [ " ++ formatContact first ++ "\n"

                tail =
                    rest
                        |> List.map (\c -> "                    , " ++ formatContact c ++ "\n")
                        |> String.concat
            in
            head ++ tail ++ "                    ]\n"


formatContact : Contact -> String
formatContact c =
    -- Emit the packed id fields (see Internal.ContactId); the trailing comment
    -- keeps the human-readable feature suffix next to them.
    "{ shapeKey = "
        ++ String.fromInt c.shapeKey
        ++ ", featureKey = "
        ++ String.fromInt c.featureKey
        ++ " -- "
        ++ ContactId.featureString c.featureKey
        ++ "\n                      , ni = "
        ++ formatVec3 c.ni
        ++ "\n                      , pi = "
        ++ formatVec3 c.pi
        ++ "\n                      , pj = "
        ++ formatVec3 c.pj
        ++ "\n                      }"


formatVec3 : { x : Float, y : Float, z : Float } -> String
formatVec3 v =
    "{ x = "
        ++ formatFloat v.x
        ++ ", y = "
        ++ formatFloat v.y
        ++ ", z = "
        ++ formatFloat v.z
        ++ " }"


formatFloat : Float -> String
formatFloat f =
    if f == 0 then
        "0"

    else
        String.fromFloat f
