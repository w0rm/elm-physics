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
import Array
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Collision.CapsuleConvex as CapsuleConvex
import Collision.ConvexConvex as ConvexConvex
import Collision.SphereConvex as SphereConvex
import Common.Camera as Camera exposing (Camera)
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Attributes)
import Common.Scene as Scene
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Cylinder3d
import Direction3d
import File.Download
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Internal.Contact exposing (Contact)
import Internal.Transform3d as Transform3d
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Math.Matrix4 as Mat4
import Math.Vector3 as MathVec3
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
import WebGL exposing (Mesh)



-- Shape options


type ControlledShape
    = ShapeCapsule
    | ShapeBox
    | ShapeSphere
    | ShapeCylinder
    | ShapeUnsafeConvexBox
    | ShapeUnsafeConvexSphere


allShapes : List ControlledShape
allShapes =
    [ ShapeCapsule, ShapeBox, ShapeSphere, ShapeCylinder, ShapeUnsafeConvexBox, ShapeUnsafeConvexSphere ]


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
            "UnsafeConvex Box × Box"

        ShapeUnsafeConvexSphere ->
            "UnsafeConvex Sphere × Box"



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


type alias Model =
    { settings : Settings
    , fps : List Float
    , camera : Camera
    , cameraAzimuth : Float
    , cameraElevation : Float
    , cameraDistance : Float
    , orbiting : Bool
    , pressedKeys : Set String
    , pose : Pose
    , shape : ControlledShape
    , showContactIds : Bool
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Reset
    | SelectShape ControlledShape
    | KeyDown String
    | KeyUp String
    | MouseDown
    | MouseUp
    | MouseMove Float Float
    | MouseWheel Float
    | ToggleContactIds Bool
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
        azimuth =
            degrees -60

        elevation =
            degrees 25

        distance =
            14
    in
    ( { settings = { settings | showFpsMeter = True, debugContacts = True }
      , fps = []
      , camera =
            Camera.camera
                { from = cameraFrom azimuth elevation distance
                , to = { x = 0, y = 0, z = 1 }
                }
      , cameraAzimuth = azimuth
      , cameraElevation = elevation
      , cameraDistance = distance
      , orbiting = False
      , pressedKeys = Set.empty
      , pose = initialPose
      , shape = ShapeCapsule
      , showContactIds = True
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


cameraFrom : Float -> Float -> Float -> { x : Float, y : Float, z : Float }
cameraFrom azimuth elevation distance =
    { x = distance * cos elevation * cos azimuth
    , y = distance * cos elevation * sin azimuth
    , z = 1 + distance * sin elevation
    }


updateCameraView : Model -> Camera
updateCameraView model =
    let
        from =
            cameraFrom model.cameraAzimuth model.cameraElevation model.cameraDistance

        cam =
            model.camera
    in
    { cam
        | from = from
        , cameraTransform =
            Mat4.makeLookAt
                (MathVec3.fromRecord from)
                (MathVec3.fromRecord cam.to)
                MathVec3.k
    }



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
                | fps = Fps.update dt model.fps
                , pose = applyHeldKeys dt model.pressedKeys model.camera model.pose
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | camera = Camera.resize width height model.camera }
            , Cmd.none
            )

        Reset ->
            ( { model | pose = initialPose }, Cmd.none )

        SelectShape s ->
            ( { model | shape = s, pose = initialPose }, Cmd.none )

        KeyDown key ->
            ( { model | pressedKeys = Set.insert key model.pressedKeys }
            , Cmd.none
            )

        KeyUp key ->
            ( { model | pressedKeys = Set.remove key model.pressedKeys }
            , Cmd.none
            )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    new =
                        { model
                            | cameraAzimuth = model.cameraAzimuth - dx * 0.005
                            , cameraElevation =
                                clamp (degrees -85)
                                    (degrees 85)
                                    (model.cameraElevation + dy * 0.005)
                        }
                in
                ( { new | camera = updateCameraView new }, Cmd.none )

            else
                ( model, Cmd.none )

        MouseWheel deltaY ->
            let
                new =
                    { model
                        | cameraDistance =
                            clamp 4 60 (model.cameraDistance + deltaY * 0.01)
                    }
            in
            ( { new | camera = updateCameraView new }, Cmd.none )

        ToggleContactIds value ->
            ( { model | showContactIds = value }, Cmd.none )

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
        , Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        , if model.orbiting then
            Sub.batch
                [ Events.onMouseMove mouseMoveDecoder
                , Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Events.onMouseDown (Decode.succeed MouseDown)
        ]


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.map (String.toLower >> toMsg)


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


wheelDecoder : Decoder ( Msg, Bool )
wheelDecoder =
    Decode.map (\dy -> ( MouseWheel dy, True ))
        (Decode.field "deltaY" Decode.float)



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


computeContacts : String -> ControlledShape -> Pose -> List Contact
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
    in
    base |> Physics.place pose



-- Meshes


type alias ShapeMeshes =
    { targetBox : Mesh Attributes
    , capsule : Mesh Attributes
    , box : Mesh Attributes
    , sphere : Mesh Attributes
    , cylinder : Mesh Attributes
    , unsafeConvexBox : Mesh Attributes
    , unsafeConvexSphere : Mesh Attributes
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


controlledMesh : ControlledShape -> Mesh Attributes
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



-- View


view : Model -> Html Msg
view model =
    let
        contactList =
            computeContacts "test" model.shape model.pose

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
        [ Html.Events.preventDefaultOn "wheel" wheelDecoder ]
        [ Scene.view
            { settings = model.settings
            , bodies =
                [ ( controlledMesh model.shape, controlledBody model.shape model.pose )
                , ( shapeMeshes.targetBox, targetBody )
                ]
            , contacts = contactPoints
            , camera = model.camera
            , floorOffset = floorOffset
            }
        , overlay model contactList
        , if model.showContactIds then
            contactLabels model.camera contactList

          else
            Html.text ""
        , Settings.view ForSettings
            model.settings
            (Html.button [ onClick Reset ] [ Html.text "Reset pose" ]
                :: Html.button [ onClick DownloadFixture ] [ Html.text "Download fixture" ]
                :: toggleButton model.showContactIds "show contact ids" ToggleContactIds
                :: List.map (shapeButton model.shape) allShapes
            )
        , if model.settings.showFpsMeter then
            Fps.view model.fps 2 0

          else
            Html.text ""
        ]


toggleButton : Bool -> String -> (Bool -> Msg) -> Html Msg
toggleButton on label toMsg =
    Html.button
        [ onClick (toMsg (not on))
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "padding" "6px"
        , Html.Attributes.style "margin" "4px 0 0"
        , Html.Attributes.style "border" "none"
        , Html.Attributes.style "color" "inherit"
        , Html.Attributes.style "font" "inherit"
        , Html.Attributes.style "background"
            (if on then
                "rgb(120, 90, 40)"

             else
                "rgb(61, 61, 61)"
            )
        ]
        [ Html.text label ]


{-| Render the contact id of every Contact as an HTML label, projected
onto screen coordinates. One label per contact (placed at `pi`).
-}
contactLabels : Camera -> List Contact -> Html Msg
contactLabels camera contacts =
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "pointer-events" "none"
        ]
        (List.filterMap (contactLabel camera) contacts)


contactLabel : Camera -> Contact -> Maybe (Html Msg)
contactLabel camera contact =
    case projectToScreen camera contact.pi of
        Nothing ->
            Nothing

        Just { x, y } ->
            Just
                (Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "left" (String.fromFloat x ++ "px")
                    , Html.Attributes.style "top" (String.fromFloat y ++ "px")
                    , Html.Attributes.style "transform" "translate(8px, -8px)"
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "font-family" "monospace"
                    , Html.Attributes.style "font-size" "11px"
                    , Html.Attributes.style "background" "rgba(180, 30, 30, 0.85)"
                    , Html.Attributes.style "padding" "1px 4px"
                    , Html.Attributes.style "border-radius" "2px"
                    , Html.Attributes.style "white-space" "nowrap"
                    ]
                    [ Html.text contact.id ]
                )


{-| Project a world-space point to pixel coordinates within the canvas.
Returns `Nothing` if the point is behind the camera.
-}
projectToScreen :
    Camera
    -> { x : Float, y : Float, z : Float }
    -> Maybe { x : Float, y : Float }
projectToScreen camera point =
    let
        forwardX =
            camera.to.x - camera.from.x

        forwardY =
            camera.to.y - camera.from.y

        forwardZ =
            camera.to.z - camera.from.z

        toX =
            point.x - camera.from.x

        toY =
            point.y - camera.from.y

        toZ =
            point.z - camera.from.z

        front =
            toX * forwardX + toY * forwardY + toZ * forwardZ
    in
    if front <= 0 then
        Nothing

    else
        let
            mvp =
                Mat4.mul camera.perspectiveTransform camera.cameraTransform

            ndc =
                Mat4.transform mvp (MathVec3.fromRecord point)
        in
        Just
            { x = (MathVec3.getX ndc + 1) * 0.5 * camera.width
            , y = (1 - MathVec3.getY ndc) * 0.5 * camera.height
            }


shapeButton : ControlledShape -> ControlledShape -> Html Msg
shapeButton current shape =
    Html.button
        [ onClick (SelectShape shape)
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "padding" "6px"
        , Html.Attributes.style "margin" "4px 0 0"
        , Html.Attributes.style "border" "none"
        , Html.Attributes.style "color" "inherit"
        , Html.Attributes.style "font" "inherit"
        , Html.Attributes.style "background"
            (if current == shape then
                "rgb(120, 90, 40)"

             else
                "rgb(61, 61, 61)"
            )
        ]
        [ Html.text (shapeName shape) ]


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


overlay : Model -> List Contact -> Html Msg
overlay model contacts =
    let
        origin =
            Point3d.toMeters (Frame3d.originPoint model.pose)

        f3 v =
            String.fromFloat (toFloat (round (v * 100)) / 100)

        text =
            shapeName model.shape
                ++ "\nposition: ("
                ++ f3 origin.x
                ++ ", "
                ++ f3 origin.y
                ++ ", "
                ++ f3 origin.z
                ++ ")"
                ++ "\ncontacts: "
                ++ String.fromInt (List.length contacts)
                ++ "\n\nWASD move (screen plane) · Q/E roll around view ray"
                ++ "\nMouse drag to orbit · wheel to zoom"
    in
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "padding" "10px"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "font-family" "monospace"
        , Html.Attributes.style "font-size" "12px"
        , Html.Attributes.style "background" "rgba(0,0,0,0.45)"
        , Html.Attributes.style "pointer-events" "none"
        , Html.Attributes.style "white-space" "pre"
        , Html.Attributes.style "line-height" "1.4"
        ]
        [ Html.text text ]



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


fixtureSnippet : ControlledShape -> Pose -> String
fixtureSnippet shape pose =
    let
        contacts =
            computeContacts "" shape pose
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
    "{ id = \""
        ++ c.id
        ++ "\"\n                      , ni = "
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
