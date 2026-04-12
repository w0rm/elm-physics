module Raycast exposing (main)

{-| This demo shows how elm-physics could be used to determine,
which object has been clicked, and also to do ray tracing.

A mouse ray is cast into the scene, and if it hits an object
the reflected ray is computed from the surface normal and cast
again.

-}

import Angle
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Length exposing (Meters, meters, millimeters)
import LineSegment3d exposing (LineSegment3d)
import Physics exposing (Body, BodyCoordinates, WorldCoordinates)
import Physics.Material
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Sphere3d exposing (Sphere3d)
import Task


type Id
    = Cylinder
    | Block
    | Sphere
    | Floor


type alias Model =
    { dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , selection : Maybe Id
    , rayPath : List (LineSegment3d Meters WorldCoordinates)
    }


type Msg
    = Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Browser.Events.onResize Resize
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dimensions = ( Pixels.int 0, Pixels.int 0 )
      , selection = Nothing
      , rayPath = []
      }
    , Task.perform
        (\{ viewport } -> Resize (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


bodies : List ( Id, Body )
bodies =
    [ ( Floor, Physics.block floor Physics.Material.wood |> Physics.moveTo (Point3d.millimeters 0 0 -5) )
    , ( Block, Physics.block block Physics.Material.wood |> Physics.moveTo (Point3d.meters -1 -1 0) )
    , ( Sphere, Physics.sphere sphere Physics.Material.wood |> Physics.moveTo (Point3d.meters 0 1.5 0) )
    , ( Cylinder, Physics.cylinder cylinder Physics.Material.wood |> Physics.moveTo (Point3d.meters 1.5 0 0) )
    ]


floor : Block3d Meters BodyCoordinates
floor =
    Block3d.centeredOn Frame3d.atOrigin
        ( meters 5, meters 5, millimeters 10 )


block : Block3d Meters BodyCoordinates
block =
    Block3d.from
        (Point3d.meters -0.5 -0.5 0)
        (Point3d.meters 0.5 0.5 1.5)


sphere : Sphere3d Meters BodyCoordinates
sphere =
    Sphere3d.atPoint (Point3d.meters 0 0 0.5)
        (meters 0.5)


cylinder : Cylinder3d Meters BodyCoordinates
cylinder =
    Cylinder3d.startingAt Point3d.origin
        Direction3d.positiveZ
        { radius = meters 0.5
        , length = meters 1.5
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize width height ->
            { model | dimensions = ( Pixels.int width, Pixels.int height ) }

        MouseDown mouseRay ->
            let
                firstHit =
                    Physics.raycast mouseRay bodies

                selection =
                    Maybe.map (\( id, _, _ ) -> id) firstHit

                rayPath =
                    buildPath mouseRay 10 []
            in
            { model | selection = selection, rayPath = rayPath }


buildPath : Axis3d Meters WorldCoordinates -> Int -> List (LineSegment3d Meters WorldCoordinates) -> List (LineSegment3d Meters WorldCoordinates)
buildPath ray hops segments =
    case Physics.raycast ray bodies of
        Nothing ->
            LineSegment3d.from (Axis3d.originPoint ray) (Point3d.along ray (Length.meters 20)) :: segments

        Just ( _, _, { point, normal } ) ->
            let
                segment =
                    LineSegment3d.from (Axis3d.originPoint ray) point

                reflectedDir =
                    Direction3d.mirrorAcross (Plane3d.through point normal) (Axis3d.direction ray)

                reflectedRay =
                    Axis3d.through point reflectedDir
            in
            if hops == 0 then
                segment :: segments

            else
                buildPath reflectedRay (hops - 1) (segment :: segments)


view : Model -> Html Msg
view { selection, dimensions, rayPath } =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (decodeMouseRay dimensions MouseDown)
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
            , shadows = True
            , camera = camera
            , dimensions = dimensions
            , background = Scene3d.transparentBackground
            , clipDepth = Length.meters 0.1
            , entities =
                List.map (bodyToEntity selection) bodies
                    ++ List.map (Scene3d.lineSegment (Material.color Color.green)) rayPath
            }
        ]


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.lookAt
        { eyePoint = Point3d.meters 5 6 4
        , focalPoint = Point3d.meters -0.5 -0.5 0
        , upDirection = Direction3d.positiveZ
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 24)
        }


bodyToEntity : Maybe Id -> ( Id, Body ) -> Entity WorldCoordinates
bodyToEntity selection ( id, body ) =
    let
        color defaultColor =
            if selection == Just id then
                Color.white

            else
                defaultColor
    in
    Scene3d.placeIn (Physics.frame body) <|
        case id of
            Floor ->
                Scene3d.block
                    (Material.matte (color Color.darkCharcoal))
                    floor

            Block ->
                Scene3d.blockWithShadow
                    (Material.nonmetal
                        { baseColor = color Color.red
                        , roughness = 0.25
                        }
                    )
                    block

            Sphere ->
                Scene3d.sphereWithShadow
                    (Material.nonmetal
                        { baseColor = color Color.yellow
                        , roughness = 0.25
                        }
                    )
                    sphere

            Cylinder ->
                Scene3d.cylinderWithShadow
                    (Material.nonmetal
                        { baseColor = color Color.blue
                        , roughness = 0.25
                        }
                    )
                    cylinder


decodeMouseRay :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay ( width, height ) rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg <|
                Camera3d.ray camera
                    (Rectangle2d.with
                        { x1 = Quantity.zero
                        , y1 = Quantity.toFloatQuantity height
                        , x2 = Quantity.toFloatQuantity width
                        , y2 = Quantity.zero
                        }
                    )
                    (Point2d.pixels x y)
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)
