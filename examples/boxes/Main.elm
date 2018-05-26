module Boxes exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Task
import Time exposing (Time)
import WebGL exposing (Entity, Shader, Mesh)
import Window
import Random


type alias Model =
    { screenWidth : Float
    , screenHeight : Float
    , world : Physics.World
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | AddRandomBox
    | AddBox Physics.Body


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| A constant cube-shaped body with unit sides and mass
-}
unitCube : Physics.Body
unitCube =
    Physics.body
        |> Physics.setMass 1
        |> Physics.addShape (Physics.box (vec3 1 1 1))


randomlyRotatedUnitCube : Random.Generator Physics.Body
randomlyRotatedUnitCube =
    Random.map4
        (\angle x y z ->
            unitCube
                |> Physics.offsetBy (vec3 0 0 10)
                |> Physics.rotateBy (Vec3.normalize (vec3 x y z)) angle
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


{-| A constant (for now) scaling factor.
The value of 1 gives a full-screen-height image that gets horizontally extended
or centered and cropped depending on screen-width.
Lower values result in this same image size with reduced resolution,
introducing jaggies or pixelation.
Higher values initially shrink the image to the upper left of the screen,
but resizing the screen then has strange effects, probably unintended,
on the image size (TODO: debug devicePixelRatio > 1).
-}
devicePixelRatio : Float
devicePixelRatio =
    1


init : ( Model, Cmd Msg )
init =
    ( { screenWidth = 1
      , screenHeight = 1
      , world =
            Physics.world
                |> Physics.setGravity (vec3 0 0 -10)
                |> Physics.addBody
                    (Physics.body
                        |> Physics.addShape Physics.plane
                        |> Physics.offsetBy (vec3 0 0 -1)
                    )
                |> Physics.addBody
                    (unitCube
                        |> Physics.offsetBy (vec3 0 0 2)
                        |> Physics.rotateBy Vec3.j (-pi / 5)
                    )
                |> Physics.addBody
                    (unitCube
                        |> Physics.offsetBy (vec3 -1.2 0 9)
                        |> Physics.rotateBy Vec3.j (-pi / 4)
                    )
                |> Physics.addBody
                    (unitCube
                        |> Physics.offsetBy (vec3 1.3 0 6)
                        |> Physics.rotateBy Vec3.j (pi / 5)
                    )
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize { width, height } ->
            ( { model
                | screenWidth = toFloat width
                , screenHeight = toFloat height
              }
            , Cmd.none
            )

        AddRandomBox ->
            ( model, Random.generate AddBox randomlyRotatedUnitCube )

        AddBox generatedBox ->
            ( { model | world = Physics.addBody generatedBox model.world }
            , Cmd.none
            )

        Tick dt ->
            ( { model | world = Physics.step (1 / 60) model.world }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]



-- View:


view : Model -> Html Msg
view { screenWidth, screenHeight, world } =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0 0 0 1
        ]
        [ width (round (screenWidth * devicePixelRatio))
        , height (round (screenHeight * devicePixelRatio))
        , style
            [ ( "display", "block" )
            , ( "width", toString screenWidth ++ "px" )
            , ( "height", toString screenHeight ++ "px" )
            ]
        , onClick AddRandomBox
        ]
        (let
            perspective =
                Mat4.makePerspective 24 (screenWidth / screenHeight) 5 2000
         in
            (Physics.foldl (addShape perspective) [] world)
                |> (if debugContacts then
                        (addContacts perspective world)
                    else
                        (\unchanged -> unchanged)
                   )
        )


{-! Set to True to see collision points
-}
debugContacts : Bool
debugContacts = False

{-| The constant point of view used for rendering the world.
-}
viewCamera =
    Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k


{-| A light-weight wrapper record to distinguish use of Vec3 for color levels
from its more common other use for 3-D coordinates.
-}
type alias ColorVector =
    { color : Vec3 }


{-| a color constructed by combining red, blue, and green levels
each ranging from 0 to 1
-}
colorVector : Float -> Float -> Float -> ColorVector
colorVector r g b =
    { color = (vec3 r g b) }


{-| a shade of grey specified as a level ranging from 0 (black) to 1 (white)
-}
greyLevel : Float -> ColorVector
greyLevel level =
    -- Mix equal parts red, blue, and green
    colorVector level level level


addShape : Mat4 -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape perspective { transform, bodyId } =
    (::)
        (WebGL.entity
            vertex
            fragment
            (-- This is hardcoded for now, because the plane body is the first added.
             -- TODO: pull the mesh info from somewhere else, using the bodyId and shapeId
             if bodyId == 0 then
                planeMesh
             else
                cubeMesh
            )
            { camera = viewCamera
            , perspective = perspective
            , transform = transform
            }
        )



-- Meshes:


{-| A WebGL compliant record.
-}
type alias GLAttributes =
    { position : Vec3
    , color : Vec3
    }


type alias GLMesh =
    Mesh GLAttributes


planeMesh : GLMesh
planeMesh =
    WebGL.triangles
        (face
            (vec3 10 10 0)
            (vec3 -10 10 0)
            (vec3 -10 -10 0)
            (vec3 10 -10 0)
            (greyLevel 0.2)
        )


cubeMesh : GLMesh
cubeMesh =
    let
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        [ face rft rfb rbb rbt (greyLevel 0.4)
        , face rft rfb lfb lft (greyLevel 0.5)
        , face rft lft lbt rbt (greyLevel 0.6)
        , face rfb lfb lbb rbb (greyLevel 0.7)
        , face lft lfb lbb lbt (greyLevel 0.8)
        , face rbt rbb lbb lbt (greyLevel 0.9)
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> ColorVector -> List ( GLAttributes, GLAttributes, GLAttributes )
face a b c d { color } =
    [ ( GLAttributes a color, GLAttributes b color, GLAttributes c color )
    , ( GLAttributes c color, GLAttributes d color, GLAttributes a color )
    ]



-- Shaders:


{-| Another WebGL compliant record.
-}
type alias GLUniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    }


{-| Yet another WebGL compliant record.
-}
type alias GLVarying =
    { vcolor : Vec3 }


type alias GLShader =
    Shader GLAttributes GLUniforms GLVarying


type alias GLFragmentShader =
    Shader {} GLUniforms GLVarying


vertex : GLShader
vertex =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec3 vcolor;
        void main () {
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
          vcolor = color;
        }
    |]


fragment : GLFragmentShader
fragment =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
          gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


{-| Render collision points for the purpose of debugging
-}
addContacts : Mat4 -> Physics.World -> List Entity -> List Entity
addContacts perspective world entities =
    List.foldl
        (\contactPoint ->
            (::)
                (WebGL.entity
                    vertex
                    fragment
                    cubeMesh
                    { camera = viewCamera
                    , perspective = perspective
                    , transform =
                        Mat4.mul
                            (Mat4.makeTranslate contactPoint)
                            (Mat4.makeScale (vec3 0.1 0.1 0.1))
                    }
                )
        )
        entities
        (Physics.contacts world)
