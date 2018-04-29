module Boxes exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
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
    , devicePixelRatio : Float
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


box : Physics.Body
box =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))


randomBox : Random.Generator Physics.Body
randomBox =
    Random.map4
        (\angle x y z ->
            box
                |> Physics.offsetBy (vec3 0 0 10)
                |> Physics.rotateBy (Vec3.normalize (vec3 x y z)) angle
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


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
                    (box
                        |> Physics.offsetBy (vec3 0 0 2)
                        |> Physics.rotateBy Vec3.j (-pi / 5)
                    )
                |> Physics.addBody
                    (box
                        |> Physics.offsetBy (vec3 -1.2 0 9)
                        |> Physics.rotateBy Vec3.j (-pi / 4)
                    )
                |> Physics.addBody
                    (box
                        |> Physics.offsetBy (vec3 1.3 0 6)
                        |> Physics.rotateBy Vec3.j (pi / 5)
                    )
      , devicePixelRatio = 1
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
            ( model, Random.generate AddBox randomBox )

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
view { screenWidth, screenHeight, devicePixelRatio, world } =
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
            camera =
                Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k

            perspective =
                Mat4.makePerspective 24 (screenWidth / screenHeight) 5 2000

            entities =
                Physics.foldl (addShape camera perspective) [] world

            -- Uncomment to see collision points
            -- |> addContacts camera perspective (Physics.contacts world)
         in
            entities
        )


type alias Attributes =
    { position : Vec3
    , color : Vec3
    }


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    }


addShape : Mat4 -> Mat4 -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape camera perspective { transform, bodyId } =
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
            { transform = transform
            , perspective = perspective
            , camera = camera
            }
        )



-- Meshes:


planeMesh : Mesh Attributes
planeMesh =
    WebGL.triangles
        (face
            (vec3 10 10 0)
            (vec3 -10 10 0)
            (vec3 -10 -10 0)
            (vec3 10 -10 0)
            (vec3 0.2 0.2 0.2)
        )


cubeMesh : Mesh Attributes
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
        [ face rft rfb rbb rbt (vec3 0.4 0.4 0.4)
        , face rft rfb lfb lft (vec3 0.5 0.5 0.5)
        , face rft lft lbt rbt (vec3 0.6 0.6 0.6)
        , face rfb lfb lbb rbb (vec3 0.7 0.7 0.7)
        , face lft lfb lbb lbt (vec3 0.8 0.8 0.8)
        , face rbt rbb lbb lbt (vec3 0.9 0.9 0.9)
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
face a b c d color =
    [ ( Attributes a color, Attributes b color, Attributes c color )
    , ( Attributes c color, Attributes d color, Attributes a color )
    ]



-- Shaders:


vertex : Shader Attributes Uniforms { vcolor : Vec3 }
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


fragment : Shader {} Uniforms { vcolor : Vec3 }
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
addContacts : Mat4 -> Mat4 -> List Vec3 -> List Entity -> List Entity
addContacts camera perspective contacts entities =
    List.foldl
        (\contactPoint ->
            (::)
                (WebGL.entity
                    vertex
                    fragment
                    cubeMesh
                    { transform =
                        Mat4.mul
                            (Mat4.makeTranslate contactPoint)
                            (Mat4.makeScale (vec3 0.1 0.1 0.1))
                    , perspective = perspective
                    , camera = camera
                    }
                )
        )
        entities
        contacts
