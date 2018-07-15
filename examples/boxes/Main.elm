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
    { screenWidth : Int
    , screenHeight : Int
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


{-| A constant cube-shaped body with unit sides and mass of 5
-}
box : Physics.Body
box =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator Physics.Body
randomlyRotatedBox =
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
    let
        initialBodies =
            [ Physics.body
                |> Physics.addShape Physics.plane
                |> Physics.offsetBy (vec3 0 0 -1)
            , box
                |> Physics.offsetBy (vec3 0 0 2)
                |> Physics.rotateBy Vec3.j (-pi / 5)
            , box
                |> Physics.offsetBy (vec3 -1.2 0 9)
                |> Physics.rotateBy Vec3.j (-pi / 4)
            , box
                |> Physics.offsetBy (vec3 1.3 0 6)
                |> Physics.rotateBy Vec3.j (pi / 5)
            ]

        initialWorld =
            Physics.world
                |> Physics.setGravity (vec3 0 0 -10)
    in
        ( { -- replaced by resize, including the initial resize
            screenWidth = 1
          , screenHeight = 1

          -- continuously updated by ticks and clicks
          , world = List.foldl Physics.addBody initialWorld initialBodies
          }
        , Task.perform Resize Window.size
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize { width, height } ->
            ( { model
                | screenWidth = width
                , screenHeight = height
              }
            , Cmd.none
            )

        AddRandomBox ->
            ( model, Random.generate AddBox randomlyRotatedBox )

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
        , WebGL.clearColor 0.3 0.3 0.3 1
        ]
        [ width screenWidth
        , height screenHeight
        , style
            [ ( "display", "block" )
            , ( "width", toString screenWidth ++ "px" )
            , ( "height", toString screenHeight ++ "px" )
            ]
        , onClick AddRandomBox
        ]
        (let
            lightDirection =
                Vec3.normalize (vec3 -1 -1 -1)

            camera =
                Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k

            aspectRatio =
                (toFloat screenWidth) / (toFloat screenHeight)

            perspective =
                Mat4.makePerspective 24 aspectRatio 5 2000
         in
            Physics.foldl (addShape lightDirection camera perspective) [] world
                |> (if debugContacts then
                        addContacts lightDirection camera perspective world
                    else
                        identity
                   )
        )


{-| Set to True to see collision points
-}
debugContacts : Bool
debugContacts =
    False


addShape : Vec3 -> Mat4 -> Mat4 -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape lightDirection camera perspective { transform, bodyId } tail =
    case bodyId of
        0 ->
            -- This is hardcoded for now, because the plane body is the first added.
            -- TODO: pull the mesh info from somewhere else, using the bodyId and shapeId
            tail

        _ ->
            -- Draw a shadow
            WebGL.entity
                vertex
                shadowFragment
                cubeMesh
                { camera = camera
                , color = vec3 0.25 0.25 0.25
                , lightDirection = lightDirection
                , perspective = perspective
                , transform =
                    transform
                        -- project on the floor
                        |> Mat4.mul (shadow (vec3 0 0 -1) Vec3.k lightDirection)
                }
                :: WebGL.entity
                    vertex
                    fragment
                    cubeMesh
                    { camera = camera
                    , color = vec3 0.9 0.9 0.9
                    , lightDirection = lightDirection
                    , perspective = perspective
                    , transform = transform
                    }
                :: tail



-- Meshes:


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


cubeMesh : Mesh Attributes
cubeMesh =
    let
        v0 =
            vec3 -1 -1 -1

        v1 =
            vec3 1 -1 -1

        v2 =
            vec3 1 1 -1

        v3 =
            vec3 -1 1 -1

        v4 =
            vec3 -1 -1 1

        v5 =
            vec3 1 -1 1

        v6 =
            vec3 1 1 1

        v7 =
            vec3 -1 1 1
    in
        [ face v3 v2 v1 v0
        , face v4 v5 v6 v7
        , face v5 v4 v0 v1
        , face v2 v3 v7 v6
        , face v0 v4 v7 v3
        , face v1 v2 v6 v5
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
face a b c d =
    let
        normal =
            Vec3.cross (Vec3.sub b a) (Vec3.sub b c)
    in
        [ ( Attributes a normal
          , Attributes b normal
          , Attributes c normal
          )
        , ( Attributes c normal
          , Attributes d normal
          , Attributes a normal
          )
        ]



-- Shaders:


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    , color : Vec3
    , lightDirection : Vec3
    }


type alias Varying =
    { vlighting : Float }


vertex : Shader Attributes Uniforms Varying
vertex =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        uniform vec3 lightDirection;
        varying float vlighting;
        void main () {
          float ambientLight = 0.4;
          float directionalLight = 0.6;
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
          vec4 transformedNormal = normalize(transform * vec4(normal, 0.0));
          float directional = max(dot(transformedNormal.xyz, lightDirection), 0.0);
          vlighting = ambientLight + directional * directionalLight;
        }
    |]


fragment : Shader {} Uniforms Varying
fragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying float vlighting;
        void main () {
          gl_FragColor = vec4(vlighting * color, 1.0);
        }
    |]


shadowFragment : Shader {} Uniforms Varying
shadowFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying float vlighting;
        void main () {
          gl_FragColor = vec4(color, 1);
        }
    |]


{-| Render collision points for the purpose of debugging
-}
addContacts : Vec3 -> Mat4 -> Mat4 -> Physics.World -> List Entity -> List Entity
addContacts lightDirection camera perspective world entities =
    let
        addContact : Vec3 -> List Entity -> List Entity
        addContact contactPoint tail =
            WebGL.entity
                vertex
                fragment
                cubeMesh
                { camera = camera
                , perspective = perspective
                , color = vec3 1 0 0
                , lightDirection = lightDirection
                , transform =
                    Mat4.mul
                        (Mat4.makeTranslate contactPoint)
                        (Mat4.makeScale (vec3 0.1 0.1 0.1))
                }
                :: tail
    in
        List.foldl
            addContact
            entities
            (Physics.contacts world)


{-| A "squash" matrix that smashes things to the ground plane,
defined by position, normal, parallel to a given light vector
-}
shadow : Vec3 -> Vec3 -> Vec3 -> Mat4
shadow position normal light =
    let
        n =
            Vec3.toRecord normal

        nw =
            -(Vec3.dot position normal)

        l =
            Vec3.toRecord light

        d =
            Vec3.dot normal light
    in
        Mat4.fromRecord
            { m11 = l.x * n.x - d
            , m21 = l.y * n.x
            , m31 = l.z * n.x
            , m41 = 0
            , m12 = l.x * n.y
            , m22 = l.y * n.y - d
            , m32 = l.z * n.y
            , m42 = 0
            , m13 = l.x * n.z
            , m23 = l.y * n.z
            , m33 = l.z * n.z - d
            , m43 = 0
            , m14 = l.x * nw
            , m24 = l.y * nw
            , m34 = l.z * nw
            , m44 = -d
            }
