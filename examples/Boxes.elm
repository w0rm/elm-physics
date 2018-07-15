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
import Common.Meshes as Meshes exposing (Attributes)
import Common.Math as Math
import Common.Shaders as Shaders


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


planeOffset : Vec3
planeOffset =
    vec3 0 0 -1


init : ( Model, Cmd Msg )
init =
    let
        initialBodies =
            [ Physics.body
                |> Physics.addShape Physics.plane
                |> Physics.offsetBy planeOffset
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
                |> (if debugNormals then
                        addNormals lightDirection camera perspective world
                    else
                        identity
                   )
                |> (if debugEdges then
                        addUniqueEdges lightDirection camera perspective world
                    else
                        identity
                   )
        )


{-| Set to True to see collision points
-}
debugContacts : Bool
debugContacts =
    False


{-| Set to True to see normal spikes
-}
debugNormals : Bool
debugNormals =
    True


{-| Set to True to see edge markers
-}
debugEdges : Bool
debugEdges =
    True


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
                Shaders.vertex
                Shaders.shadowFragment
                cubeMesh
                { camera = camera
                , color = vec3 0.25 0.25 0.25
                , lightDirection = lightDirection
                , perspective = perspective
                , transform =
                    transform
                        -- project on the floor
                        |> Mat4.mul
                            (Math.makeShadow
                                planeOffset
                                Vec3.k
                                lightDirection
                            )
                }
                :: WebGL.entity
                    Shaders.vertex
                    Shaders.fragment
                    cubeMesh
                    { camera = camera
                    , color = vec3 0.9 0.9 0.9
                    , lightDirection = lightDirection
                    , perspective = perspective
                    , transform = transform
                    }
                :: tail



-- Meshes


cubeMesh : Mesh Attributes
cubeMesh =
    Meshes.makeBox (vec3 1 1 1)


normalMesh : Mesh Attributes
normalMesh =
    Meshes.makePyramid 0.05 0.05


edgeMesh : Mesh Attributes
edgeMesh =
    Meshes.makePyramid 0.1 0.5


contactMesh : Mesh Attributes
contactMesh =
    Meshes.makeBox (vec3 0.1 0.1 0.1)


{-| Render collision points for the purpose of debugging
-}
addContacts : Vec3 -> Mat4 -> Mat4 -> Physics.World -> List Entity -> List Entity
addContacts lightDirection camera perspective world entities =
    let
        addContact : Vec3 -> List Entity -> List Entity
        addContact contactPoint tail =
            WebGL.entity
                Shaders.vertex
                Shaders.fragment
                contactMesh
                { camera = camera
                , perspective = perspective
                , color = vec3 1 0 0
                , lightDirection = lightDirection
                , transform =
                    Mat4.makeTranslate contactPoint
                }
                :: tail
    in
        List.foldl
            addContact
            entities
            (Physics.contacts world)


{-| Render shape face normals for the purpose of debugging
-}
addNormals : Vec3 -> Mat4 -> Mat4 -> Physics.World -> List Entity -> List Entity
addNormals lightDirection camera perspective world entities =
    let
        addNormalIndicator : Mat4 -> Vec3 -> Vec3 -> List Entity -> List Entity
        addNormalIndicator transform normal facePoint tail =
            WebGL.entity
                Shaders.vertex
                Shaders.fragment
                normalMesh
                { camera = camera
                , lightDirection = lightDirection
                , color = vec3 1 0 1
                , perspective = perspective
                , transform =
                    Math.makeRotateKTo normal
                        |> Mat4.mul
                            (facePoint
                                |> Mat4.makeTranslate
                                |> Mat4.mul transform
                            )
                }
                :: tail
    in
        Physics.foldFaceNormals
            addNormalIndicator
            entities
            world


{-| Render shapes' unique edges for the purpose of debugging
-}
addUniqueEdges : Vec3 -> Mat4 -> Mat4 -> Physics.World -> List Entity -> List Entity
addUniqueEdges lightDirection camera perspective world entities =
    let
        addEdgeIndicator : Mat4 -> Vec3 -> Vec3 -> List Entity -> List Entity
        addEdgeIndicator transform edge origin tail =
            WebGL.entity
                Shaders.vertex
                Shaders.fragment
                edgeMesh
                { camera = camera
                , lightDirection = lightDirection
                , color = vec3 0 1 0
                , perspective = perspective
                , transform =
                    Math.makeRotateKTo edge
                        |> Mat4.mul
                            (origin
                                |> Mat4.makeTranslate
                                |> Mat4.mul transform
                            )
                }
                :: tail
    in
        Physics.foldUniqueEdges
            addEdgeIndicator
            entities
            world
