module Common.Demo exposing
    ( Demo
    , DemoProgram
    , addBodies
    , demo
    , dropOnClick
    , run
    )

import AltMath.Matrix4 as AltMat4
import AltMath.Vector3 as AltVec3
import AltPhysics exposing (World)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Common.Bodies as Bodies exposing (DemoBody)
import Common.Math as Math
import Common.Meshes as Meshes exposing (Attributes)
import Common.Shaders as Shaders
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (checked, height, style, type_, width)
import Html.Events exposing (onCheck, onClick)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random
import Task
import WebGL exposing (Entity, Mesh, Shader)



-- Model


type alias Model =
    { debugContacts : Bool -- Set to True to see collision points
    , debugNormals : Bool -- Set to True to see normal spikes
    , debugEdges : Bool -- Set to True to see edge markers
    , debugWireframes : Bool -- Set to True to see wireframes
    , showFpsMeter : Bool
    , showSettings : Bool
    , screenWidth : Float
    , screenHeight : Float
    , initialWorld : World
    , initialBodies : Dict Int DemoBody
    , world : World
    , bodies : Dict Int DemoBody
    , dt : List Float
    }



-- Msg


type Msg
    = Tick Float
    | ToggleContacts Bool
    | ToggleNormals Bool
    | ToggleEdges Bool
    | ToggleWireframes Bool
    | ToggleFpsMeter Bool
    | ToggleSettings
    | Resize Float Float
    | ResetClick
    | SceneClick
    | AddBody ( DemoBody, AltPhysics.Body )



-- App


{-| Protect the config from being directly modifiable
-}
type Demo
    = Demo DemoConfig


type alias DemoConfig =
    { world : World
    , randomBody : Maybe (Random.Generator ( DemoBody, AltPhysics.Body ))
    , bodies : Dict Int DemoBody
    }


{-| Initial demo configuration
-}
demo : Demo
demo =
    Demo
        { world =
            AltPhysics.world
                |> AltPhysics.setGravity (AltVec3.vec3 0 0 -10)
                |> AltPhysics.addBody plane
                |> Tuple.first
        , bodies = Dict.empty
        , randomBody = Nothing
        }


planeOffset : AltVec3.Vec3
planeOffset =
    AltVec3.vec3 0 0 -1


plane : AltPhysics.Body
plane =
    AltPhysics.body
        |> AltPhysics.addShape AltPhysics.plane
        |> Tuple.first
        |> AltPhysics.offsetBy planeOffset


{-| Allow to drop random bodies on click
-}
dropOnClick : Random.Generator ( DemoBody, AltPhysics.Body ) -> Demo -> Demo
dropOnClick randomBody (Demo demo_) =
    Demo { demo_ | randomBody = Just randomBody }


{-| Add initial bodies for the scene
-}
addBodies : List ( DemoBody, AltPhysics.Body ) -> Demo -> Demo
addBodies bodiesWithMeshes (Demo demo_) =
    Demo
        (List.foldl addBodyWithMesh demo_ bodiesWithMeshes)


addBodyWithMesh : ( DemoBody, AltPhysics.Body ) -> { a | world : World, bodies : Dict Int DemoBody } -> { a | world : World, bodies : Dict Int DemoBody }
addBodyWithMesh ( mesh, body ) demo_ =
    let
        ( world, bodyId ) =
            AltPhysics.addBody body demo_.world
    in
    { demo_
        | world = world
        , bodies = Dict.insert bodyId mesh demo_.bodies
    }


type alias DemoProgram =
    Program Value Model Msg


{-| Run the demo as an Elm program!
-}
run : Demo -> DemoProgram
run (Demo demo_) =
    Browser.element
        { init = \_ -> init demo_
        , update = update demo_.randomBody
        , subscriptions = subscriptions
        , view = view
        }


init : DemoConfig -> ( Model, Cmd Msg )
init demo_ =
    ( { debugContacts = False
      , debugNormals = False
      , debugEdges = False
      , debugWireframes = False
      , showSettings = False
      , showFpsMeter = False

      -- replaced by resize, including the initial resize
      , screenWidth = 1
      , screenHeight = 1
      , initialWorld = demo_.world
      , initialBodies = demo_.bodies
      , world = demo_.world
      , bodies = demo_.bodies
      , dt = [ 16 ]
      }
    , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
    )


update : Maybe (Random.Generator ( DemoBody, AltPhysics.Body )) -> Msg -> Model -> ( Model, Cmd Msg )
update randomBody msg model =
    case msg of
        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        ToggleContacts debugContacts ->
            ( { model | debugContacts = debugContacts }, Cmd.none )

        ToggleNormals debugNormals ->
            ( { model | debugNormals = debugNormals }, Cmd.none )

        ToggleEdges debugEdges ->
            ( { model | debugEdges = debugEdges }, Cmd.none )

        ToggleWireframes debugWireframes ->
            ( { model | debugWireframes = debugWireframes }, Cmd.none )

        ToggleFpsMeter showFpsMeter ->
            ( { model | showFpsMeter = showFpsMeter }, Cmd.none )

        Resize width height ->
            ( { model
                | screenWidth = width
                , screenHeight = height
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | world = AltPhysics.step (1 / 60) model.world
                , dt = List.take 50 (dt :: model.dt)
              }
            , Cmd.none
            )

        SceneClick ->
            case randomBody of
                Nothing ->
                    ( model, Cmd.none )

                Just body_ ->
                    ( model, Random.generate AddBody body_ )

        ResetClick ->
            ( { model
                | world = model.initialWorld
                , bodies = model.initialBodies
              }
            , Cmd.none
            )

        AddBody bodyAndMesh ->
            ( addBodyWithMesh bodyAndMesh model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onAnimationFrameDelta Tick
        ]


type alias SceneParams =
    { lightDirection : AltVec3.Vec3
    , camera : AltMat4.Mat4
    , perspective : AltMat4.Mat4
    , bodies : Dict Int DemoBody
    , debugWireframes : Bool
    }


view : Model -> Html Msg
view model =
    Html.div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        ]
        [ webGL model
        , settings model
        , fpsMeter model
        ]


fpsMeter : Model -> Html Msg
fpsMeter { showFpsMeter, dt, bodies } =
    let
        average currentWeight sumOfWeights weightedSum list =
            case list of
                [] ->
                    weightedSum / sumOfWeights

                el :: rest ->
                    average
                        (currentWeight * 0.9)
                        (currentWeight + sumOfWeights)
                        (el * currentWeight + weightedSum)
                        rest

        numBodies =
            Dict.size bodies
    in
    Html.div
        [ style "position" "fixed"
        , style "font-family" "monospaced"
        , style "right" "250px"
        , style "top" "0"
        , style "color" "white"
        ]
        (if showFpsMeter then
            [ Html.span [ style "font" "50px sans-serif" ]
                [ Html.text (String.fromInt (round (1000 / average 1 0 0 dt))) ]
            , Html.text " fps"
            , Html.span
                [ style "font" "50px sans-serif" ]
                [ Html.text (" " ++ String.fromInt numBodies) ]
            , Html.text " bodies"
            ]

         else
            []
        )


settings : Model -> Html Msg
settings { showSettings, debugContacts, debugEdges, debugNormals, debugWireframes, showFpsMeter } =
    Html.div
        [ style "position" "fixed"
        , style "right" "6px"
        , style "top" "0"
        , style "font-family" "monospace"
        , style "color" "white"
        ]
        (if showSettings then
            [ button ToggleSettings "Hide Settings"
            , Html.div
                [ style "padding" "6px"
                , style "min-width" "24ch"
                , style "background" "rgb(50, 50, 50)"
                , style "border-radius" "0 0 4px 4px"
                ]
                [ checkbox ToggleContacts debugContacts "collision points"
                , checkbox ToggleNormals debugNormals "normals"
                , checkbox ToggleEdges debugEdges "unique edges"
                , checkbox ToggleWireframes debugWireframes "wireframes"
                , checkbox ToggleFpsMeter showFpsMeter "fps meter"
                , Html.button [ onClick ResetClick, style "margin" "10px 0 5px" ] [ Html.text "Click to restart the demo" ]
                ]
            ]

         else
            [ button ToggleSettings "Show Settings" ]
        )


button : Msg -> String -> Html Msg
button msg text =
    Html.button
        [ style "padding" "6px"
        , style "box-sizing" "content-box"
        , style "min-width" "24ch"
        , style "color" "inherit"
        , style "border" "none"
        , style "font" "inherit"
        , style "text-align" "center"
        , style "margin" "0"
        , style "display" "block"
        , style "background" "rgb(61, 61, 61)"
        , onClick msg
        ]
        [ Html.text text ]


checkbox : (Bool -> Msg) -> Bool -> String -> Html Msg
checkbox msg value label =
    Html.label [ style "display" "block", style "padding" "5px 0" ]
        [ Html.input
            [ onCheck msg
            , checked value
            , type_ "checkbox"
            , style "margin-right" "10px"
            ]
            []
        , Html.text label
        ]


webGL : Model -> Html Msg
webGL { screenWidth, screenHeight, world, bodies, debugContacts, debugNormals, debugEdges, debugWireframes } =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.3 0.3 0.3 1
        ]
        [ width (round screenWidth)
        , height (round screenHeight)
        , style "display" "block"
        , onClick SceneClick
        ]
        (let
            aspectRatio =
                screenWidth / screenHeight

            sceneParams =
                { lightDirection = AltVec3.normalize (AltVec3.vec3 -1 -1 -1)
                , camera = AltMat4.makeLookAt (AltVec3.vec3 0 30 20) (AltVec3.vec3 0 0 0) AltVec3.k
                , perspective = AltMat4.makePerspective 24 aspectRatio 5 2000
                , bodies = bodies
                , debugWireframes = debugWireframes
                }
         in
         [ ( True, AltPhysics.fold (addShape sceneParams) )
         , ( debugContacts
           , AltPhysics.foldContacts (addContactIndicator sceneParams)
           )
         , ( debugNormals
           , AltPhysics.foldFaceNormals (addNormalIndicator sceneParams)
           )
         , ( debugEdges
           , AltPhysics.foldUniqueEdges (addEdgeIndicator sceneParams)
           )
         ]
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.foldl (\fn entities -> fn entities world) []
        )


addShape : SceneParams -> { transform : AltMat4.Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape { lightDirection, bodies, camera, perspective, debugWireframes } { transform, bodyId } tail =
    case ( Dict.get bodyId bodies, debugWireframes ) of
        ( Nothing, _ ) ->
            tail

        ( Just demoBody, True ) ->
            WebGL.entity
                Shaders.vertex
                Shaders.fragment
                (Bodies.getWireframe demoBody)
                { camera = Mat4.fromRecord camera
                , color = vec3 0.9 0.9 0.9
                , lightDirection = Vec3.fromRecord lightDirection
                , perspective = Mat4.fromRecord perspective
                , transform = Mat4.fromRecord transform
                }
                :: tail

        ( Just demoBody, False ) ->
            -- Draw a shadow
            WebGL.entity
                Shaders.vertex
                Shaders.shadowFragment
                (Bodies.getMesh demoBody)
                { camera = Mat4.fromRecord camera
                , color = vec3 0.25 0.25 0.25
                , lightDirection = Vec3.fromRecord lightDirection
                , perspective = Mat4.fromRecord perspective
                , transform =
                    transform
                        -- project on the floor
                        |> AltMat4.mul
                            (Math.makeShadow
                                planeOffset
                                AltVec3.k
                                lightDirection
                            )
                        |> Mat4.fromRecord
                }
                -- Draw a mesh
                :: WebGL.entity
                    Shaders.vertex
                    Shaders.fragment
                    (Bodies.getMesh demoBody)
                    { camera = Mat4.fromRecord camera
                    , color = vec3 0.9 0.9 0.9
                    , lightDirection = Vec3.fromRecord lightDirection
                    , perspective = Mat4.fromRecord perspective
                    , transform = Mat4.fromRecord transform
                    }
                :: tail


{-| Render collision point for the purpose of debugging
-}
addContactIndicator : SceneParams -> AltVec3.Vec3 -> List Entity -> List Entity
addContactIndicator { lightDirection, camera, perspective } contactPoint tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        contactMesh
        { camera = Mat4.fromRecord camera
        , perspective = Mat4.fromRecord perspective
        , color = vec3 1 0 0
        , lightDirection = Vec3.fromRecord lightDirection
        , transform =
            Mat4.makeTranslate (Vec3.fromRecord contactPoint)
        }
        :: tail


{-| Render shape face normals for the purpose of debugging
-}
addNormalIndicator : SceneParams -> AltMat4.Mat4 -> AltVec3.Vec3 -> AltVec3.Vec3 -> List Entity -> List Entity
addNormalIndicator { lightDirection, camera, perspective } transform normal facePoint tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        normalMesh
        { camera = Mat4.fromRecord camera
        , lightDirection = Vec3.fromRecord lightDirection
        , color = vec3 1 0 1
        , perspective = Mat4.fromRecord perspective
        , transform =
            Math.makeRotateKTo normal
                |> AltMat4.mul
                    (facePoint
                        |> AltMat4.makeTranslate
                        |> AltMat4.mul transform
                    )
                |> Mat4.fromRecord
        }
        :: tail


{-| Render shapes' unique edge for the purpose of debugging
-}
addEdgeIndicator : SceneParams -> AltMat4.Mat4 -> AltVec3.Vec3 -> AltVec3.Vec3 -> List Entity -> List Entity
addEdgeIndicator { lightDirection, camera, perspective } transform edge origin tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        edgeMesh
        { camera = Mat4.fromRecord camera
        , lightDirection = Vec3.fromRecord lightDirection
        , color = vec3 0 1 0
        , perspective = Mat4.fromRecord perspective
        , transform =
            Math.makeRotateKTo edge
                |> AltMat4.mul
                    (origin
                        |> AltMat4.makeTranslate
                        |> AltMat4.mul transform
                    )
                |> Mat4.fromRecord
        }
        :: tail



-- Meshes


normalMesh : Mesh Attributes
normalMesh =
    Meshes.makePyramid 0.05 0.05


edgeMesh : Mesh Attributes
edgeMesh =
    Meshes.makePyramid 0.1 0.5


contactMesh : Mesh Attributes
contactMesh =
    Meshes.makeSphere 2 0.07
