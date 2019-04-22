module Common.Demo exposing
    ( Demo
    , DemoProgram
    , addBodies
    , demo
    , dropOnClick
    , run
    )

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
import Html.Events exposing (on, onCheck, onClick)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Body as Body exposing (Body)
import Physics.Debug as Debug
import Physics.World as World exposing (RaycastResult, World)
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
    , initialWorld : World DemoBodyWithId
    , world : World DemoBodyWithId
    , dt : List Float
    , raycastResult : Maybe (RaycastResult DemoBodyWithId)
    , lastId : Int
    }


type alias DemoBodyWithId =
    { id : Int
    , mesh : Mesh Attributes
    , wireframe : Mesh Attributes
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
    | SceneClick (Maybe (RaycastResult DemoBodyWithId))
    | AddBody (Body DemoBody)



-- App


{-| Protect the config from being directly modifiable
-}
type Demo
    = Demo DemoConfig


type alias DemoConfig =
    { bodies : List (Body DemoBody)
    , randomBody : Maybe (Random.Generator (Body DemoBody))
    }


planeOffset : { x : Float, y : Float, z : Float }
planeOffset =
    { x = 0, y = 0, z = -1 }


{-| Initial demo configuration
-}
demo : Demo
demo =
    Demo
        { bodies = [ Body.moveBy planeOffset Bodies.plane ]
        , randomBody = Nothing
        }


{-| Allow to drop random bodies on click
-}
dropOnClick : Random.Generator (Body DemoBody) -> Demo -> Demo
dropOnClick randomBody (Demo demo_) =
    Demo { demo_ | randomBody = Just randomBody }


{-| Add initial bodies for the scene
-}
addBodies : List (Body DemoBody) -> Demo -> Demo
addBodies bodiesWithMeshes (Demo demo_) =
    Demo { demo_ | bodies = List.foldl (::) demo_.bodies bodiesWithMeshes }


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
    let
        ( initialWorld, lastId ) =
            List.foldl
                (\body ( currentWorld, currentLastId ) ->
                    let
                        { mesh, wireframe } =
                            Body.getData body

                        bodyWithId =
                            Body.setData { id = currentLastId, mesh = mesh, wireframe = wireframe } body
                    in
                    ( World.add bodyWithId currentWorld, currentLastId + 1 )
                )
                ( World.setGravity { x = 0, y = 0, z = -10 } World.empty, 0 )
                demo_.bodies
    in
    ( { debugContacts = False
      , debugNormals = False
      , debugEdges = False
      , debugWireframes = False
      , showSettings = False
      , showFpsMeter = False

      -- replaced by resize, including the initial resize
      , screenWidth = 1
      , screenHeight = 1
      , initialWorld = initialWorld
      , world = initialWorld
      , dt = [ 16 ]
      , raycastResult = Nothing
      , lastId = lastId
      }
    , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
    )


update : Maybe (Random.Generator (Body DemoBody)) -> Msg -> Model -> ( Model, Cmd Msg )
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
                | world = World.simulate (1000 / 60) model.world
                , dt = List.take 50 (dt :: model.dt)
              }
            , Cmd.none
            )

        SceneClick raycastResult ->
            case ( raycastResult, randomBody ) of
                ( Just result, _ ) ->
                    ( { model | raycastResult = raycastResult }, Cmd.none )

                ( Nothing, Just body_ ) ->
                    ( { model | raycastResult = raycastResult }, Random.generate AddBody body_ )

                _ ->
                    ( model, Cmd.none )

        ResetClick ->
            ( { model | world = model.initialWorld, raycastResult = Nothing, lastId = 0 }
            , Cmd.none
            )

        AddBody body ->
            let
                { mesh, wireframe } =
                    Body.getData body

                bodyWithId =
                    Body.setData { id = model.lastId, mesh = mesh, wireframe = wireframe } body
            in
            ( { model | world = World.add bodyWithId model.world, lastId = model.lastId + 1 }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onAnimationFrameDelta Tick
        ]


type alias SceneParams =
    { lightDirection : Vec3
    , camera : Mat4
    , perspective : Mat4
    , debugWireframes : Bool
    , debugNormals : Bool
    , debugEdges : Bool
    , raycastResult : Maybe (RaycastResult DemoBodyWithId)
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
fpsMeter { showFpsMeter, dt, world } =
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
            List.length (World.getBodies world)
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


onMouseClick : (Float -> Float -> Msg) -> Html.Attribute Msg
onMouseClick tagger =
    on "click"
        (Json.Decode.map2
            tagger
            (Json.Decode.field "pageX" Json.Decode.float)
            (Json.Decode.field "pageY" Json.Decode.float)
        )


webGL : Model -> Html Msg
webGL { screenWidth, screenHeight, world, debugContacts, debugNormals, debugEdges, debugWireframes, raycastResult } =
    let
        aspectRatio =
            screenWidth / screenHeight

        camera =
            Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k

        perspective =
            Mat4.makePerspective 24 aspectRatio 5 2000

        sceneParams =
            { lightDirection = Vec3.normalize (vec3 -1 -1 -1)
            , camera = camera
            , perspective = perspective
            , debugWireframes = debugWireframes
            , debugNormals = debugNormals
            , debugEdges = debugEdges
            , raycastResult = raycastResult
            }

        sceneClick x y =
            SceneClick
                (World.raycast
                    { from = { x = 0, y = 30, z = 20 }
                    , direction =
                        Math.mouseDirection
                            { camera = camera
                            , screenWidth = screenWidth
                            , screenHeight = screenHeight
                            , perspective = perspective
                            , x = x
                            , y = y
                            }
                    }
                    world
                )
    in
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.3 0.3 0.3 1
        ]
        [ width (round screenWidth)
        , height (round screenHeight)
        , style "display" "block"
        , onMouseClick sceneClick
        ]
        ([ ( True
           , \entities -> List.foldl (addBodyEntities sceneParams) entities (World.getBodies world)
           )
         , ( debugContacts
           , \entities -> List.foldl (addContactIndicator sceneParams) entities (Debug.getContacts world)
           )
         ]
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.foldl (<|) []
        )


addBodyEntities : SceneParams -> Body DemoBodyWithId -> List Entity -> List Entity
addBodyEntities ({ lightDirection, camera, perspective, debugWireframes, debugEdges, debugNormals, raycastResult } as sceneParams) body entities =
    let
        transform =
            Mat4.fromRecord (Body.getTransformation body)

        addEdges acc =
            if debugEdges then
                List.foldl (addEdgeIndicator sceneParams transform)
                    acc
                    (Debug.getUniqueEdges body)

            else
                acc

        ( color, normals ) =
            case raycastResult of
                Just res ->
                    if (Body.getData res.body).id == (Body.getData body).id then
                        ( vec3 1 0.2 0.2, [ { normal = res.normal, point = res.point } ] )

                    else
                        ( vec3 0.9 0.9 0.9, [] )

                Nothing ->
                    ( vec3 0.9 0.9 0.9, [] )

        addNormals acc =
            if debugNormals then
                List.foldl (addNormalIndicator sceneParams transform)
                    acc
                    (normals ++ Debug.getFaceNormals body)

            else
                acc

        newEntities =
            entities |> addEdges |> addNormals
    in
    if debugWireframes then
        WebGL.entity
            Shaders.vertex
            Shaders.fragment
            (Body.getData body |> .wireframe)
            { camera = camera
            , color = color
            , lightDirection = lightDirection
            , perspective = perspective
            , transform = transform
            }
            :: newEntities

    else
        -- Draw a shadow
        WebGL.entity
            Shaders.vertex
            Shaders.shadowFragment
            (Body.getData body |> .mesh)
            { camera = camera
            , color = vec3 0.25 0.25 0.25
            , lightDirection = lightDirection
            , perspective = perspective
            , transform =
                transform
                    -- project on the floor
                    |> Mat4.mul
                        (Math.makeShadow
                            (Vec3.fromRecord planeOffset)
                            Vec3.k
                            lightDirection
                        )
            }
            -- Draw a mesh
            :: WebGL.entity
                Shaders.vertex
                Shaders.fragment
                (Body.getData body |> .mesh)
                { camera = camera
                , color = color
                , lightDirection = lightDirection
                , perspective = perspective
                , transform = transform
                }
            :: newEntities


{-| Render collision point for the purpose of debugging
-}
addContactIndicator : SceneParams -> { x : Float, y : Float, z : Float } -> List Entity -> List Entity
addContactIndicator { lightDirection, camera, perspective } { x, y, z } tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        contactMesh
        { camera = camera
        , perspective = perspective
        , color = vec3 1 0 0
        , lightDirection = lightDirection
        , transform = Mat4.makeTranslate3 x y z
        }
        :: tail


{-| Render shape face normals for the purpose of debugging
-}
addNormalIndicator : SceneParams -> Mat4 -> { normal : { x : Float, y : Float, z : Float }, point : { x : Float, y : Float, z : Float } } -> List Entity -> List Entity
addNormalIndicator { lightDirection, camera, perspective } transform { normal, point } tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        normalMesh
        { camera = camera
        , lightDirection = lightDirection
        , color = vec3 1 0 1
        , perspective = perspective
        , transform =
            Math.makeRotateKTo (Vec3.fromRecord normal)
                |> Mat4.mul
                    (Vec3.fromRecord point
                        |> Mat4.makeTranslate
                        |> Mat4.mul transform
                    )
        }
        :: tail


{-| Render shapes' unique edge for the purpose of debugging
-}
addEdgeIndicator : SceneParams -> Mat4 -> { direction : { x : Float, y : Float, z : Float }, point : { x : Float, y : Float, z : Float } } -> List Entity -> List Entity
addEdgeIndicator { lightDirection, camera, perspective } transform { direction, point } tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        edgeMesh
        { camera = camera
        , lightDirection = lightDirection
        , color = vec3 0 1 0
        , perspective = perspective
        , transform =
            Math.makeRotateKTo (Vec3.fromRecord direction)
                |> Mat4.mul
                    (Vec3.fromRecord point
                        |> Mat4.makeTranslate
                        |> Mat4.mul transform
                    )
        }
        :: tail



-- Meshes


normalMesh : Mesh Attributes
normalMesh =
    Meshes.toMesh (Meshes.pyramid 0.05 0.05)


edgeMesh : Mesh Attributes
edgeMesh =
    Meshes.toMesh (Meshes.pyramid 0.1 0.5)


contactMesh : Mesh Attributes
contactMesh =
    Meshes.toMesh (Meshes.sphere 2 0.07)
