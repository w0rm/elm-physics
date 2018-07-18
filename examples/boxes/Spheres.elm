module Boxes exposing (main)

import AnimationFrame
import Dict exposing (Dict)
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
    , shapes : ShapeDict
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | AddRandomShape
    | AddShape ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| A cube-shaped body with unit sides, default mass 5,
and custom orientation.
Orienting the body with a function (chain) prior to addShape makes chaining easier
here and in the caller which must also track the ShapeId generated here.
-}
box : (Physics.Body -> Physics.Body) -> ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) )
box orient =
    ( cubeMesh
    , Physics.body
        |> orient
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))
    )


{-| A constant sphere
-}
sphere : Mesh Attributes -> (Physics.Body -> Physics.Body) -> ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) )
sphere mesh orient =
    ( mesh
    , Physics.body
        |> orient
        |> Physics.setMass 5
        |> Physics.addShape (Physics.sphere (sqrt 3))
    )


{-| An immovable plane at a custom orientation.
-}
plane : (Physics.Body -> Physics.Body) -> ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) )
plane orient =
    ( planeMesh
    , Physics.body
        |> orient
        |> Physics.addShape Physics.plane
    )


shapeSelector : Int -> (Physics.Body -> Physics.Body) -> ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) )
shapeSelector pick =
    (case pick of
        0 ->
            box
        1 ->
            sphere sphereMesh
        2 ->
            sphere extendedTruncatedOctohedronMesh
        -- assuming 3
        _ ->
            sphere altSphereMesh
    )

{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedShape : Random.Generator ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) )
randomlyRotatedShape =
    Random.map4
        (\shapeOption jitter angle xyz  ->
            let direction = case xyz of
                x :: y :: z :: _ ->
                    vec3 x y z
                        |> Vec3.normalize
                -- not expected:
                _ ->
                   vec3 0 0 1
            in    
                (shapeSelector shapeOption)
                    -- Hack a small random value into the x offset for some
                    -- position jitter. This is important to have spheres that
                    -- interact realistically vs. perfectly balancing into stacks.
                    (Physics.offsetBy (vec3 jitter 0 10)
                        >> Physics.rotateBy direction angle
                    )
        )
        (Random.int 0 3)
        (Random.float -0.1 0.1)
        (Random.float (-pi / 2) (pi / 2))
        (Random.list 3 (Random.float -1 1))


init : ( Model, Cmd Msg )
init =
    let
        initialBodies =
            [ plane ( Physics.offsetBy (vec3 0 0 -1) )
            , box
                ( Physics.offsetBy (vec3 0 0 2)
                    >> Physics.rotateBy Vec3.j (-pi / 5)
                )
            , box
                ( Physics.offsetBy (vec3 -1.2 0 9)
                    >> Physics.rotateBy Vec3.j (-pi / 4)
                )
            , box
                ( Physics.offsetBy (vec3 1.3 0 6)
                    >> Physics.rotateBy Vec3.j (pi / 5)
                )
            ]

        initialWorld =
            Physics.world
                |> Physics.setGravity (vec3 0 0 -10)

        ( world, shapes ) =
            List.foldl
                addBody
                ( initialWorld, Dict.empty )
                initialBodies
    in
        ( { -- replaced by resize, including the initial resize
            screenWidth = 1
          , screenHeight = 1

          -- continuously updated by ticks and clicks
          , world = world

          -- updated by clicks
          , shapes = shapes
          }
        , Task.perform Resize Window.size
        )


addBody : ( Mesh Attributes, ( Physics.Body, Physics.ShapeId ) ) -> ( Physics.World, ShapeDict ) -> ( Physics.World, ShapeDict )
addBody ( mesh, ( body, shapeId ) ) ( world, shapes ) =
    let
        ( nextWorld, bodyId ) =
            Physics.addBody body world

        nextShapes =
            insertShape bodyId shapeId mesh shapes
    in
        ( nextWorld, nextShapes )


type alias ShapeDict =
    Dict ( Physics.BodyId, Physics.ShapeId ) (Mesh Attributes)


insertShape : Physics.BodyId -> Physics.ShapeId -> Mesh Attributes -> ShapeDict -> ShapeDict
insertShape bodyId shapeId mesh shapes =
    Dict.insert ( bodyId, shapeId ) mesh shapes


getShape : Physics.BodyId -> Physics.ShapeId -> ShapeDict -> Mesh Attributes
getShape bodyId shapeId shapes =
    Dict.get ( bodyId, shapeId ) shapes
        -- The default value of cubeMesh will never be used as all valid
        -- bodyid/shapeid pairs map to a mesh in the ShapeDict.
        -- An alternative to this unused default would be to use Debug.crash.
        -- Another alternative would be to RELY on the cubeMesh default,
        -- skipping the ShapeDict update for objects that use cubeMesh.
        -- That would improve space efficiency for a fraction of
        -- the objects in this example.
        |> Maybe.withDefault cubeMesh


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

        AddRandomShape ->
            ( model
            , (Random.generate
                AddShape
                    randomlyRotatedShape
              )
            )

        AddShape generatedShape ->
            ( let
                ( world, shapes ) =
                    addBody generatedShape ( model.world, model.shapes )
              in
                { model | world = world, shapes = shapes }
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
view { screenWidth, screenHeight, world, shapes } =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0 0 0 1
        ]
        [ width screenWidth
        , height screenHeight
        , style
            [ ( "display", "block" )
            , ( "width", toString screenWidth ++ "px" )
            , ( "height", toString screenHeight ++ "px" )
            ]
        , onClick AddRandomShape
        ]
        (let
            camera =
                Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k

            aspectRatio =
                (toFloat screenWidth) / (toFloat screenHeight)

            perspective =
                Mat4.makePerspective 24 aspectRatio 5 2000
         in
            Physics.foldl (addShape shapes camera perspective) [] world
                |> (if debugContacts then
                        addContacts camera perspective world
                    else
                        identity
                   )
        )


{-| Set to True to see collision points
-}
debugContacts : Bool
debugContacts =
    False


addShape : ShapeDict -> Mat4 -> Mat4 -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape shapes camera perspective { transform, bodyId, shapeId } tail =
    WebGL.entity
        vertex
        fragment
        (getShape bodyId shapeId shapes)
        { camera = camera
        , perspective = perspective
        , transform = transform
        }
        :: tail



-- Meshes:


type alias Attributes =
    { position : Vec3
    , color : Vec3
    }


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
        , face rfb rft lft lfb (vec3 0.5 0.5 0.5)
        , face lft rft rbt lbt (vec3 0.6 0.6 0.6)
        , face rfb lfb lbb rbb (vec3 0.7 0.7 0.7)
        , face lfb lft lbt lbb (vec3 0.8 0.8 0.8)
        , face rbt rbb lbb lbt (vec3 0.9 0.9 0.9)
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
face a b c d color =
    [ ( Attributes a color, Attributes b color, Attributes c color )
    , ( Attributes c color, Attributes d color, Attributes a color )
    ]


{-| A pseudo-spherical polyhedron with 26 vertices and 48 faces.
The vertices consist of the 8 vertices of a cube,
an additional 6 vertices extending out along each face normal,
and an additional 12 vertices bisecting each edge of the cube.
It has 72 edges and 48 triangular faces.
-}
sphereMesh : Mesh Attributes
sphereMesh =
    let
        radius =
            sqrt 3

        diag =
            radius / (sqrt 2)

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

        libj =
            vec3 (-diag) (-diag) 0

        ribj =
            vec3 diag (-diag) 0

        rifj =
            vec3 diag diag 0

        lifj =
            vec3 (-diag) diag 0

        bjbk =
            vec3 0 (-diag) (-diag)

        fjbk =
            vec3 0 diag (-diag)

        fjtk =
            vec3 0 diag diag

        bjtk =
            vec3 0 (-diag) diag

        bkli =
            vec3 (-diag) 0 (-diag)

        tkli =
            vec3 (-diag) 0 diag

        bkri =
            vec3 diag 0 (-diag)

        tkri =
            vec3 diag 0 diag

        li =
            Vec3.scale (-radius) Vec3.i

        ri =
            Vec3.scale radius Vec3.i

        bj =
            Vec3.scale (-radius) Vec3.j

        fj =
            Vec3.scale radius Vec3.j

        bk =
            Vec3.scale (-radius) Vec3.k

        tk =
            Vec3.scale radius Vec3.k

        yellow : Float -> Vec3
        yellow brightness =
            (vec3 brightness brightness 0.0)
    in
        [ facet rifj rft ri (yellow 0.2)
        , facet rifj ri rfb (yellow 0.4)
        , facet bkri rfb ri (yellow 0.8)
        , facet bkri ri rbb (yellow 0.6)
        , facet ribj rbb ri (yellow 0.21)
        , facet ribj ri rbt (yellow 0.41)
        , facet tkri rbt ri (yellow 0.81)
        , facet tkri ri rft (yellow 0.61)
        , facet rifj rfb fj (yellow 0.22)
        , facet rifj fj rft (yellow 0.42)
        , facet fjtk rft fj (yellow 0.82)
        , facet fjtk fj lft (yellow 0.62)
        , facet lifj lft fj (yellow 0.23)
        , facet lifj fj lfb (yellow 0.43)
        , facet fjbk lfb fj (yellow 0.83)
        , facet fjbk fj rfb (yellow 0.63)
        , facet fjtk lft tk (yellow 0.24)
        , facet fjtk tk rft (yellow 0.44)
        , facet tkri rft tk (yellow 0.84)
        , facet tkri tk rbt (yellow 0.64)
        , facet bjtk rbt tk (yellow 0.25)
        , facet bjtk tk lbt (yellow 0.45)
        , facet tkli lbt tk (yellow 0.85)
        , facet tkli tk lft (yellow 0.65)
        , facet fjbk rfb bk (yellow 0.26)
        , facet fjbk bk lfb (yellow 0.46)
        , facet bkli lfb bk (yellow 0.86)
        , facet bkli bk lbb (yellow 0.66)
        , facet bjbk lbb bk (yellow 0.27)
        , facet bjbk bk rbb (yellow 0.47)
        , facet bkri rbb bk (yellow 0.87)
        , facet bkri bk rfb (yellow 0.67)
        , facet lifj lfb li (yellow 0.28)
        , facet lifj li lft (yellow 0.48)
        , facet tkli lft li (yellow 0.88)
        , facet tkli li lbt (yellow 0.68)
        , facet libj lbt li (yellow 0.29)
        , facet libj li lbb (yellow 0.49)
        , facet bkli lbb li (yellow 0.89)
        , facet bkli li lfb (yellow 0.69)
        , facet ribj rbt bj (yellow 0.3)
        , facet ribj bj rbb (yellow 0.5)
        , facet bjbk rbb bj (yellow 0.9)
        , facet bjbk bj lbb (yellow 0.7)
        , facet libj lbb bj (yellow 0.31)
        , facet libj bj lbt (yellow 0.51)
        , facet bjtk lbt bj (yellow 0.91)
        , facet bjtk bj rbt (yellow 0.71)
        ]
            |> WebGL.triangles


{-| A pseudo-spherical polyhedron with 32 vertices and 54 faces.
The vertices consist of the 24 vertices of a truncated octohedron,
and an additional 8 vertices extending out along the normal for each
of its hexagonal face.
It has the original 6 square faces and an additional 48 triangular faces
arranged in shallow hexagonal pyramids around the 8 added vertices.
-}
extendedTruncatedOctohedronMesh : Mesh Attributes
extendedTruncatedOctohedronMesh =
    let
        radius =
            sqrt 3

        defaultR =
            sqrt 5

        scaleDown =
            radius / defaultR

        l2bz =
            (vec3 -2 -1 0)
                |> Vec3.scale scaleDown

        l2fz =
            (vec3 -2 1 0)
                |> Vec3.scale scaleDown

        l2zb =
            (vec3 -2 0 -1)
                |> Vec3.scale scaleDown

        l2zt =
            (vec3 -2 0 1)
                |> Vec3.scale scaleDown

        lb2z =
            (vec3 -1 -2 0)
                |> Vec3.scale scaleDown

        lf2z =
            (vec3 -1 2 0)
                |> Vec3.scale scaleDown

        lzb2 =
            (vec3 -1 0 -2)
                |> Vec3.scale scaleDown

        lzt2 =
            (vec3 -1 0 2)
                |> Vec3.scale scaleDown

        zb2b =
            (vec3 0 -2 -1)
                |> Vec3.scale scaleDown

        zb2t =
            (vec3 0 -2 1)
                |> Vec3.scale scaleDown

        zbb2 =
            (vec3 0 -1 -2)
                |> Vec3.scale scaleDown

        zbt2 =
            (vec3 0 -1 2)
                |> Vec3.scale scaleDown

        zfb2 =
            (vec3 0 1 -2)
                |> Vec3.scale scaleDown

        zft2 =
            (vec3 0 1 2)
                |> Vec3.scale scaleDown

        zf2b =
            (vec3 0 2 -1)
                |> Vec3.scale scaleDown

        zf2t =
            (vec3 0 2 1)
                |> Vec3.scale scaleDown

        rb2z =
            (vec3 1 -2 0)
                |> Vec3.scale scaleDown

        rzb2 =
            (vec3 1 0 -2)
                |> Vec3.scale scaleDown

        rzt2 =
            (vec3 1 0 2)
                |> Vec3.scale scaleDown

        rf2z =
            (vec3 1 2 0)
                |> Vec3.scale scaleDown

        r2bz =
            (vec3 2 -1 0)
                |> Vec3.scale scaleDown

        r2zb =
            (vec3 2 0 -1)
                |> Vec3.scale scaleDown

        r2zt =
            (vec3 2 0 1)
                |> Vec3.scale scaleDown

        r2fz =
            (vec3 2 1 0)
                |> Vec3.scale scaleDown

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

        red brightness =
            (vec3 brightness 0 0)
    in
        [ face l2bz l2zb l2fz l2zt (red 0.3)
        , face r2zb r2bz r2zt r2fz (red 0.3)
        , face zb2b lb2z zb2t rb2z (red 0.3)
        , face zf2t lf2z zf2b rf2z (red 0.3)
        , face lzt2 zft2 rzt2 zbt2 (red 0.3)
        , face rzb2 zfb2 lzb2 zbb2 (red 0.3)
        , hexcone lbt l2bz l2zt lzt2 zbt2 zb2t lb2z (red 0.9) (red 0.8)
        , hexcone lbb l2zb l2bz lb2z zb2b zbb2 lzb2 (red 0.4) (red 0.3)
        , hexcone lfb l2fz l2zb lzb2 zfb2 zf2b lf2z (red 0.85) (red 0.75)
        , hexcone lft l2zt l2fz lf2z zf2t zft2 lzt2 (red 0.45) (red 0.35)
        , hexcone rfb r2zb r2fz rf2z zf2b zfb2 rzb2 (red 0.7) (red 0.6)
        , hexcone rbb r2bz r2zb rzb2 zbb2 zb2b rb2z (red 0.5) (red 0.4)
        , hexcone rbt r2zt r2bz rb2z zb2t zbt2 rzt2 (red 0.65) (red 0.55)
        , hexcone rft r2fz r2zt rzt2 zft2 zf2t rf2z (red 0.55) (red 0.45)
        ]
            |> List.concat
            |> WebGL.triangles


hexcone : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
hexcone peak a b c d e f color1 color2 =
    [ ( Attributes peak color1, Attributes a color1, Attributes b color1 )
    , ( Attributes peak color2, Attributes b color2, Attributes c color2 )
    , ( Attributes peak color1, Attributes c color1, Attributes d color1 )
    , ( Attributes peak color2, Attributes d color2, Attributes e color2 )
    , ( Attributes peak color1, Attributes e color1, Attributes f color1 )
    , ( Attributes peak color2, Attributes f color2, Attributes a color2 )
    ]


{-| An alternative pseudo-spherical polyhedron with 30 vertices,
84 edges and 56 triangular faces.
The vertices consist of the 24 vertices of a truncated cube
having 6 regular octogonal faces connected at the cube's corners
by 8 regular triangles.
An additional 6 vertices extending out along each face normal
replace the octogonal faces with a shallow octoganal pyramid.
-}
altSphereMesh : Mesh Attributes
altSphereMesh =
    let
        radius =
            sqrt 3

        li =
            Vec3.scale (-radius) Vec3.i

        ri =
            Vec3.scale radius Vec3.i

        bj =
            Vec3.scale (-radius) Vec3.j

        fj =
            Vec3.scale radius Vec3.j

        bk =
            Vec3.scale (-radius) Vec3.k

        tk =
            Vec3.scale radius Vec3.k

        tangent =
            tan (pi / 8)

        halfExtent =
            radius / (sqrt (tangent * tangent + 2))

        halfEdge =
            tangent * halfExtent

        rfti =
            vec3 halfEdge halfExtent halfExtent

        rftj =
            vec3 halfExtent halfEdge halfExtent

        rftk =
            vec3 halfExtent halfExtent halfEdge

        lfti =
            vec3 -halfEdge halfExtent halfExtent

        lftj =
            vec3 -halfExtent halfEdge halfExtent

        lftk =
            vec3 -halfExtent halfExtent halfEdge

        lbti =
            vec3 -halfEdge -halfExtent halfExtent

        lbtj =
            vec3 -halfExtent -halfEdge halfExtent

        lbtk =
            vec3 -halfExtent -halfExtent halfEdge

        rbti =
            vec3 halfEdge -halfExtent halfExtent

        rbtj =
            vec3 halfExtent -halfEdge halfExtent

        rbtk =
            vec3 halfExtent -halfExtent halfEdge

        rbbi =
            vec3 halfEdge -halfExtent -halfExtent

        rbbj =
            vec3 halfExtent -halfEdge -halfExtent

        rbbk =
            vec3 halfExtent -halfExtent -halfEdge

        rfbi =
            vec3 halfEdge halfExtent -halfExtent

        rfbj =
            vec3 halfExtent halfEdge -halfExtent

        rfbk =
            vec3 halfExtent halfExtent -halfEdge

        lfbi =
            vec3 -halfEdge halfExtent -halfExtent

        lfbj =
            vec3 -halfExtent halfEdge -halfExtent

        lfbk =
            vec3 -halfExtent halfExtent -halfEdge

        lbbi =
            vec3 -halfEdge -halfExtent -halfExtent

        lbbj =
            vec3 -halfExtent -halfEdge -halfExtent

        lbbk =
            vec3 -halfExtent -halfExtent -halfEdge

        grue : Float -> Vec3
        grue brightness =
            (vec3 0.0 brightness brightness)
    in
        [ [ facet rftk rftj rfti (grue 0.3)
          , facet lfti lftj lftk (grue 0.3)
          , facet lbtk lbtj lbti (grue 0.3)
          , facet rbti rbtj rbtk (grue 0.3)
          , facet rbbk rbbj rbbi (grue 0.3)
          , facet rfbi rfbj rfbk (grue 0.3)
          , facet lfbk lfbj lfbi (grue 0.3)
          , facet lbbi lbbj lbbk (grue 0.3)
          ]
        , octocone tk rfti rftj rbtj rbti lbti lbtj lftj lfti (grue 0.4) (grue 0.5)
        , octocone bk rfbi lfbi lfbj lbbj lbbi rbbi rbbj rfbj (grue 0.433) (grue 0.533)
        , octocone fj rfti lfti lftk lfbk lfbi rfbi rfbk rftk (grue 0.467) (grue 0.567)
        , octocone bj rbti rbtk rbbk rbbi lbbi lbbk lbtk lbti (grue 0.5) (grue 0.6)
        , octocone ri rftk rfbk rfbj rbbj rbbk rbtk rbtj rftj (grue 0.533) (grue 0.633)
        , octocone li lftk lftj lbtj lbtk lbbk lbbj lfbj lfbk (grue 0.567) (grue 0.667)
        ]
            |> List.concat
            |> WebGL.triangles


octocone : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
octocone peak a b c d e f g h color1 color2 =
    [ ( Attributes peak color1, Attributes a color1, Attributes b color1 )
    , ( Attributes peak color2, Attributes b color2, Attributes c color2 )
    , ( Attributes peak color1, Attributes c color1, Attributes d color1 )
    , ( Attributes peak color2, Attributes d color2, Attributes e color2 )
    , ( Attributes peak color1, Attributes e color1, Attributes f color1 )
    , ( Attributes peak color2, Attributes f color2, Attributes g color2 )
    , ( Attributes peak color1, Attributes g color1, Attributes h color1 )
    , ( Attributes peak color2, Attributes h color2, Attributes a color2 )
    ]


facet : Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( Attributes, Attributes, Attributes )
facet a b c color =
    ( Attributes a color, Attributes b color, Attributes c color )



-- Shaders:


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    }


type alias Varying =
    { vcolor : Vec3 }


vertex : Shader Attributes Uniforms Varying
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


fragment : Shader {} Uniforms Varying
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
addContacts : Mat4 -> Mat4 -> Physics.World -> List Entity -> List Entity
addContacts camera perspective world entities =
    let
        addContact : Vec3 -> List Entity -> List Entity
        addContact contactPoint tail =
            WebGL.entity
                vertex
                fragment
                cubeMesh
                { camera = camera
                , perspective = perspective
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
