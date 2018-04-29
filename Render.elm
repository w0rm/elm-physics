module Render exposing (world)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Physics.Quaternion as Quaternion
import Physics.NarrowPhase as NarrowPhase
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.World as Physics exposing (World)
import Physics.Body as Physics exposing (Body, BodyId)
import Physics.Shape as Physics exposing (Shape(..))
import Dict exposing (Dict)
import WebGL exposing (Mesh, Shader, Entity)
import Types exposing (..)


type alias RenderShape =
    { transform : Mat4
    , mesh : Mesh Attributes
    }


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    }


type alias Attributes =
    { position : Vec3
    , color : Vec3
    }


world : Model -> List Entity
world { screenWidth, screenHeight, world } =
    let
        perspective =
            Mat4.makePerspective 24 (screenWidth / screenHeight) 5 2000

        contacts =
            NarrowPhase.getContacts world

        entities =
            fold (addShape camera perspective) [] world
    in
        List.foldl (addContact camera perspective world.bodies) entities contacts


addContact : Mat4 -> Mat4 -> Dict BodyId Body -> ContactEquation -> List Entity -> List Entity
addContact camera perspective bodies { bodyId1, bodyId2, ri, rj } =
    let
        b1 =
            Dict.get bodyId1 bodies
                |> Maybe.map .position
                |> Maybe.withDefault (vec3 0 0 0)

        b2 =
            Dict.get bodyId2 bodies
                |> Maybe.map .position
                |> Maybe.withDefault (vec3 0 0 0)
    in
        (++)
            [ WebGL.entity
                vertex
                fragment
                cube
                { transform =
                    Mat4.mul
                        (Mat4.makeTranslate (Vec3.add ri b1))
                        (Mat4.makeScale (vec3 0.1 0.1 0.1))
                , perspective = perspective
                , camera = camera
                }
            , WebGL.entity
                vertex
                fragment
                cube
                { transform =
                    Mat4.mul
                        (Mat4.makeTranslate (Vec3.add rj b2))
                        (Mat4.makeScale (vec3 0.1 0.1 0.1))
                , perspective = perspective
                , camera = camera
                }
            ]


addShape : Mat4 -> Mat4 -> RenderShape -> List Entity -> List Entity
addShape camera perspective { transform, mesh } =
    (::)
        (WebGL.entity
            vertex
            fragment
            mesh
            { transform = transform
            , perspective = perspective
            , camera = camera
            }
        )


origin : Vec3
origin =
    Vec3.vec3 0 30 20


destination : Vec3
destination =
    Vec3.vec3 0 0 0


camera : Mat4
camera =
    Mat4.makeLookAt origin destination Vec3.k


fold : (RenderShape -> a -> a) -> a -> World -> a
fold fn acc { bodies } =
    Dict.foldl
        (\bodyId { position, quaternion, shapes, shapeOffsets, shapeOrientations } acc1 ->
            Dict.foldl
                (\shapeId shape acc2 ->
                    fn
                        { transform =
                            Physics.transform shape
                                |> Mat4.mul (Quaternion.toMat4 (Maybe.withDefault Quaternion.identity (Dict.get shapeId shapeOrientations)))
                                |> Mat4.mul (Mat4.makeTranslate (Maybe.withDefault Physics.zero3 (Dict.get shapeId shapeOffsets)))
                                |> Mat4.mul (Quaternion.toMat4 quaternion)
                                |> Mat4.mul (Mat4.makeTranslate position)
                        , mesh =
                            case shape of
                                Box _ ->
                                    cube

                                Plane ->
                                    plane
                        }
                        acc2
                )
                acc1
                shapes
        )
        acc
        bodies


plane : Mesh Attributes
plane =
    let
        rf =
            vec3 10 10 0

        lf =
            vec3 -10 10 0

        lb =
            vec3 -10 -10 0

        rb =
            vec3 10 -10 0
    in
        -- WebGL.triangles (face rf lf lb rb (vec3 0.2 0.2 0.2))
        WebGL.triangles []


cube : Mesh Attributes
cube =
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
