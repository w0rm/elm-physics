module Render exposing (world)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Physics.Quaternion as Quaternion
import Physics.World as Physics exposing (World)
import Physics.Body as Physics
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
    { position : Vec3 }


world : Model -> List Entity
world { screenWidth, screenHeight, world } =
    let
        perspective =
            Mat4.makePerspective 24 (screenWidth / screenHeight) 5 2000
    in
        fold (shape camera perspective) [] world


shape : Mat4 -> Mat4 -> RenderShape -> List Entity -> List Entity
shape camera perspective { transform, mesh } =
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
            vec3 1 1 0

        lf =
            vec3 -1 1 0

        lb =
            vec3 -1 -1 0

        rb =
            vec3 1 -1 0
    in
        -- WebGL.triangles (face rf lf lb rb)
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
        [ face rft rfb rbb rbt
        , face rft rfb lfb lft
        , face rft lft lbt rbt
        , face rfb lfb lbb rbb
        , face lft lfb lbb lbt
        , face rbt rbb lbb lbt
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
face a b c d =
    [ ( Attributes a, Attributes b, Attributes c )
    , ( Attributes c, Attributes d, Attributes a )
    ]


vertex : Shader Attributes Uniforms {}
vertex =
    [glsl|
        attribute vec3 position;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        void main () {
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
        }
    |]


fragment : Shader {} Uniforms {}
fragment =
    [glsl|
        precision mediump float;
        void main () {
          gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
        }
    |]
