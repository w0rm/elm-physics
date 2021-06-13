module Common.Shaders exposing
    ( Uniforms
    , fragment
    , shadowFragment
    , vertex
    , wireframeFragment
    )

{-| This file contains shaders that are used in examples.
Shaders support simple flat lighting.
-}

import Common.Meshes exposing (Attributes)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    , color : Vec3
    , lightDirection : Vec3
    }


vertex : Shader Attributes Uniforms { vposition : Vec3 }
vertex =
    [glsl|
        attribute vec3 position;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec3 vposition;
        void main () {
          vec4 worldPosition = transform * vec4(position, 1.0);
          vposition = worldPosition.xyz;
          gl_Position = perspective * camera * worldPosition;
        }
    |]


fragment : Shader {} Uniforms { vposition : Vec3 }
fragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        uniform vec3 lightDirection;
        varying vec3 vposition;
        void main () {
          float ambientLight = 0.4;
          float directionalLight = 0.6;
          vec3 normal = -normalize(cross(dFdx(vposition), dFdy(vposition)));
          float directional = max(dot(normal, lightDirection), 0.0);
          float vlighting = ambientLight + directional * directionalLight;
          gl_FragColor = vec4(vlighting * color, 1.0);
        }
    |]


wireframeFragment : Shader {} Uniforms { vposition : Vec3 }
wireframeFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying vec3 vposition;
        void main () {
          gl_FragColor = vec4(color, 1.0);
        }
    |]


shadowFragment : Shader {} Uniforms { vposition : Vec3 }
shadowFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying vec3 vposition;
        void main () {
          gl_FragColor = vec4(color, 1);
        }
    |]
