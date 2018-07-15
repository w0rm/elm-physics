module Common.Shaders exposing (Uniforms, Varying, vertex, fragment, shadowFragment)

import Common.Meshes exposing (Attributes)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL exposing (Shader)


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
