module Common.Shaders exposing
    ( Uniforms
    , fragment
    , shadowFragment
    , vertex
    , wireframeFragment
    , wireframeVertex
    )

{-| This file contains shaders that are used in examples.
Shaders support simple flat lighting.
-}

import Common.Meshes exposing (Attributes)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
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
        attribute vec2 barycentric;
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


wireframeVertex : Shader Attributes Uniforms { vbarycentric : Vec2 }
wireframeVertex =
    [glsl|
        attribute vec3 position;
        attribute vec2 barycentric;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec2 vbarycentric;

        void main () {
          vec4 worldPosition = transform * vec4(position, 1.0);
          vbarycentric = barycentric;
          gl_Position = perspective * camera * worldPosition;
        }
    |]


wireframeFragment : Shader {} Uniforms { vbarycentric : Vec2 }
wireframeFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying vec2 vbarycentric;

        void main () {
            float width = 0.5;
            vec3 barycentric = vec3(vbarycentric, 1.0 - vbarycentric.x - vbarycentric.y);
            vec3 d = fwidth(barycentric);
            vec3 step = smoothstep(d * (width - 0.5), d * (width + 0.5), barycentric);
            float alpha = 1.0 - min(min(step.x, step.y), step.z);
            if (alpha < 0.01) {
                discard;
            }
            gl_FragColor = vec4(color * alpha, alpha);
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
