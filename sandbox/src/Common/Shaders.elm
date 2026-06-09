module Common.Shaders exposing
    ( Uniforms
    , ambientFragment
    , colorFragment
    , diffuseFragment
    , fragment
    , shadowVolumeFragment
    , shadowVolumeVertex
    , vertex
    , wireframeFragment
    , wireframeVertex
    )

{-| This file contains shaders that are used in examples.
Shaders support simple flat lighting and stencil shadow volumes.
-}

import Common.Meshes exposing (Attributes, ShadowVertex)
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
        attribute vec3 barycentric;
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
          vec3 normal = normalize(cross(dFdx(vposition), dFdy(vposition)));
          float directional = max(dot(normal, lightDirection), 0.0);
          float vlighting = ambientLight + directional * directionalLight;
          gl_FragColor = vec4(vlighting * color, 1.0);
        }
    |]


wireframeVertex : Shader Attributes Uniforms { vbarycentric : Vec3 }
wireframeVertex =
    [glsl|
        attribute vec3 position;
        attribute vec3 barycentric;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec3 vbarycentric;

        void main () {
          vec4 worldPosition = transform * vec4(position, 1.0);
          vbarycentric = barycentric;
          gl_Position = perspective * camera * worldPosition;
        }
    |]


wireframeFragment : Shader {} Uniforms { vbarycentric : Vec3 }
wireframeFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying vec3 vbarycentric;

        void main () {
            float width = 0.5;
            vec3 d = fwidth(vbarycentric);
            vec3 step = smoothstep(d * (width - 0.5), d * (width + 0.5), vbarycentric);
            float alpha = 1.0 - min(min(step.x, step.y), step.z);
            if (alpha < 0.01) {
                discard;
            }
            gl_FragColor = vec4(color * alpha, alpha);
        }
    |]


{-| Base color every surface gets, lit or shadowed. The ambient wraps around
the key-light direction, with a gain on the dot so faces spread out even under
a near-overhead light (where raw orientation differences are tiny). This gives
shadowed faces good contrast with each other while the modest base/amount keep
the shadow-to-lit gap in check. Monotonic: a face turned away can never
out-shine one turned towards the light.
-}
ambientFragment : Shader {} Uniforms { vposition : Vec3 }
ambientFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        uniform vec3 lightDirection;
        varying vec3 vposition;
        void main () {
          vec3 normal = normalize(cross(dFdx(vposition), dFdy(vposition)));
          float wrap = clamp(0.5 + dot(normal, lightDirection), 0.0, 1.0);
          float ambientLight = 0.28 + 0.22 * wrap;
          gl_FragColor = vec4(ambientLight * color, 1.0);
        }
    |]


{-| Directional term, blended additively onto the ambient pass only where
the stencil says the fragment is not in shadow.
-}
diffuseFragment : Shader {} Uniforms { vposition : Vec3 }
diffuseFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        uniform vec3 lightDirection;
        varying vec3 vposition;
        void main () {
          float directionalLight = 0.4;
          vec3 normal = normalize(cross(dFdx(vposition), dFdy(vposition)));
          float directional = max(dot(normal, lightDirection), 0.0);
          gl_FragColor = vec4(directionalLight * directional * color, 1.0);
        }
    |]


{-| Extrudes the silhouette of a shadow caster away from the light to build
its shadow volume. `lightDirection` points towards the light, so faces with
`dot(normal, lightDirection) <= 0` face away and get pushed to "infinity".
-}
shadowVolumeVertex : Shader ShadowVertex Uniforms {}
shadowVolumeVertex =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        uniform vec3 lightDirection;
        void main () {
          vec3 worldPosition = (transform * vec4(position, 1.0)).xyz;
          vec3 worldNormal = (transform * vec4(normal, 0.0)).xyz;
          vec3 offset = vec3(0.0, 0.0, 0.0);
          if (dot(lightDirection, worldNormal) <= 0.0) {
            offset = -100.0 * lightDirection;
          }
          gl_Position = perspective * camera * vec4(worldPosition + offset, 1.0);
        }
    |]


{-| The shadow volume writes to the stencil buffer only, so its color is
never used.
-}
shadowVolumeFragment : Shader {} Uniforms {}
shadowVolumeFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        void main () {
          gl_FragColor = vec4(color, 1.0);
        }
    |]


colorFragment : Shader {} Uniforms { vposition : Vec3 }
colorFragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying vec3 vposition;
        void main () {
          gl_FragColor = vec4(color, 1.0);
        }
    |]
