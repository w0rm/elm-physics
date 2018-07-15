module Common.Meshes exposing (Attributes, makeBox, makePyramid)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


makeBox : Vec3 -> Mesh Attributes
makeBox halfExtends =
    let
        { x, y, z } =
            Vec3.toRecord halfExtends

        v0 =
            vec3 -x -y -z

        v1 =
            vec3 x -y -z

        v2 =
            vec3 x y -z

        v3 =
            vec3 -x y -z

        v4 =
            vec3 -x -y z

        v5 =
            vec3 x -y z

        v6 =
            vec3 x y z

        v7 =
            vec3 -x y z
    in
        [ facet v3 v2 v1
        , facet v1 v0 v3
        , facet v4 v5 v6
        , facet v6 v7 v4
        , facet v5 v4 v0
        , facet v0 v1 v5
        , facet v2 v3 v7
        , facet v7 v6 v2
        , facet v0 v4 v7
        , facet v7 v3 v0
        , facet v1 v2 v6
        , facet v6 v5 v1
        ]
            |> WebGL.triangles


makePyramid : Float -> Float -> Mesh Attributes
makePyramid halfbase baserise =
    let
        top =
            vec3 0 0 1

        rbb =
            vec3 halfbase -halfbase baserise

        rfb =
            vec3 halfbase halfbase baserise

        lfb =
            vec3 -halfbase halfbase baserise

        lbb =
            vec3 -halfbase -halfbase baserise
    in
        [ facet rfb lfb lbb
        , facet lbb rbb rfb
        , facet top rfb rbb
        , facet top lfb rfb
        , facet top lbb lfb
        , facet top rbb lbb
        ]
            |> WebGL.triangles


facet : Vec3 -> Vec3 -> Vec3 -> ( Attributes, Attributes, Attributes )
facet a b c =
    let
        normal =
            Vec3.cross (Vec3.sub b a) (Vec3.sub b c)
    in
        ( Attributes a normal
        , Attributes b normal
        , Attributes c normal
        )
