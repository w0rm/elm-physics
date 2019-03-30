module Common.Meshes exposing
    ( Attributes
    , box
    , moveBy
    , pyramid
    , sphere
    , toMesh
    , toWireframe
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


toMesh : List ( Attributes, Attributes, Attributes ) -> Mesh Attributes
toMesh =
    WebGL.triangles


toWireframe : List ( Attributes, Attributes, Attributes ) -> Mesh Attributes
toWireframe =
    trianglesToLines >> WebGL.lines


moveBy : { x : Float, y : Float, z : Float } -> List ( Attributes, Attributes, Attributes ) -> List ( Attributes, Attributes, Attributes )
moveBy offset triangles =
    List.map
        (\( p1, p2, p3 ) ->
            ( { p1 | position = Vec3.add (Vec3.fromRecord offset) p1.position }
            , { p2 | position = Vec3.add (Vec3.fromRecord offset) p2.position }
            , { p3 | position = Vec3.add (Vec3.fromRecord offset) p3.position }
            )
        )
        triangles


trianglesToLines : List ( Attributes, Attributes, Attributes ) -> List ( Attributes, Attributes )
trianglesToLines triangles =
    List.foldl
        (\( p1, p2, p3 ) result -> ( p1, p2 ) :: ( p2, p3 ) :: ( p3, p1 ) :: result)
        []
        triangles


box : { x : Float, y : Float, z : Float } -> List ( Attributes, Attributes, Attributes )
box dimensions =
    let
        x =
            dimensions.x * 0.5

        y =
            dimensions.y * 0.5

        z =
            dimensions.z * 0.5

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


pyramid : Float -> Float -> List ( Attributes, Attributes, Attributes )
pyramid halfbase baserise =
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


sphere : Int -> Float -> List ( Attributes, Attributes, Attributes )
sphere iterations radius =
    divideSphere iterations radius (octahedron radius)
        |> List.map (\( p1, p2, p3 ) -> facet p1 p2 p3)


{-| Recursively divide an octahedron to turn it into a sphere
-}
divideSphere : Int -> Float -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divideSphere step radius triangles =
    if step == 0 then
        triangles

    else
        triangles
            |> List.foldl (divide radius) []
            |> divideSphere (step - 1) radius


{-|

        1
       / \
    b /___\ c
     /\   /\
    /__\ /__\
    0   a    2

-}
divide : Float -> ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divide radius ( v0, v1, v2 ) result =
    let
        a =
            Vec3.add v0 v2 |> Vec3.normalize |> Vec3.scale radius

        b =
            Vec3.add v0 v1 |> Vec3.normalize |> Vec3.scale radius

        c =
            Vec3.add v1 v2 |> Vec3.normalize |> Vec3.scale radius
    in
    ( v0, b, a ) :: ( b, v1, c ) :: ( a, b, c ) :: ( a, c, v2 ) :: result


{-| Octahedron
-}
octahedron : Float -> List ( Vec3, Vec3, Vec3 )
octahedron radius =
    [ ( vec3 radius 0 0, vec3 0 radius 0, vec3 0 0 radius )
    , ( vec3 0 radius 0, vec3 -radius 0 0, vec3 0 0 radius )
    , ( vec3 -radius 0 0, vec3 0 -radius 0, vec3 0 0 radius )
    , ( vec3 0 -radius 0, vec3 radius 0 0, vec3 0 0 radius )
    , ( vec3 radius 0 0, vec3 0 0 -radius, vec3 0 radius 0 )
    , ( vec3 0 radius 0, vec3 0 0 -radius, vec3 -radius 0 0 )
    , ( vec3 -radius 0 0, vec3 0 0 -radius, vec3 0 -radius 0 )
    , ( vec3 0 -radius 0, vec3 0 0 -radius, vec3 radius 0 0 )
    ]


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
