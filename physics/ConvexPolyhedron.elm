module Physics.ConvexPolyhedron exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias ConvexPolyhedron =
    { faces : List (List Int)
    , vertices : List Vec3
    , normals : List Vec3
    }


fromBox : Vec3 -> ConvexPolyhedron
fromBox halfExtents =
    let
        { x, y, z } =
            Vec3.toRecord halfExtents

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
        { faces =
            [ [ 3, 2, 1, 0 ]
            , [ 4, 5, 6, 7 ]
            , [ 5, 4, 0, 1 ]
            , [ 2, 3, 7, 6 ]
            , [ 0, 4, 7, 3 ]
            , [ 1, 2, 6, 5 ]
            ]
        , vertices =
            [ v0, v1, v2, v3, v4, v5, v6, v7 ]
        , normals =
            [ normal v3 v2 v1
            , normal v4 v5 v6
            , normal v5 v4 v0
            , normal v2 v3 v7
            , normal v0 v4 v7
            , normal v1 v2 v6
            ]
        }


normal : Vec3 -> Vec3 -> Vec3 -> Vec3
normal v0 v1 v2 =
    Vec3.cross (Vec3.sub v2 v1) (Vec3.sub v1 v0)
        |> Vec3.normalize
