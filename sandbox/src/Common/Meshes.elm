module Common.Meshes exposing
    ( Attributes
    , block
    , contact
    , cylinder
    , edge
    , fromTriangles
    , normal
    , sphere
    , triangularMesh
    )

import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Frame3d
import Length exposing (Meters, inMeters)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Sphere3d exposing (Sphere3d)
import TriangularMesh exposing (TriangularMesh)
import WebGL exposing (Mesh)


type alias Attributes =
    { position : Vec3
    , barycentric : Vec3
    }



-- Meshes


normal : Mesh Attributes
normal =
    fromTriangles (pyramid 0.05 0.05)


edge : Mesh Attributes
edge =
    fromTriangles (pyramid 0.1 0.5)


contact : Mesh Attributes
contact =
    fromTriangles (sphere 2 (Sphere3d.atOrigin (Length.meters 0.07)))


fromTriangles : List ( Attributes, Attributes, Attributes ) -> Mesh Attributes
fromTriangles =
    WebGL.triangles


trianglesToLines : List ( Attributes, Attributes, Attributes ) -> List ( Attributes, Attributes )
trianglesToLines triangles =
    List.foldl
        (\( p1, p2, p3 ) result -> ( p1, p2 ) :: ( p2, p3 ) :: ( p3, p1 ) :: result)
        []
        triangles


block : Block3d Meters BodyCoordinates -> List ( Attributes, Attributes, Attributes )
block block3d =
    let
        ( sizeX, sizeY, sizeZ ) =
            Block3d.dimensions block3d

        blockFrame3d =
            Block3d.axes block3d

        x =
            inMeters sizeX * 0.5

        y =
            inMeters sizeY * 0.5

        z =
            inMeters sizeZ * 0.5

        transform px py pz =
            Point3d.placeIn blockFrame3d (Point3d.meters px py pz)
                |> Point3d.toMeters
                |> Vec3.fromRecord

        v0 =
            transform -x -y -z

        v1 =
            transform x -y -z

        v2 =
            transform x y -z

        v3 =
            transform -x y -z

        v4 =
            transform -x -y z

        v5 =
            transform x -y z

        v6 =
            transform x y z

        v7 =
            transform -x y z
    in
    [ facet v2 v1 v3 1
    , facet v0 v3 v1 1
    , facet v5 v6 v4 1
    , facet v7 v4 v6 1
    , facet v4 v0 v5 1
    , facet v1 v5 v0 1
    , facet v3 v7 v2 1
    , facet v6 v2 v7 1
    , facet v4 v7 v0 1
    , facet v3 v0 v7 1
    , facet v2 v6 v1 1
    , facet v5 v1 v6 1
    ]


triangularMesh : TriangularMesh (Point3d Meters BodyCoordinates) -> List ( Attributes, Attributes, Attributes )
triangularMesh mesh =
    mesh
        |> TriangularMesh.mapVertices Point3d.toMeters
        |> TriangularMesh.faceVertices
        |> List.map
            (\( v1, v2, v3 ) ->
                facet (Vec3.fromRecord v1)
                    (Vec3.fromRecord v2)
                    (Vec3.fromRecord v3)
                    0
            )


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
    [ facet rfb lfb lbb 0
    , facet lbb rbb rfb 0
    , facet top rfb rbb 0
    , facet top lfb rfb 0
    , facet top lbb lfb 0
    , facet top rbb lbb 0
    ]


{-| Code taken from elm-3d-scene's Primitives module.
-}
cylinder : Int -> Cylinder3d Meters BodyCoordinates -> List ( Attributes, Attributes, Attributes )
cylinder subdivisions cylinder3d =
    let
        wedgeAngle =
            2 * pi / toFloat subdivisions

        length =
            Length.inMeters (Cylinder3d.length cylinder3d)

        radius =
            Length.inMeters (Cylinder3d.radius cylinder3d)

        bottomZ =
            -0.5 * length

        topZ =
            0.5 * length

        ( a, b ) =
            Cylinder3d.axialDirection cylinder3d
                |> Direction3d.perpendicularBasis

        cylinderFrame3d =
            Frame3d.unsafe
                { originPoint = Cylinder3d.centerPoint cylinder3d
                , xDirection = a
                , yDirection = b
                , zDirection = Cylinder3d.axialDirection cylinder3d
                }

        transform px py pz =
            Point3d.placeIn cylinderFrame3d (Point3d.meters px py pz)
                |> Point3d.toMeters
                |> Vec3.fromRecord

        bottomCenter =
            transform 0 0 bottomZ

        topCenter =
            transform 0 0 topZ

        wedge startIndex =
            let
                startAngle =
                    wedgeAngle * toFloat startIndex

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle * toFloat endIndex

                startX =
                    radius * cos startAngle

                endX =
                    radius * cos endAngle

                startY =
                    radius * sin startAngle

                endY =
                    radius * sin endAngle

                p0 =
                    transform startX startY bottomZ

                p1 =
                    transform endX endY bottomZ

                p2 =
                    transform startX startY topZ

                p3 =
                    transform endX endY topZ
            in
            [ facet topCenter p2 p3 2
            , facet p1 p3 p0 1
            , facet p2 p0 p3 1
            , facet bottomCenter p1 p0 2
            ]
    in
    List.range 0 (subdivisions - 1)
        |> List.concatMap wedge


sphere : Int -> Sphere3d Meters BodyCoordinates -> List ( Attributes, Attributes, Attributes )
sphere iterations sphere3d =
    let
        position p =
            Point3d.toMeters (Sphere3d.centerPoint sphere3d)
                |> Vec3.fromRecord
                |> Vec3.add p

        radius =
            Length.inMeters (Sphere3d.radius sphere3d)
    in
    divideSphere iterations radius (octahedron radius)
        |> List.map
            (\( p1, p2, p3 ) ->
                facet (position p1) (position p2) (position p3) 0
            )


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


facet : Vec3 -> Vec3 -> Vec3 -> Int -> ( Attributes, Attributes, Attributes )
facet a b c remove =
    case remove of
        1 ->
            -- b c is removed
            ( Attributes a (vec3 0 0 1)
            , Attributes b (vec3 0 1 0)
            , Attributes c (vec3 1 0 1)
            )

        2 ->
            -- only b c is kept
            ( Attributes a (vec3 1 1 1)
            , Attributes b (vec3 1 0 0)
            , Attributes c (vec3 1 0 0)
            )

        _ ->
            -- all edges are kept
            ( Attributes a (vec3 0 0 1)
            , Attributes b (vec3 0 1 0)
            , Attributes c (vec3 1 0 0)
            )
