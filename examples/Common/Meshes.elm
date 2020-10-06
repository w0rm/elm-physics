module Common.Meshes exposing
    ( Attributes
    , Meshes
    , block
    , contact
    , cylinder
    , edge
    , fromTriangles
    , normal
    , sphere
    , triangularMesh
    )

import Angle
import Block3d exposing (Block3d)
import Direction2d
import Direction3d
import Length exposing (Length, Meters, inMeters)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Quantity exposing (zero)
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d
import WebGL exposing (Mesh)


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }



-- Meshes


normal : Mesh Attributes
normal =
    toMesh (pyramid 0.05 0.05)


edge : Mesh Attributes
edge =
    toMesh (pyramid 0.1 0.5)


contact : Mesh Attributes
contact =
    toMesh (sphere 2 (Sphere3d.atOrigin (Length.meters 0.07)))


toMesh : List ( Attributes, Attributes, Attributes ) -> Mesh Attributes
toMesh =
    WebGL.triangles


toWireframe : List ( Attributes, Attributes, Attributes ) -> Mesh Attributes
toWireframe =
    trianglesToLines >> WebGL.lines


type alias Meshes =
    { mesh : Mesh Attributes
    , wireframe : Mesh Attributes
    }


fromTriangles : List ( Attributes, Attributes, Attributes ) -> Meshes
fromTriangles triangles =
    { mesh = toMesh triangles
    , wireframe = toWireframe triangles
    }


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
    [ facet rfb lfb lbb
    , facet lbb rbb rfb
    , facet top rfb rbb
    , facet top lfb rfb
    , facet top lbb lfb
    , facet top rbb lbb
    ]


{-| Code taken from elm-3d-scene's Primitives module.
-}
cylinder : Int -> Length -> Length -> List ( Attributes, Attributes, Attributes )
cylinder subdivisions radius height =
    let
        wedgeAngle =
            Angle.turns 1 |> Quantity.divideBy (toFloat subdivisions)

        negativeZVector =
            Direction3d.negativeZ |> Direction3d.toVector

        positiveZVector =
            Direction3d.positiveZ |> Direction3d.toVector

        bottomZ =
            Quantity.multiplyBy -0.5 height

        topZ =
            Quantity.multiplyBy 0.5 height

        bottomCenter =
            Point3d.xyz zero zero bottomZ

        topCenter =
            Point3d.xyz zero zero topZ

        wedge startIndex =
            let
                startAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat startIndex)

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat endIndex)

                startX =
                    radius |> Quantity.multiplyBy (Angle.cos startAngle)

                endX =
                    radius |> Quantity.multiplyBy (Angle.cos endAngle)

                startY =
                    radius |> Quantity.multiplyBy (Angle.sin startAngle)

                endY =
                    radius |> Quantity.multiplyBy (Angle.sin endAngle)

                p0 =
                    Point3d.xyz startX startY bottomZ

                p1 =
                    Point3d.xyz endX endY bottomZ

                p2 =
                    Point3d.xyz startX startY topZ

                p3 =
                    Point3d.xyz endX endY topZ

                startNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle startAngle)
                        |> Direction3d.toVector

                endNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle endAngle)
                        |> Direction3d.toVector
            in
            [ ( { position = bottomCenter, normal = negativeZVector }
              , { position = p1, normal = negativeZVector }
              , { position = p0, normal = negativeZVector }
              )
            , ( { position = p0, normal = startNormal }
              , { position = p1, normal = endNormal }
              , { position = p3, normal = endNormal }
              )
            , ( { position = p0, normal = startNormal }
              , { position = p3, normal = endNormal }
              , { position = p2, normal = startNormal }
              )
            , ( { position = topCenter, normal = positiveZVector }
              , { position = p2, normal = positiveZVector }
              , { position = p3, normal = positiveZVector }
              )
            ]

        wedges =
            List.range 0 (subdivisions - 1)
                |> List.map wedge

        mapTriple mapFunc ( a, b, c ) =
            ( mapFunc a, mapFunc b, mapFunc c )

        recordToVec3 { x, y, z } =
            Vec3.vec3 x y z
    in
    List.concat wedges
        |> List.map
            (mapTriple
                (\a ->
                    { position = Point3d.unwrap a.position |> recordToVec3
                    , normal = Vector3d.unwrap (Vector3d.reverse a.normal) |> recordToVec3
                    }
                )
            )


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
                facet (position p1) (position p2) (position p3)
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


facet : Vec3 -> Vec3 -> Vec3 -> ( Attributes, Attributes, Attributes )
facet a b c =
    let
        n =
            Vec3.cross (Vec3.sub b a) (Vec3.sub b c)
    in
    ( Attributes a n
    , Attributes b n
    , Attributes c n
    )
