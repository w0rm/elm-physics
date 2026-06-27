module Common.Meshes exposing
    ( Attributes
    , Meshes
    , ShadowVertex
    , block
    , capsule
    , cone
    , contact
    , cylinder
    , fromTriangleGroups
    , fromTriangles
    , sphere
    , triangularMesh
    )

import Block3d exposing (Block3d)
import Cone3d exposing (Cone3d)
import Cylinder3d exposing (Cylinder3d)
import Dict exposing (Dict)
import Direction3d
import Frame3d
import Length exposing (Meters, inMeters)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Sphere3d exposing (Sphere3d)
import TriangularMesh exposing (TriangularMesh)
import WebGL exposing (Mesh)


type alias Attributes =
    { position : Vec3
    , barycentric : Vec3
    }


{-| A renderable body: its lit mesh plus the matching shadow-volume mesh
used by the stencil shadow pass.
-}
type alias Meshes =
    { mesh : Mesh Attributes
    , shadow : Mesh ShadowVertex
    }


{-| A shadow-volume vertex carries the adjacent face normal so the shadow
vertex shader can decide whether to extrude it away from the light.
-}
type alias ShadowVertex =
    { position : Vec3
    , normal : Vec3
    }



-- Meshes


{-| A unit-radius sphere; the scene scales it to the demo's `contactRadius`.
-}
contact : Mesh Attributes
contact =
    WebGL.triangles (sphere 2 (Sphere3d.atOrigin (Length.meters 1)))


{-| Build the lit mesh and its shadow volume from a single triangle list.
-}
fromTriangles : List ( Attributes, Attributes, Attributes ) -> Meshes
fromTriangles triangles =
    fromTriangleGroups [ triangles ]


{-| Like `fromTriangles`, but the shadow volume is built per group rather than
over the merged soup. Use one group per closed convex piece of a compound body:
the silhouette builder assumes each edge is shared by exactly two faces, which
breaks where pieces share coincident internal faces. Per-piece closed volumes
overlap correctly under the z-pass stencil.
-}
fromTriangleGroups : List (List ( Attributes, Attributes, Attributes )) -> Meshes
fromTriangleGroups groups =
    { mesh = WebGL.triangles (List.concat groups)
    , shadow = WebGL.triangles (List.concatMap shadowVolume groups)
    }



-- Shadow volumes


type alias EdgeKey =
    ( ( Int, Int, Int ), ( Int, Int, Int ) )


type alias Collected =
    { left : Dict EdgeKey { start : Vec3, end : Vec3, normal : Vec3 }
    , right : Dict EdgeKey Vec3
    }


{-| Turn a closed triangle mesh into shadow-volume side quads. Each shared
edge yields one quad whose two corners carry the normals of its adjacent
faces; the shadow vertex shader extrudes whichever side faces away from the
light, so only silhouette edges (one face lit, one dark) form a wall and
coplanar interior edges stay degenerate.
-}
shadowVolume : List ( Attributes, Attributes, Attributes ) -> List ( ShadowVertex, ShadowVertex, ShadowVertex )
shadowVolume triangles =
    let
        { left, right } =
            List.foldl collectEdges { left = Dict.empty, right = Dict.empty } triangles
    in
    Dict.merge
        (\_ _ acc -> acc)
        (\_ edge rightNormal acc ->
            ( ShadowVertex edge.start rightNormal
            , ShadowVertex edge.end rightNormal
            , ShadowVertex edge.end edge.normal
            )
                :: ( ShadowVertex edge.end edge.normal
                   , ShadowVertex edge.start edge.normal
                   , ShadowVertex edge.start rightNormal
                   )
                :: acc
        )
        (\_ _ acc -> acc)
        left
        right
        []


faceNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
faceNormal a b c =
    Vec3.normalize (Vec3.cross (Vec3.sub b a) (Vec3.sub c a))


collectEdges : ( Attributes, Attributes, Attributes ) -> Collected -> Collected
collectEdges ( a, b, c ) collected =
    let
        normal =
            faceNormal a.position b.position c.position
    in
    collected
        |> addEdge normal a.position b.position
        |> addEdge normal b.position c.position
        |> addEdge normal c.position a.position


addEdge : Vec3 -> Vec3 -> Vec3 -> Collected -> Collected
addEdge normal start end collected =
    let
        startKey =
            vertexKey start

        endKey =
            vertexKey end
    in
    if startKey < endKey then
        { collected
            | left =
                Dict.insert ( startKey, endKey )
                    { start = start, end = end, normal = normal }
                    collected.left
        }

    else
        { collected | right = Dict.insert ( endKey, startKey ) normal collected.right }


{-| Quantize a position so vertices shared between faces hash to the same
key despite tiny floating-point differences.
-}
vertexKey : Vec3 -> ( Int, Int, Int )
vertexKey v =
    ( round (Vec3.getX v * 4096)
    , round (Vec3.getY v * 4096)
    , round (Vec3.getZ v * 4096)
    )


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


{-| Render a capsule (cylinder with hemispherical end caps) as a triangle mesh.
The capsule axis, center, radius, and length are taken from the given `Cylinder3d`,
matching the convention used by `Shape.capsule`.
-}
capsule : Int -> Cylinder3d Meters BodyCoordinates -> List ( Attributes, Attributes, Attributes )
capsule subdivisions cylinder3d =
    let
        clampedSubdivisions =
            max 3 subdivisions

        wedgeAngle =
            2 * pi / toFloat clampedSubdivisions

        length =
            Length.inMeters (Cylinder3d.length cylinder3d)

        radius =
            Length.inMeters (Cylinder3d.radius cylinder3d)

        halfLength =
            0.5 * length

        -- Number of latitude bands per hemisphere cap
        latBands =
            max 2 (clampedSubdivisions // 2)

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

        -- Cylindrical body sides (no flat end caps)
        sideWedge startIndex =
            let
                theta0 =
                    wedgeAngle * toFloat startIndex

                theta1 =
                    wedgeAngle * toFloat (modBy clampedSubdivisions (startIndex + 1))

                p0 =
                    transform (radius * cos theta0) (radius * sin theta0) -halfLength

                p1 =
                    transform (radius * cos theta1) (radius * sin theta1) -halfLength

                p2 =
                    transform (radius * cos theta0) (radius * sin theta0) halfLength

                p3 =
                    transform (radius * cos theta1) (radius * sin theta1) halfLength
            in
            [ facet p1 p3 p0 1
            , facet p2 p0 p3 1
            ]

        -- Top hemisphere cap band (outward normal has +z component)
        topCapBand startIndex j =
            let
                theta0 =
                    wedgeAngle * toFloat startIndex

                theta1 =
                    wedgeAngle * toFloat (modBy clampedSubdivisions (startIndex + 1))

                phi0 =
                    pi / 2 * toFloat j / toFloat latBands

                phi1 =
                    pi / 2 * toFloat (j + 1) / toFloat latBands

                z0 =
                    halfLength + radius * sin phi0

                z1 =
                    halfLength + radius * sin phi1

                r0 =
                    radius * cos phi0

                r1 =
                    radius * cos phi1

                p0 =
                    transform (r0 * cos theta0) (r0 * sin theta0) z0

                p1 =
                    transform (r0 * cos theta1) (r0 * sin theta1) z0

                p2 =
                    transform (r1 * cos theta0) (r1 * sin theta0) z1

                p3 =
                    transform (r1 * cos theta1) (r1 * sin theta1) z1
            in
            if j == latBands - 1 then
                -- Last band converges at the pole: emit one triangle.
                -- Hide both radial spokes (b-c and c-a), keep only the
                -- latitude rim a-b = p0-p1.
                [ facet p0 p1 p2 5 ]

            else
                -- Hide the quad diagonal p1-p2: it's edge b-c in
                -- triangle 1 (remove=1) and edge c-a in triangle 2
                -- (remove=3).
                [ facet p0 p1 p2 1
                , facet p1 p3 p2 3
                ]

        -- Bottom hemisphere cap band (outward normal has -z component; winding reversed)
        bottomCapBand startIndex j =
            let
                theta0 =
                    wedgeAngle * toFloat startIndex

                theta1 =
                    wedgeAngle * toFloat (modBy clampedSubdivisions (startIndex + 1))

                phi0 =
                    pi / 2 * toFloat j / toFloat latBands

                phi1 =
                    pi / 2 * toFloat (j + 1) / toFloat latBands

                z0 =
                    -(halfLength + radius * sin phi0)

                z1 =
                    -(halfLength + radius * sin phi1)

                r0 =
                    radius * cos phi0

                r1 =
                    radius * cos phi1

                p0 =
                    transform (r0 * cos theta0) (r0 * sin theta0) z0

                p1 =
                    transform (r0 * cos theta1) (r0 * sin theta1) z0

                p2 =
                    transform (r1 * cos theta0) (r1 * sin theta0) z1

                p3 =
                    transform (r1 * cos theta1) (r1 * sin theta1) z1
            in
            if j == latBands - 1 then
                -- Last band converges at the pole (reversed winding):
                -- hide both radial spokes (a-b and b-c), keep only the
                -- latitude rim c-a = p1-p0.
                [ facet p0 p2 p1 6 ]

            else
                -- Hide the quad diagonal p1-p2: it's edge b-c in
                -- triangle 1 (remove=1) and edge a-b in triangle 2
                -- (remove=4).
                [ facet p0 p2 p1 1
                , facet p1 p2 p3 4
                ]
    in
    List.concat
        [ List.range 0 (clampedSubdivisions - 1)
            |> List.concatMap sideWedge
        , List.range 0 (clampedSubdivisions - 1)
            |> List.concatMap
                (\i -> List.range 0 (latBands - 1) |> List.concatMap (topCapBand i))
        , List.range 0 (clampedSubdivisions - 1)
            |> List.concatMap
                (\i -> List.range 0 (latBands - 1) |> List.concatMap (bottomCapBand i))
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
                -- Match Convex.fromCylinder's ring exactly (vertices at the
                -- (k - 0.5) wedge angles, x = sin / y = cos) so the rendered
                -- mesh sits on the collision hull rather than rotated off it.
                startAngle =
                    wedgeAngle * (toFloat startIndex - 0.5)

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle * (toFloat endIndex - 0.5)

                startX =
                    radius * sin startAngle

                endX =
                    radius * sin endAngle

                startY =
                    radius * cos startAngle

                endY =
                    radius * cos endAngle

                p0 =
                    transform startX startY bottomZ

                p1 =
                    transform endX endY bottomZ

                p2 =
                    transform startX startY topZ

                p3 =
                    transform endX endY topZ
            in
            -- The ring uses x = sin / y = cos (to match Convex.fromCylinder),
            -- which reverses angular handedness, so the outward winding is the
            -- swap of the usual order: front faces and shadow normals face out.
            [ facet topCenter p3 p2 2
            , facet p1 p0 p3 1
            , facet p2 p3 p0 1
            , facet bottomCenter p0 p1 2
            ]
    in
    List.range 0 (subdivisions - 1)
        |> List.concatMap wedge


cone : Int -> Cone3d Meters BodyCoordinates -> List ( Attributes, Attributes, Attributes )
cone subdivisions cone3d =
    let
        wedgeAngle =
            2 * pi / toFloat subdivisions

        length =
            Length.inMeters (Cone3d.length cone3d)

        radius =
            Length.inMeters (Cone3d.radius cone3d)

        bottomZ =
            -0.5 * length

        topZ =
            0.5 * length

        ( a, b ) =
            Cone3d.axialDirection cone3d
                |> Direction3d.perpendicularBasis

        cylinderFrame3d =
            Frame3d.unsafe
                { originPoint = Point3d.midpoint (Cone3d.basePoint cone3d) (Cone3d.tipPoint cone3d)
                , xDirection = a
                , yDirection = b
                , zDirection = Cone3d.axialDirection cone3d
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
                -- Match Convex.fromCylinder's ring exactly (vertices at the
                -- (k - 0.5) wedge angles, x = sin / y = cos) so the rendered
                -- mesh sits on the collision hull rather than rotated off it.
                startAngle =
                    wedgeAngle * (toFloat startIndex - 0.5)

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle * (toFloat endIndex - 0.5)

                startX =
                    radius * sin startAngle

                endX =
                    radius * sin endAngle

                startY =
                    radius * cos startAngle

                endY =
                    radius * cos endAngle

                p0 =
                    transform startX startY bottomZ

                p1 =
                    transform endX endY bottomZ
            in
            -- The ring uses x = sin / y = cos (to match Convex.fromCylinder),
            -- which reverses angular handedness, so the outward winding is the
            -- swap of the usual order: front faces and shadow normals face out.
            [ facet topCenter p1 p0 1
            , facet bottomCenter p0 p1 2
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
        divideSphere (step - 1) radius (List.foldl (divide radius) [] triangles)


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


{-| Barycentric encoding controls which of a triangle's three edges
the wireframe shader draws. The shader takes `min(b.x, b.y, b.z)` as
"distance to nearest edge", so an edge is drawn iff some component
reads 0 along it; an edge is hidden iff no component reads 0 along it.

Cases:

  - `0` — all three edges drawn (default, full wireframe).
  - `1` — hide edge b-c (use on quad diagonals where the diagonal is
    the b-c edge of the triangle).
  - `2` — draw only edge b-c (use on radial fans that converge to a
    centre vertex; keeps only the rim segment).
  - `3` — hide edge c-a.
  - `4` — hide edge a-b.
  - `5` — draw only edge a-b (radial fan to a pole, keep the latitude).
  - `6` — draw only edge c-a (radial fan to a pole, keep the latitude).

-}
facet : Vec3 -> Vec3 -> Vec3 -> Int -> ( Attributes, Attributes, Attributes )
facet a b c remove =
    case remove of
        1 ->
            ( Attributes a (vec3 0 0 1)
            , Attributes b (vec3 0 1 0)
            , Attributes c (vec3 1 0 1)
            )

        2 ->
            ( Attributes a (vec3 1 1 1)
            , Attributes b (vec3 1 0 0)
            , Attributes c (vec3 1 0 0)
            )

        3 ->
            ( Attributes a (vec3 0 1 1)
            , Attributes b (vec3 0 1 0)
            , Attributes c (vec3 1 0 0)
            )

        4 ->
            ( Attributes a (vec3 0 0 1)
            , Attributes b (vec3 1 1 0)
            , Attributes c (vec3 1 0 0)
            )

        5 ->
            ( Attributes a (vec3 0 1 1)
            , Attributes b (vec3 0 1 1)
            , Attributes c (vec3 1 1 1)
            )

        6 ->
            ( Attributes a (vec3 1 0 1)
            , Attributes b (vec3 1 1 1)
            , Attributes c (vec3 1 0 1)
            )

        _ ->
            ( Attributes a (vec3 0 0 1)
            , Attributes b (vec3 0 1 0)
            , Attributes c (vec3 1 0 0)
            )
