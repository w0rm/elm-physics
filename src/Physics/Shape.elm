module Physics.Shape exposing
    ( Shape, block, sphere, unsafeConvex
    , cylinder
    )

{-|

@docs Shape, block, sphere, unsafeConvex

-}

import Angle
import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Frame3d
import Internal.Matrix3
import Internal.Shape as Internal exposing (Protected(..))
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Shapes.Convex as Convex
import Shapes.Sphere as Sphere
import Sphere3d exposing (Sphere3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The supported primitive shapes are:

  - [block](#block),
  - [sphere](#sphere).

For the more complex cases use the [unsafeConvex](#unsafeConvex) shape.

Shapes are defined in the body coordinate system.

-}
type alias Shape =
    Protected


{-| -}
block : Block3d Meters BodyCoordinates -> Shape
block block3d =
    let
        ( sizeX, sizeY, sizeZ ) =
            Block3d.dimensions block3d

        frame3d =
            Block3d.axes block3d

        rightHandedFrame3d =
            if Frame3d.isRightHanded frame3d then
                frame3d

            else
                Frame3d.reverseZ frame3d

        origin =
            Point3d.unwrap (Frame3d.originPoint rightHandedFrame3d)

        x =
            Direction3d.unwrap (Frame3d.xDirection rightHandedFrame3d)

        y =
            Direction3d.unwrap (Frame3d.yDirection rightHandedFrame3d)

        z =
            Direction3d.unwrap (Frame3d.zDirection rightHandedFrame3d)

        tranform3d =
            Transform3d.fromOriginAndBasis origin x y z
    in
    Protected
        (Internal.Convex
            (Convex.fromBlock
                (Length.inMeters sizeX)
                (Length.inMeters sizeY)
                (Length.inMeters sizeZ)
                |> Convex.placeIn tranform3d
            )
        )


{-| -}
sphere : Sphere3d Meters BodyCoordinates -> Shape
sphere sphere3d =
    let
        radius =
            Length.inMeters (Sphere3d.radius sphere3d)

        origin =
            Point3d.toMeters (Sphere3d.centerPoint sphere3d)
    in
    Protected
        (Internal.Sphere
            (Sphere.atOrigin radius
                |> Sphere.placeIn (Transform3d.atPoint origin)
            )
        )


{-|

    cylinder 12 myCylinder -- A cylinder with 12 faces (not counting the top and bottom face)

    cylinder 2 myCylinder -- Too few faces so it has 3 faces instead

-}
cylinder : Int -> Cylinder3d Meters BodyCoordinates -> Shape
cylinder detail cylinder3d =
    let
        detail_ =
            max 3 detail

        ( a, b ) =
            Cylinder3d.axialDirection cylinder3d |> Direction3d.perpendicularBasis

        top =
            Cylinder3d.length cylinder3d |> Quantity.unwrap |> (*) 0.5

        bottom =
            Cylinder3d.length cylinder3d |> Quantity.unwrap |> (*) -0.5

        radius =
            Cylinder3d.radius cylinder3d |> Quantity.unwrap

        faces =
            List.range 0 (detail_ - 1)
                |> List.map
                    (\value ->
                        let
                            r0 =
                                2 * pi * (toFloat value - 0.5) / toFloat detail_

                            r1 =
                                2 * pi * toFloat value / toFloat detail_

                            r2 =
                                2 * pi * (toFloat value + 0.5) / toFloat detail_
                        in
                        { normal = { x = sin r1, y = cos r1, z = 0 }
                        , v0 = { x = sin r0 * radius, y = cos r0 * radius, z = top }
                        , v1 = { x = sin r2 * radius, y = cos r2 * radius, z = top }
                        , v2 = { x = sin r2 * radius, y = cos r2 * radius, z = bottom }
                        , v3 = { x = sin r0 * radius, y = cos r0 * radius, z = bottom }
                        }
                    )

        transform3d =
            Transform3d.fromOriginAndBasis
                (Cylinder3d.centerPoint cylinder3d |> Point3d.unwrap)
                (Direction3d.unwrap a)
                (Direction3d.unwrap b)
                (Cylinder3d.axialDirection cylinder3d |> Direction3d.toVector |> Vector3d.unwrap)

        volume =
            Cylinder3d.volume cylinder3d |> Quantity.unwrap

        cap z =
            List.range 0 (detail_ - 1)
                |> List.map
                    (\value ->
                        let
                            r0 =
                                2 * pi * (toFloat value - 0.5) / toFloat detail_
                        in
                        { x = sin r0 * radius, y = cos r0 * radius, z = z }
                    )
    in
    { faces =
        { vertices = cap bottom, normal = { x = 0, y = 0, z = -1 } }
            :: { vertices = List.reverse <| cap top, normal = { x = 0, y = 0, z = 1 } }
            :: List.map
                (\face ->
                    { vertices = [ face.v0, face.v1, face.v2, face.v3 ]
                    , normal = face.normal
                    }
                )
                faces
    , vertices = List.concatMap (\face -> [ face.v1, face.v2 ]) faces
    , uniqueEdges = List.concatMap (\face -> [ face.v0, face.v1, face.v2, face.v3, face.v1, face.v2 ]) faces
    , uniqueNormals =
        { x = 0, y = 0, z = -1 }
            :: { x = 0, y = 0, z = 1 }
            :: (if modBy 2 detail_ == 0 then
                    List.map .normal faces |> List.take (detail_ // 2)

                else
                    List.map .normal faces
               )
    , position = { x = 0, y = 0, z = 0 }
    , inertia =
        Internal.Matrix3.cylinderInertia
            volume
            radius
            (Cylinder3d.length cylinder3d |> Quantity.unwrap)
    , volume = volume
    }
        |> Convex.placeIn transform3d
        |> Internal.Convex
        |> Protected


{-| Create a shape from the triangular mesh. This is useful if you want
to import from Blender using [elm-obj-file](https://package.elm-lang.org/packages/w0rm/elm-obj-file/latest).

**Note:** this may cause unexpected behavior, unless you make sure that:

  - the mesh is a [convex polyhedron](https://en.wikipedia.org/wiki/Convex_polytope);
  - the mesh is watertight, consisting of one closed surface;
  - all faces have counterclockwise [winding order](https://cmichel.io/understanding-front-faces-winding-order-and-normals).

-}
unsafeConvex : TriangularMesh (Point3d Meters BodyCoordinates) -> Shape
unsafeConvex triangularMesh =
    let
        faceIndices =
            TriangularMesh.faceIndices triangularMesh

        vertices =
            triangularMesh
                |> TriangularMesh.mapVertices Point3d.toMeters
                |> TriangularMesh.vertices
    in
    Protected
        (Internal.Convex
            (Convex.fromTriangularMesh faceIndices vertices)
        )
