module Physics.Shape exposing (Shape, block, sphere, cylinder, unsafeConvex)

{-|

@docs Shape, block, sphere, cylinder, unsafeConvex

-}

import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Frame3d
import Internal.Shape as Internal exposing (Protected(..))
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Quantity
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
  - [sphere](#sphere),
  - [cylinder](#cylinder).

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


{-| Create a cylinder shape by specifying the number of subdivisions >= 3.

    cylinder 12 myCylinder -- A cylinder with 12 faces (not counting the top and bottom face)

    cylinder 2 myCylinder -- Too few faces so it has 3 faces instead

Note that it’s more efficient to simulate cylinders with an even number of
faces than an odd number of faces. This is because the collision performance depends
on the number of unique faces that are not parallel with each other (and edges too).

-}
cylinder : Int -> Cylinder3d Meters BodyCoordinates -> Shape
cylinder subdivisions cylinder3d =
    let
        ( a, b ) =
            Cylinder3d.axialDirection cylinder3d
                |> Direction3d.perpendicularBasis

        transform3d =
            Transform3d.fromOriginAndBasis
                (Point3d.toMeters (Cylinder3d.centerPoint cylinder3d))
                (Direction3d.unwrap a)
                (Direction3d.unwrap b)
                (Direction3d.unwrap (Cylinder3d.axialDirection cylinder3d))
    in
    Convex.fromCylinder (max 3 subdivisions)
        (Length.inMeters (Cylinder3d.radius cylinder3d))
        (Length.inMeters (Cylinder3d.length cylinder3d))
        |> Convex.placeIn transform3d
        |> Internal.Convex
        |> Protected


{-| Create a shape from a triangular mesh. This is useful if you want
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
