module Physics.Shape exposing
    ( Shape, block, sphere, cylinder
    , minus, plus, sum, unsafeConvex
    )

{-|

@docs Shape, block, sphere, cylinder


# Complex shapes

@docs minus, plus, sum, unsafeConvex

-}

import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Frame3d
import Internal.Coordinates exposing (BodyCoordinates)
import Internal.Shape as Internal
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Physics.Types as Types
import Point3d exposing (Point3d)
import Shapes.Convex as Convex
import Shapes.Sphere as Sphere
import Sphere3d exposing (Sphere3d)
import TriangularMesh exposing (TriangularMesh)


{-| Shapes are needed for creating compound [dynamic](Physics#dynamic)
and [static](Physics#static) bodies.

The supported primitive shapes are [block](#block), [sphere](#sphere),
and [cylinder](#cylinder). For complex geometry use [unsafeConvex](#unsafeConvex).

Shapes within a body **should not overlap** — composing shapes only affects physical
properties like mass, inertia, and center of mass. Use [plus](#plus) and
[minus](#minus) to combine shapes, for example to create hollow bodies.

-}
type alias Shape =
    Types.Shape


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
    Types.Shape
        [ ( Internal.Convex
                (Convex.fromBlock
                    (Length.inMeters sizeX)
                    (Length.inMeters sizeY)
                    (Length.inMeters sizeZ)
                    |> Convex.placeIn tranform3d
                )
          , 1
          )
        ]


{-| -}
sphere : Sphere3d Meters BodyCoordinates -> Shape
sphere sphere3d =
    let
        radius =
            Length.inMeters (Sphere3d.radius sphere3d)

        origin =
            Point3d.toMeters (Sphere3d.centerPoint sphere3d)
    in
    Types.Shape
        [ ( Internal.Sphere
                (Sphere.atOrigin radius
                    |> Sphere.placeIn (Transform3d.atPoint origin)
                )
          , 1
          )
        ]


{-| Create a cylinder shape with the given number of side faces, clamped to at least 3.
Even numbers are more efficient, because collision performance depends
on the number of unique non-parallel faces and edges.
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
    Types.Shape
        [ ( Convex.fromCylinder (max 3 subdivisions)
                (Length.inMeters (Cylinder3d.radius cylinder3d))
                (Length.inMeters (Cylinder3d.length cylinder3d))
                |> Convex.placeIn transform3d
                |> Internal.Convex
          , 1
          )
        ]


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
    Types.Shape
        [ ( Internal.Convex
                (Convex.fromTriangularMesh faceIndices vertices)
          , 1
          )
        ]


{-| Add a shape to another.

    snowman =
        Shape.sphere bottom
            |> Shape.plus (Shape.sphere top)

-}
plus : Shape -> Shape -> Shape
plus (Types.Shape toAdd) (Types.Shape base) =
    Types.Shape (base ++ toAdd)


{-| Subtract a shape from another, the first argument is subtracted from the second.
The subtracted shape reduces volume, mass and inertia and is excluded from collision.
Useful for hollow bodies.

    crate =
        Shape.block outer
            |> Shape.minus (Shape.block inner)

-}
minus : Shape -> Shape -> Shape
minus (Types.Shape toSubtract) (Types.Shape base) =
    Types.Shape (base ++ List.map (Tuple.mapSecond negate) toSubtract)


{-| Combine a list of shapes.

    dumbbell =
        Shape.sum
            [ Shape.cylinder 12 leftWeight
            , Shape.cylinder 12 bar
            , Shape.cylinder 12 rightWeight
            ]

-}
sum : List Shape -> Shape
sum =
    List.foldl plus (Types.Shape [])
