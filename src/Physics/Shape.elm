module Physics.Shape exposing (Shape, block, sphere, unsafeConvex)

{-|

@docs Shape, block, sphere, unsafeConvex

-}

import Block3d exposing (Block3d)
import Direction3d
import Frame3d
import Internal.Shape as Internal exposing (Protected(..))
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Shapes.Convex as Convex
import Shapes.Sphere as Sphere
import Sphere3d exposing (Sphere3d)
import TriangularMesh exposing (TriangularMesh)


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The only supported shapes are:

  - [block](#block),
  - [sphere](#sphere).

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


{-| -}
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
