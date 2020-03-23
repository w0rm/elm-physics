module Physics.Shape exposing (Shape, block, sphere)

{-|

@docs Shape, block, sphere

-}

import Block3d exposing (Block3d)
import Direction3d
import Frame3d
import Internal.Convex as Convex
import Internal.Shape as Internal exposing (Protected(..))
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d
import Sphere3d exposing (Sphere3d)


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

        halfX =
            Length.inMeters sizeX * 0.5

        halfY =
            Length.inMeters sizeY * 0.5

        halfZ =
            Length.inMeters sizeZ * 0.5

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
    in
    Protected
        { transform3d = Transform3d.fromOriginAndBasis origin x y z
        , kind = Internal.Convex (Convex.fromBlock halfX halfY halfZ)
        }


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
        { transform3d = Transform3d.atPoint origin
        , kind = Internal.Sphere radius
        }
