module Physics.Shape exposing
    ( Shape, block, sphere
    , moveTo, translateBy, rotateAround
    )

{-|

@docs Shape, block, sphere


## Positioning and Orientation

Shapes are positioned in the body coordinate system,
they can be moved and rotated just like bodies in
the world.

@docs moveTo, translateBy, rotateAround

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d
import Internal.Convex as Convex
import Internal.Shape as Internal exposing (Protected(..))
import Internal.Transform3d as Transform3d
import Length exposing (Length, Meters)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


{-| Shapes are only needed for creating [compound](Physics-Body#compound) bodies.

If you need a body with a single shape, use the corresponding functions
from the [Physics.Body](Physics-Body) module.

The only supported shapes are:

  - [block](#block),
  - [sphere](#sphere).

-}
type alias Shape =
    Protected


{-| A block is defined by dimensions along the x, y and z axes.
-}
block : Length -> Length -> Length -> Shape
block x y z =
    let
        halfX =
            Length.inMeters x * 0.5

        halfY =
            Length.inMeters y * 0.5

        halfZ =
            Length.inMeters z * 0.5
    in
    Protected
        { transform3d = Transform3d.atOrigin
        , kind = Internal.Convex (Convex.fromBlock halfX halfY halfZ)
        , volume = Length.inMeters x * Length.inMeters y * Length.inMeters z
        }


{-| A sphere is defined by a radius and a position.
-}
sphere : Length -> Shape
sphere radius =
    Protected
        { transform3d = Transform3d.atOrigin
        , kind = Internal.Sphere (Length.inMeters radius)
        , volume = 4 / 3 * pi * (Length.inMeters radius ^ 3)
        }


{-| Set the position of the shape in the body,
e.g. to raise a shape 5 meters above the origin:

    movedShape =
        shape
            |> moveTo (Point3d.meters 0 0 5)

-}
moveTo : Point3d Meters BodyCoordinates -> Shape -> Shape
moveTo point3d (Protected shape) =
    let
        newTransform3d =
            Transform3d.moveTo (Point3d.toMeters point3d) shape.transform3d
    in
    Protected { shape | transform3d = newTransform3d }


{-| Rotate the shape in the body around axis,
e.g. to rotate a shape 45 degrees around Z axis:

    rotatedShape =
        shape
            |> rotateAround Axis3d.z (Angle.degrees 45)

-}
rotateAround : Axis3d Meters BodyCoordinates -> Angle -> Shape -> Shape
rotateAround axis angle (Protected shape) =
    let
        rotatedOrigin =
            Point3d.rotateAround
                axis
                angle
                (Point3d.fromMeters
                    (Transform3d.originPoint shape.transform3d)
                )

        newTransform3d =
            shape.transform3d
                |> Transform3d.moveTo
                    (Point3d.toMeters rotatedOrigin)
                |> Transform3d.rotateAroundOwn
                    (Direction3d.unwrap (Axis3d.direction axis))
                    (Angle.inRadians angle)
    in
    Protected { shape | transform3d = newTransform3d }


{-| Move the shape in the body relative to its current position,
e.g. to translate a shape down by 5 meters:

    translatedShape =
        shape
            |> translateBy (Vector3d.meters 0 0 -5)

-}
translateBy : Vector3d Meters BodyCoordinates -> Shape -> Shape
translateBy vector3d (Protected shape) =
    let
        newTransform3d =
            Transform3d.translateBy
                (Vector3d.toMeters vector3d)
                shape.transform3d
    in
    Protected { shape | transform3d = newTransform3d }
