module Internal.Shape exposing
    ( Protected(..)
    , Shape(..)
    , aabbClosure
    , expandBoundingSphereRadius
    , raycast
    , shapesPlaceIn
    , volume
    )

import Internal.AABB as AABB
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Plane as Plane exposing (Plane)
import Internal.Sphere as Sphere exposing (Sphere)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)


type Protected
    = Protected (Shape BodyCoordinates)


type Shape coordinates
    = Convex Convex
    | Plane Plane
    | Sphere Sphere
    | Particle Vec3


volume : Shape coordinates -> Float
volume shape =
    case shape of
        Sphere { radius } ->
            4 / 3 * pi * (radius ^ 3)

        Convex convex ->
            convex.volume

        Plane _ ->
            0

        Particle _ ->
            0


aabbClosure : Shape CenterOfMassCoordinates -> AABB.AABB
aabbClosure shape =
    case shape of
        Convex convex ->
            AABB.convex convex

        Plane plane ->
            AABB.plane plane

        Sphere sphere ->
            AABB.sphere sphere

        Particle position ->
            AABB.particle position


{-| Transforms shapes, reverses the original order
-}
shapesPlaceIn : Transform3d coordinates { defines : originalCoords } -> List (Shape originalCoords) -> List (Shape coordinates)
shapesPlaceIn transform3d shapes =
    shapesPlaceInHelp transform3d shapes []


shapesPlaceInHelp : Transform3d coordinates { defines : originalCoords } -> List (Shape originalCoords) -> List (Shape coordinates) -> List (Shape coordinates)
shapesPlaceInHelp transform3d shapes result =
    case shapes of
        shape :: remainingShapes ->
            shapesPlaceInHelp
                transform3d
                remainingShapes
                ((case shape of
                    Convex convex ->
                        Convex (Convex.placeIn transform3d convex)

                    Plane plane ->
                        Plane (Plane.placeIn transform3d plane)

                    Sphere sphere ->
                        Sphere (Sphere.placeIn transform3d sphere)

                    Particle position ->
                        Particle (Transform3d.pointPlaceIn transform3d position)
                 )
                    :: result
                )

        [] ->
            result


expandBoundingSphereRadius : Shape CenterOfMassCoordinates -> Float -> Float
expandBoundingSphereRadius shape boundingSphereRadius =
    case shape of
        Convex convex ->
            Convex.expandBoundingSphereRadius convex boundingSphereRadius

        Sphere sphere ->
            Sphere.expandBoundingSphereRadius sphere boundingSphereRadius

        Plane _ ->
            Const.maxNumber

        Particle position ->
            max boundingSphereRadius (Vec3.length position)


raycast : { from : Vec3, direction : Vec3 } -> Shape WorldCoordinates -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray shape =
    case shape of
        Plane plane ->
            Plane.raycast ray plane

        Sphere sphere ->
            Sphere.raycast ray sphere

        Convex convex ->
            Convex.raycast ray convex

        Particle _ ->
            Nothing
