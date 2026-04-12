module Internal.Shape exposing
    ( CenterOfMassCoordinates
    , Shape(..)
    , centerOfMass
    , expandBoundingSphereRadius
    , inertia
    , placeIn
    , raycast
    , volume
    )

import Internal.Const as Const
import Internal.Coordinates exposing (WorldCoordinates)
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex as Convex exposing (Convex)
import Shapes.Plane as Plane exposing (Plane)
import Shapes.Sphere as Sphere exposing (Sphere)


type CenterOfMassCoordinates
    = CenterOfMassCoordinates


type Shape coordinates
    = Convex Convex
    | Plane Plane
    | Sphere Sphere
    | Particle Vec3


volume : Shape coordinates -> Float
volume shape =
    case shape of
        Sphere sphere ->
            sphere.volume

        Convex convex ->
            convex.volume

        Plane _ ->
            0

        Particle _ ->
            0


inertia : Shape coordinates -> Mat3
inertia shape =
    case shape of
        Sphere sphere ->
            sphere.inertia

        Convex convex ->
            convex.inertia

        Plane _ ->
            Mat3.zero

        Particle _ ->
            Mat3.zero


centerOfMass : Shape coordinates -> Vec3
centerOfMass shape =
    case shape of
        Sphere sphere ->
            sphere.position

        Convex convex ->
            convex.position

        Plane _ ->
            Vec3.zero

        Particle _ ->
            Vec3.zero


{-| Transforms shapes, reverses the original order
-}
placeIn : Transform3d coordinates { defines : originalCoords } -> Shape originalCoords -> Shape coordinates
placeIn transform3d shape =
    case shape of
        Convex convex ->
            Convex (Convex.placeIn transform3d convex)

        Plane plane ->
            Plane (Plane.placeIn transform3d plane)

        Sphere sphere ->
            Sphere (Sphere.placeIn transform3d sphere)

        Particle position ->
            Particle (Transform3d.pointPlaceIn transform3d position)


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
