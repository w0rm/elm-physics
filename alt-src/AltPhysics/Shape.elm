module AltPhysics.Shape exposing
    ( Shape(..)
    , ShapeId
    , aabbClosure
    , expandBoundingSphereRadius
    )

import AltMath.Vector3 as Vec3
import AltPhysics.AABB as AABB
import AltPhysics.Const as Const
import AltPhysics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import AltPhysics.Transform as Transform exposing (Transform)


type alias ShapeId =
    Int


type Shape
    = Convex ConvexPolyhedron
    | Plane
    | Sphere Float


aabbClosure : Shape -> Transform -> AABB.AABB
aabbClosure shape =
    case shape of
        Convex convexPolyhedron ->
            AABB.convexPolyhedron convexPolyhedron

        Plane ->
            AABB.plane

        Sphere radius ->
            AABB.sphere radius


expandBoundingSphereRadius : Transform -> Shape -> Float -> Float
expandBoundingSphereRadius shapeTransform shape boundingSphereRadius =
    case shape of
        Convex convexPolyhedron ->
            ConvexPolyhedron.expandBoundingSphereRadius
                shapeTransform
                convexPolyhedron
                boundingSphereRadius

        Sphere radius ->
            Const.zero3
                |> Transform.pointToWorldFrame shapeTransform
                |> Vec3.length
                |> (+) radius
                |> max boundingSphereRadius

        Plane ->
            Const.maxNumber
