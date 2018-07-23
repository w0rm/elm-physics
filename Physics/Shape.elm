module Physics.Shape
    exposing
        ( ShapeId
        , Shape(..)
        , aabbClosure
        , expandBoundingSphereRadius
        )

import Physics.AABB as AABB
import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Physics.Transform exposing (Transform)
import Physics.Const as Const
import Math.Vector3 as Vec3
import Physics.Transform as Transform


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
