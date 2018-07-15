module Physics.Shape exposing (ShapeId, Shape(..), aabbClosure, boundingSphereRadiusClosure)

import Physics.AABB as AABB
import Physics.Const as Const
import Physics.ConvexPolyhedron exposing (ConvexPolyhedron, convexRadius)
import Physics.Sphere exposing (Sphere, boundingRadius)
import Physics.Transform exposing (Transform)


type alias ShapeId =
    Int


type Shape
    = Convex ConvexPolyhedron
    | Plane
    | Sphere Sphere


aabbClosure : Shape -> Transform -> AABB.AABB
aabbClosure shape =
    case shape of
        Convex convexPolyhedron ->
            AABB.convexPolyhedron convexPolyhedron

        Plane ->
            AABB.plane

        Sphere { radius } ->
            AABB.sphere radius


boundingSphereRadiusClosure : Shape -> Transform -> Float
boundingSphereRadiusClosure shape =
    let
        planeRadius transform =
            Const.maxNumber

    in

        case shape of
            Convex convexPolyhedron ->
                convexRadius convexPolyhedron

            Plane ->
                planeRadius

            Sphere { radius } ->
                boundingRadius radius
