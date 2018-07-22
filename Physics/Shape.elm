module Physics.Shape exposing (ShapeId, Shape(..), aabbClosure)

import Physics.AABB as AABB
import Physics.ConvexPolyhedron exposing (ConvexPolyhedron)
import Physics.Transform exposing (Transform)


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
