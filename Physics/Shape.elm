module Physics.Shape exposing (ShapeId, Shape(..))

import Physics.ConvexPolyhedron exposing (ConvexPolyhedron)


type alias ShapeId =
    Int


type Shape
    = Convex ConvexPolyhedron
    | Plane
