module Physics.Shape exposing (..)

import Physics.ConvexPolyhedron exposing (ConvexPolyhedron)


type alias ShapeId =
    Int


type Shape
    = Convex ConvexPolyhedron
    | Plane
