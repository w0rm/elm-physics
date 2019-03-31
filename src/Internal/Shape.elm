module Internal.Shape exposing
    ( Kind(..)
    , Protected(..)
    , Shape
    , aabbClosure
    , expandBoundingSphereRadius
    )

import Internal.AABB as AABB
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)


type Protected
    = Protected Shape


type alias Shape =
    { position : Vec3
    , orientation : Quaternion
    , kind : Kind
    }


type Kind
    = Convex ConvexPolyhedron
    | Plane
    | Sphere Float


aabbClosure : Kind -> Transform -> AABB.AABB
aabbClosure kind =
    case kind of
        Convex convexPolyhedron ->
            AABB.convexPolyhedron convexPolyhedron

        Plane ->
            AABB.plane

        Sphere radius ->
            AABB.sphere radius


expandBoundingSphereRadius : Shape -> Float -> Float
expandBoundingSphereRadius { position, orientation, kind } boundingSphereRadius =
    case kind of
        Convex convexPolyhedron ->
            ConvexPolyhedron.expandBoundingSphereRadius
                { position = position, orientation = orientation }
                convexPolyhedron
                boundingSphereRadius

        Sphere radius ->
            Const.zero3
                |> Transform.pointToWorldFrame { position = position, orientation = orientation }
                |> Vec3.length
                |> (+) radius
                |> max boundingSphereRadius

        Plane ->
            Const.maxNumber
