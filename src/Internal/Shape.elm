module Internal.Shape exposing
    ( Kind(..)
    , Protected(..)
    , Shape
    , aabbClosure
    , expandBoundingSphereRadius
    , raycast
    )

import Frame3d exposing (Frame3d)
import Internal.AABB as AABB
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Coordinates exposing (BodyLocalCoordinates, ShapeLocalCoordinates, WorldCoordinates)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Length exposing (Meters)
import Point3d


type Protected
    = Protected Shape


type alias Shape =
    { frame3d : Frame3d Meters BodyLocalCoordinates { defines : ShapeLocalCoordinates }
    , kind : Kind
    }


type Kind
    = Convex Convex
    | Plane
    | Sphere Float
    | Particle


aabbClosure : Kind -> Frame3d Meters BodyLocalCoordinates { defines : ShapeLocalCoordinates } -> AABB.AABB
aabbClosure kind =
    case kind of
        Convex convex ->
            AABB.convex convex

        Plane ->
            AABB.plane

        Sphere radius ->
            AABB.sphere radius

        Particle ->
            AABB.particle


expandBoundingSphereRadius : Shape -> Float -> Float
expandBoundingSphereRadius { frame3d, kind } boundingSphereRadius =
    case kind of
        Convex convex ->
            Convex.expandBoundingSphereRadius frame3d convex boundingSphereRadius

        Sphere radius ->
            Frame3d.originPoint frame3d
                |> Point3d.distanceFrom Point3d.origin
                |> Length.inMeters
                |> (+) radius
                |> max boundingSphereRadius

        Plane ->
            Const.maxNumber

        Particle ->
            max boundingSphereRadius (Vec3.length (Point3d.toMeters (Frame3d.originPoint frame3d)))


raycast : { from : Vec3, direction : Vec3 } -> Frame3d Meters WorldCoordinates { defines : ShapeLocalCoordinates } -> Shape -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray frame3d { kind } =
    case kind of
        Plane ->
            Nothing

        Sphere radius ->
            raycastSphere ray (Point3d.toMeters (Frame3d.originPoint frame3d)) radius

        Convex convex ->
            Convex.raycast ray frame3d convex

        Particle ->
            Nothing


raycastSphere : { from : Vec3, direction : Vec3 } -> Vec3 -> Float -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycastSphere { from, direction } position radius =
    let
        a =
            direction.x * direction.x + direction.y * direction.y + direction.z * direction.z

        b =
            2 * (direction.x * (from.x - position.x) + direction.y * (from.y - position.y) + direction.z * (from.z - position.z))

        c =
            (from.x - position.x) * (from.x - position.x) + (from.y - position.y) * (from.y - position.y) + (from.z - position.z) * (from.z - position.z) - radius * radius

        delta =
            b * b - 4 * a * c
    in
    if delta < 0 then
        Nothing

    else
        let
            distance =
                (-b - sqrt delta) / (2 * a)
        in
        if distance >= 0 then
            let
                point =
                    { x = from.x + direction.x * distance
                    , y = from.y + direction.y * distance
                    , z = from.z + direction.z * distance
                    }

                normal =
                    Vec3.sub point position
            in
            Just
                { distance = distance
                , point = point
                , normal = normal
                }

        else
            Nothing
