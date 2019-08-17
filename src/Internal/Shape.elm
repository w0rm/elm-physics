module Internal.Shape exposing
    ( Kind(..)
    , Protected(..)
    , Shape
    , aabbClosure
    , expandBoundingSphereRadius
    , raycast
    )

import Internal.AABB as AABB
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Quaternion exposing (Quaternion)
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
    = Convex Convex
    | Plane
    | Sphere Float


aabbClosure : Kind -> Transform -> AABB.AABB
aabbClosure kind =
    case kind of
        Convex convex ->
            AABB.convex convex

        Plane ->
            AABB.plane

        Sphere radius ->
            AABB.sphere radius


expandBoundingSphereRadius : Shape -> Float -> Float
expandBoundingSphereRadius { position, orientation, kind } boundingSphereRadius =
    case kind of
        Convex convex ->
            Convex.expandBoundingSphereRadius
                { position = position, orientation = orientation }
                convex
                boundingSphereRadius

        Sphere radius ->
            Vec3.zero
                |> Transform.pointToWorldFrame { position = position, orientation = orientation }
                |> Vec3.length
                |> (+) radius
                |> max boundingSphereRadius

        Plane ->
            Const.maxNumber


raycast : { from : Vec3, direction : Vec3 } -> Transform -> Shape -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray transform { kind } =
    case kind of
        Plane ->
            Nothing

        Sphere radius ->
            raycastSphere ray transform.position radius

        Convex convex ->
            Convex.raycast ray transform convex


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
