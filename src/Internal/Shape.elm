module Internal.Shape exposing
    ( Kind(..)
    , Protected(..)
    , Shape
    , aabbClosure
    , expandBoundingSphereRadius
    , raycast
    , volume
    )

import Internal.AABB as AABB
import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Coordinates exposing (CenterOfMassCoordinates, ShapeCoordinates)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)


type Protected
    = Protected (Shape BodyCoordinates)


type alias Shape coordinates =
    { transform3d : Transform3d coordinates { defines : ShapeCoordinates }
    , kind : Kind
    }


type Kind
    = Convex Convex
    | Plane
    | Sphere Float
    | Particle


volume : Shape coordinates -> Float
volume { kind } =
    case kind of
        Sphere radius ->
            4 / 3 * pi * (radius ^ 3)

        Convex convex ->
            convex.volume

        Plane ->
            0

        Particle ->
            0


aabbClosure : Kind -> Transform3d CenterOfMassCoordinates { defines : ShapeCoordinates } -> AABB.AABB
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


expandBoundingSphereRadius : Shape CenterOfMassCoordinates -> Float -> Float
expandBoundingSphereRadius { transform3d, kind } boundingSphereRadius =
    case kind of
        Convex convex ->
            Convex.expandBoundingSphereRadius transform3d convex boundingSphereRadius

        Sphere radius ->
            Transform3d.originPoint transform3d
                |> Vec3.length
                |> (+) radius
                |> max boundingSphereRadius

        Plane ->
            Const.maxNumber

        Particle ->
            max boundingSphereRadius (Vec3.length (Transform3d.originPoint transform3d))


raycast : { from : Vec3, direction : Vec3 } -> Transform3d WorldCoordinates { defines : ShapeCoordinates } -> Shape CenterOfMassCoordinates -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast ray transform3d { kind } =
    case kind of
        Plane ->
            raycastPlane ray transform3d

        Sphere radius ->
            raycastSphere ray (Transform3d.originPoint transform3d) radius

        Convex convex ->
            Convex.raycast ray transform3d convex

        Particle ->
            Nothing


raycastPlane : { from : Vec3, direction : Vec3 } -> Transform3d WorldCoordinates { defines : ShapeCoordinates } -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycastPlane { from, direction } transform3d =
    let
        planeNormalWS =
            Transform3d.directionPlaceIn transform3d Vec3.k

        dot =
            Vec3.dot direction planeNormalWS
    in
    if dot < 0 then
        let
            pointOnFaceWS =
                Transform3d.originPoint transform3d

            pointToFrom =
                Vec3.sub pointOnFaceWS from

            scalar =
                Vec3.dot planeNormalWS pointToFrom / dot
        in
        if scalar >= 0 then
            Just
                { distance = scalar
                , point =
                    { x = direction.x * scalar + from.x
                    , y = direction.y * scalar + from.y
                    , z = direction.z * scalar + from.z
                    }
                , normal = planeNormalWS
                }

        else
            Nothing

    else
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
