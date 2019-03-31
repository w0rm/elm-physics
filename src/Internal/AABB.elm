module Internal.AABB exposing
    ( AABB
    , convexPolyhedron
    , extend
    , impossible
    , plane
    , sphere
    , toHalfExtends
    )

import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Array exposing (Array)
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform exposing (Transform)


type alias AABB =
    { upperBound : Vec3
    , lowerBound : Vec3
    }


zero : AABB
zero =
    { lowerBound = Const.zero3
    , upperBound = Const.zero3
    }


maximum : AABB
maximum =
    { lowerBound = vec3 -Const.maxNumber -Const.maxNumber -Const.maxNumber
    , upperBound = vec3 Const.maxNumber Const.maxNumber Const.maxNumber
    }


impossible : AABB
impossible =
    { lowerBound = vec3 Const.maxNumber Const.maxNumber Const.maxNumber
    , upperBound = vec3 -Const.maxNumber -Const.maxNumber -Const.maxNumber
    }


extend : AABB -> AABB -> AABB
extend aabb1 aabb =
    let
        l =
            aabb.lowerBound

        u =
            aabb.upperBound

        l1 =
            aabb1.lowerBound

        u1 =
            aabb1.upperBound
    in
    { lowerBound = vec3 (min l.x l1.x) (min l.y l1.y) (min l.z l1.z)
    , upperBound = vec3 (max u.x u1.x) (max u.y u1.y) (max u.z u1.z)
    }


overlaps : AABB -> AABB -> Bool
overlaps aabb1 aabb2 =
    let
        l1 =
            aabb1.lowerBound

        u1 =
            aabb1.upperBound

        l2 =
            aabb2.lowerBound

        u2 =
            aabb2.upperBound
    in
    ((l2.x <= u1.x && u1.x <= u2.x) || (l1.x <= u2.x && u2.x <= u1.x))
        && ((l2.y <= u1.y && u1.y <= u2.y) || (l1.y <= u2.y && u2.y <= u1.y))
        && ((l2.z <= u1.z && u1.z <= u2.z) || (l1.z <= u2.z && u2.z <= u1.z))


convexPolyhedron : ConvexPolyhedron -> Transform -> AABB
convexPolyhedron { vertices } transform =
    Array.foldl
        (\point ->
            let
                p =
                    Transform.pointToWorldFrame transform point
            in
            extend (AABB p p)
        )
        impossible
        vertices


toHalfExtends : AABB -> Vec3
toHalfExtends { lowerBound, upperBound } =
    lowerBound
        |> Vec3.sub upperBound
        |> Vec3.scale 0.5


type T3
    = T3 Float Float Float


plane : Transform -> AABB
plane { position, quaternion } =
    let
        { x, y, z } =
            Quaternion.rotate quaternion Vec3.k
    in
    if abs x == 1 then
        { maximum | upperBound = vec3 position.x (x * Const.maxNumber) (x * Const.maxNumber) }

    else if abs y == 1 then
        { maximum | lowerBound = vec3 (y * Const.maxNumber) position.y (y * Const.maxNumber) }

    else if abs z == 1 then
        { maximum | lowerBound = vec3 (z * Const.maxNumber) (z * Const.maxNumber) position.z }

    else
        maximum


sphere : Float -> Transform -> AABB
sphere radius { position } =
    let
        c =
            position
    in
    { lowerBound = vec3 (c.x - radius) (c.y - radius) (c.z - radius)
    , upperBound = vec3 (c.x + radius) (c.y + radius) (c.z + radius)
    }
