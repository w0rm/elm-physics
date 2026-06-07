module Internal.Vector3 exposing
    ( Vec3
    , add
    , almostZero
    , basis
    , closestPointsBetweenSegments
    , cross
    , direction
    , distance
    , distanceSquared
    , dot
    , length
    , lengthSquared
    , lerp
    , negate
    , normalize
    , one
    , scale
    , stableTangents
    , sub
    , tangents
    , xAxis
    , xNegative
    , yAxis
    , yNegative
    , zAxis
    , zNegative
    , zero
    )

import Internal.Const as Const


{-| In `tangents`, cross the normal with whichever world axis it is least
aligned to; |n.x| past this switches from x to y to stay well-conditioned.
-}
tangentAxisThreshold : Float
tangentAxisThreshold =
    0.9


almostZero : Vec3 -> Bool
almostZero { x, y, z } =
    (abs x - Const.precision <= 0)
        && (abs y - Const.precision <= 0)
        && (abs z - Const.precision <= 0)


{-| Three dimensional vector type
-}
type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


{-| x, y and z directions
-}
basis : List Vec3
basis =
    [ xAxis, yAxis, zAxis ]


{-| The zero vector
-}
zero : Vec3
zero =
    { x = 0, y = 0, z = 0 }


{-| The unit vector which points in the x direction: `vec3 1 0 0`
-}
xAxis : Vec3
xAxis =
    { x = 1, y = 0, z = 0 }


{-| The unit vector which points in the y direction: `vec3 0 1 0`
-}
yAxis : Vec3
yAxis =
    { x = 0, y = 1, z = 0 }


{-| The unit vector which points in the z direction: `vec3 0 0 1`
-}
zAxis : Vec3
zAxis =
    { x = 0, y = 0, z = 1 }


{-| The unit vector which points in the direction opposite to x: `vec3 -1 0 0`
-}
xNegative : Vec3
xNegative =
    { x = -1, y = 0, z = 0 }


{-| The unit vector which points in the direction opposite to y: `vec3 0 -1 0`
-}
yNegative : Vec3
yNegative =
    { x = 0, y = -1, z = 0 }


{-| The unit vector which points in the direction opposite to z: `vec3 0 0 -1`
-}
zNegative : Vec3
zNegative =
    { x = 0, y = 0, z = -1 }


one : Vec3
one =
    { x = 1, y = 1, z = 1 }


{-| Vector addition: a + b
-}
add : Vec3 -> Vec3 -> Vec3
add a b =
    { x = a.x + b.x, y = a.y + b.y, z = a.z + b.z }


{-| Vector subtraction: a - b
-}
sub : Vec3 -> Vec3 -> Vec3
sub a b =
    { x = a.x - b.x, y = a.y - b.y, z = a.z - b.z }


{-| Vector negation: -a
-}
negate : Vec3 -> Vec3
negate v3 =
    { x = -v3.x, y = -v3.y, z = -v3.z }


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec3 -> Vec3 -> Vec3
direction a b =
    let
        c =
            sub a b

        len =
            length c
    in
    { x = c.x / len, y = c.y / len, z = c.z / len }


{-| The length of the given vector: |a|
-}
length : Vec3 -> Float
length { x, y, z } =
    sqrt (x * x + y * y + z * z)


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec3 -> Float
lengthSquared { x, y, z } =
    x * x + y * y + z * z


{-| The distance between two vectors.
-}
distance : Vec3 -> Vec3 -> Float
distance a b =
    let
        x =
            b.x - a.x

        y =
            b.y - a.y

        z =
            b.z - a.z
    in
    sqrt (x * x + y * y + z * z)


{-| The squared distance between two vectors.
-}
distanceSquared : Vec3 -> Vec3 -> Float
distanceSquared a b =
    let
        x =
            b.x - a.x

        y =
            b.y - a.y

        z =
            b.z - a.z
    in
    x * x + y * y + z * z


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec3 -> Vec3
normalize v3 =
    let
        len =
            length v3
    in
    { x = v3.x / len, y = v3.y / len, z = v3.z / len }


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec3 -> Vec3
scale s v3 =
    { x = s * v3.x, y = s * v3.y, z = s * v3.z }


{-| The dot product of a and b
-}
dot : Vec3 -> Vec3 -> Float
dot a b =
    a.x * b.x + a.y * b.y + a.z * b.z


{-| The cross product of a and b
-}
cross : Vec3 -> Vec3 -> Vec3
cross a b =
    { x = a.y * b.z - a.z * b.y
    , y = a.z * b.x - a.x * b.z
    , z = a.x * b.y - a.y * b.x
    }


{-| Get normalized tangents
-}
tangents : Vec3 -> ( Vec3, Vec3 )
tangents vec =
    if lengthSquared vec > 0 then
        let
            normalized =
                normalize vec

            v =
                normalize
                    (if abs normalized.x - tangentAxisThreshold < 0 then
                        cross normalized xAxis

                     else
                        cross normalized yAxis
                    )
        in
        ( v, cross normalized v )

    else
        ( xAxis, yAxis )


{-| Reuse a previous tangent direction by projecting `cachedT1` onto the plane
perpendicular to `ni` and renormalising; `t2 = ni × t1`. Keeps a friction basis
continuous as the contact normal rotates between steps, avoiding the basis
discontinuity in `tangents`.

Falls back to `tangents` if the cached direction is nearly parallel to the new
normal (degenerate projection).

-}
stableTangents : Vec3 -> Vec3 -> ( Vec3, Vec3 )
stableTangents cachedT1 ni =
    let
        d =
            dot cachedT1 ni

        projected =
            sub cachedT1 (scale d ni)

        lenSq =
            lengthSquared projected
    in
    if lenSq - Const.precision < 0 then
        tangents ni

    else
        let
            t1 =
                scale (1 / sqrt lenSq) projected
        in
        ( t1, cross ni t1 )


lerp : Float -> Vec3 -> Vec3 -> Vec3
lerp t v1 v2 =
    { x = v1.x + t * (v2.x - v1.x)
    , y = v1.y + t * (v2.y - v1.y)
    , z = v1.z + t * (v2.z - v1.z)
    }


{-| Closest points between segments p1-q1 and p2-q2, returned as
`(pointOnFirst, pointOnSecond)`. Adapted from Christer Ericson's
"Real-Time Collision Detection" §5.1.9.
-}
closestPointsBetweenSegments : Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( Vec3, Vec3 )
closestPointsBetweenSegments p1 q1 p2 q2 =
    let
        d1 =
            sub q1 p1

        d2 =
            sub q2 p2

        r =
            sub p1 p2

        a =
            dot d1 d1

        e =
            dot d2 d2

        f =
            dot d2 r

        ( s, t ) =
            if a - Const.precision <= 0 && e - Const.precision <= 0 then
                ( 0, 0 )

            else if a - Const.precision <= 0 then
                ( 0, clamp 0 1 (f / e) )

            else
                let
                    c =
                        dot d1 r
                in
                if e - Const.precision <= 0 then
                    ( clamp 0 1 (-c / a), 0 )

                else
                    let
                        b =
                            dot d1 d2

                        denom =
                            a * e - b * b

                        sInit =
                            if denom /= 0.0 then
                                clamp 0 1 ((b * f - c * e) / denom)

                            else
                                0.0

                        tNom =
                            b * sInit + f
                    in
                    if tNom < 0.0 then
                        ( clamp 0 1 (-c / a), 0 )

                    else if tNom - e > 0 then
                        ( clamp 0 1 ((b - c) / a), 1 )

                    else
                        ( sInit, tNom / e )
    in
    ( add p1 (scale s d1)
    , add p2 (scale t d2)
    )
