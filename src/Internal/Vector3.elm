module Internal.Vector3 exposing
    ( Vec3
    , add
    , almostZero
    , basis
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
    , scale
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


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec3 -> Vec3 -> Float
distanceSquared a b =
    lengthSquared (sub a b)


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
                if abs normalized.x < 0.9 then
                    cross normalized xAxis

                else
                    cross normalized yAxis
        in
        ( v, cross normalized v )

    else
        ( xAxis, yAxis )


lerp : Float -> Vec3 -> Vec3 -> Vec3
lerp t v1 v2 =
    { x = v1.x + t * (v2.x - v1.x)
    , y = v1.y + t * (v2.y - v1.y)
    , z = v1.z + t * (v2.z - v1.z)
    }
