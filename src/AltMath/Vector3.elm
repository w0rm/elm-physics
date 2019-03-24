module AltMath.Vector3 exposing
    ( Vec3, vec3, i, j, k
    , getX, getY, getZ, setX, setY, setZ
    , add, sub, negate, scale, dot, cross, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , toRecord, fromRecord
    )

{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All vectors are immutable.


# Create

@docs Vec3, vec3, i, j, k


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, setX, setY, setZ


# Operations

@docs add, sub, negate, scale, dot, cross, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Conversions

@docs toRecord, fromRecord

-}


{-| Three dimensional vector type
-}
type alias Vec3 =
    { x : Float, y : Float, z : Float }


{-| Creates a new 3-element vector with the given values.
-}
vec3 : Float -> Float -> Float -> Vec3
vec3 =
    Vec3


{-| The unit vector &icirc; which points in the x direction: `vec3 1 0 0`
-}
i : Vec3
i =
    Vec3 1 0 0


{-| The unit vector &jcirc; which points in the y direction: `vec3 0 1 0`
-}
j : Vec3
j =
    Vec3 0 1 0


{-| The unit vector k&#0770; which points in the z direction: `vec3 0 0 1`
-}
k : Vec3
k =
    Vec3 0 0 1


{-| Extract the x component of a vector.
-}
getX : Vec3 -> Float
getX =
    .x


{-| Extract the y component of a vector.
-}
getY : Vec3 -> Float
getY =
    .y


{-| Extract the z component of a vector.
-}
getZ : Vec3 -> Float
getZ =
    .z


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec3 -> Vec3
setX x v3 =
    { v3 | x = x }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec3 -> Vec3
setY y v3 =
    { v3 | y = y }


{-| Update the z component of a vector, returning a new vector.
-}
setZ : Float -> Vec3 -> Vec3
setZ z v3 =
    { v3 | z = z }


{-| Convert a vector to a record.
-}
toRecord : Vec3 -> Vec3
toRecord =
    identity


{-| Convert a record to a vector.
-}
fromRecord : Vec3 -> Vec3
fromRecord =
    identity


{-| Vector addition: a + b
-}
add : Vec3 -> Vec3 -> Vec3
add a b =
    Vec3 (a.x + b.x) (a.y + b.y) (a.z + b.z)


{-| Vector subtraction: a - b
-}
sub : Vec3 -> Vec3 -> Vec3
sub a b =
    Vec3 (a.x - b.x) (a.y - b.y) (a.z - b.z)


{-| Vector negation: -a
-}
negate : Vec3 -> Vec3
negate v3 =
    Vec3 -v3.x -v3.y -v3.z


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
    Vec3 (c.x / len) (c.y / len) (c.z / len)


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
    length (sub a b)


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
    Vec3 (v3.x / len) (v3.y / len) (v3.z / len)


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec3 -> Vec3
scale s v3 =
    Vec3 (s * v3.x) (s * v3.y) (s * v3.z)


{-| The dot product of a and b
-}
dot : Vec3 -> Vec3 -> Float
dot a b =
    a.x * b.x + a.y * b.y + a.z * b.z


{-| The cross product of a and b
-}
cross : Vec3 -> Vec3 -> Vec3
cross a b =
    Vec3
        (a.y * b.z - a.z * b.y)
        (a.z * b.x - a.x * b.z)
        (a.x * b.y - a.y * b.x)
