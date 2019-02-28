module AltMath.Vector4 exposing
    ( Vec4, vec4
    , getX, getY, getZ, getW, setX, setY, setZ, setW
    , add, sub, negate, scale, dot, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , toRecord, fromRecord
    )

{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All vectors are immutable.


# Create

@docs Vec4, vec4


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, getW, setX, setY, setZ, setW


# Operations

@docs add, sub, negate, scale, dot, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Conversions

@docs toRecord, fromRecord

-}


{-| Four dimensional vector type
-}
type alias Vec4 =
    { x : Float, y : Float, z : Float, w : Float }


{-| Creates a new 4-element vector with the given x, y, z, and w values.
-}
vec4 : Float -> Float -> Float -> Float -> Vec4
vec4 =
    Vec4


{-| Extract the x component of a vector.
-}
getX : Vec4 -> Float
getX =
    .x


{-| Extract the y component of a vector.
-}
getY : Vec4 -> Float
getY =
    .y


{-| Extract the z component of a vector.
-}
getZ : Vec4 -> Float
getZ =
    .z


{-| Extract the w component of a vector.
-}
getW : Vec4 -> Float
getW =
    .w


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec4 -> Vec4
setX x v4 =
    { v4 | x = x }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec4 -> Vec4
setY y v4 =
    { v4 | y = y }


{-| Update the z component of a vector, returning a new vector.
-}
setZ : Float -> Vec4 -> Vec4
setZ z v4 =
    { v4 | z = z }


{-| Update the w component of a vector, returning a new vector.
-}
setW : Float -> Vec4 -> Vec4
setW w v4 =
    { v4 | w = w }


{-| Convert a vector to a record.
-}
toRecord : Vec4 -> Vec4
toRecord =
    identity


{-| Convert a record to a vector.
-}
fromRecord : Vec4 -> Vec4
fromRecord =
    identity


{-| Vector addition: a + b
-}
add : Vec4 -> Vec4 -> Vec4
add a b =
    Vec4 (a.x + b.x) (a.y + b.y) (a.z + b.z) (a.w + b.w)


{-| Vector subtraction: a - b
-}
sub : Vec4 -> Vec4 -> Vec4
sub a b =
    Vec4 (a.x - b.x) (a.y - b.y) (a.z - b.z) (a.w - b.w)


{-| Vector negation: -a
-}
negate : Vec4 -> Vec4
negate a =
    Vec4 -a.x -a.y -a.z -a.w


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec4 -> Vec4 -> Vec4
direction a b =
    let
        c =
            sub a b

        len =
            length c
    in
    Vec4 (c.x / len) (c.y / len) (c.z / len) (c.w / len)


{-| The length of the given vector: |a|
-}
length : Vec4 -> Float
length { x, y, z, w } =
    sqrt (x * x + y * y + z * z + w * w)


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec4 -> Float
lengthSquared { x, y, z, w } =
    x * x + y * y + z * z + w * w


{-| The distance between two vectors.
-}
distance : Vec4 -> Vec4 -> Float
distance a b =
    length (sub a b)


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec4 -> Vec4 -> Float
distanceSquared a b =
    lengthSquared (sub a b)


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec4 -> Vec4
normalize v4 =
    let
        len =
            length v4
    in
    Vec4 (v4.x / len) (v4.y / len) (v4.z / len) (v4.w / len)


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec4 -> Vec4
scale s v =
    Vec4 (s * v.x) (s * v.y) (s * v.z) (s * v.w)


{-| The dot product of a and b
-}
dot : Vec4 -> Vec4 -> Float
dot a b =
    a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
