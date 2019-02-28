module AltMath.Vector2 exposing
    ( Vec2, vec2
    , getX, getY, setX, setY
    , add, sub, negate, scale, dot, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , toRecord, fromRecord
    )

{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All vectors are immutable.


# Create

@docs Vec2, vec2


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY


# Operations

@docs add, sub, negate, scale, dot, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Conversions

@docs toRecord, fromRecord

-}


{-| Two dimensional vector type
-}
type alias Vec2 =
    { x : Float, y : Float }


{-| Creates a new 2-element vector with the given values.
-}
vec2 : Float -> Float -> Vec2
vec2 =
    Vec2


{-| Extract the x component of a vector.
-}
getX : Vec2 -> Float
getX =
    .x


{-| Extract the y component of a vector.
-}
getY : Vec2 -> Float
getY =
    .y


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec2 -> Vec2
setX x v2 =
    { v2 | x = x }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec2 -> Vec2
setY y v2 =
    { v2 | y = y }


{-| Convert a vector to a record.
-}
toRecord : Vec2 -> Vec2
toRecord =
    identity


{-| Convert a record to a vector.
-}
fromRecord : Vec2 -> Vec2
fromRecord =
    identity


{-| Vector addition: a + b
-}
add : Vec2 -> Vec2 -> Vec2
add a b =
    Vec2 (a.x + b.x) (a.y + b.y)


{-| Vector subtraction: a - b
-}
sub : Vec2 -> Vec2 -> Vec2
sub a b =
    Vec2 (a.x - b.x) (a.y - b.y)


{-| Vector negation: -a
-}
negate : Vec2 -> Vec2
negate a =
    Vec2 -a.x -a.y


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec2 -> Vec2 -> Vec2
direction a b =
    let
        c =
            sub a b

        len =
            length c
    in
    Vec2 (c.x / len) (c.y / len)


{-| The length of the given vector: |a|
-}
length : Vec2 -> Float
length { x, y } =
    sqrt (x * x + y * y)


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec2 -> Float
lengthSquared { x, y } =
    x * x + y * y


{-| The distance between two vectors.
-}
distance : Vec2 -> Vec2 -> Float
distance a b =
    length (sub a b)


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared a b =
    lengthSquared (sub a b)


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec2 -> Vec2
normalize v2 =
    let
        len =
            length v2
    in
    Vec2 (v2.x / len) (v2.y / len)


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec2 -> Vec2
scale s v2 =
    Vec2 (s * v2.x) (s * v2.y)


{-| The dot product of a and b
-}
dot : Vec2 -> Vec2 -> Float
dot a b =
    a.x * b.x + a.y * b.y
