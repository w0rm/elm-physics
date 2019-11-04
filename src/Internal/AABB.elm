module Internal.AABB exposing
    ( AABB
    , convex
    , dimensions
    , extend
    , impossible
    , particle
    , plane
    , sphere
    )

import Direction3d
import Frame3d exposing (Frame3d)
import Internal.Const as Const
import Internal.Convex exposing (Convex)
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Length exposing (Meters)
import Physics.Coordinates exposing (ShapeCoordinates)
import Point3d


type alias AABB =
    { upperBound : Vec3
    , lowerBound : Vec3
    }


maximum : AABB
maximum =
    { lowerBound =
        { x = -Const.maxNumber
        , y = -Const.maxNumber
        , z = -Const.maxNumber
        }
    , upperBound =
        { x = Const.maxNumber
        , y = Const.maxNumber
        , z = Const.maxNumber
        }
    }


impossible : AABB
impossible =
    { lowerBound =
        { x = Const.maxNumber
        , y = Const.maxNumber
        , z = Const.maxNumber
        }
    , upperBound =
        { x = -Const.maxNumber
        , y = -Const.maxNumber
        , z = -Const.maxNumber
        }
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
    { lowerBound =
        { x = min l.x l1.x
        , y = min l.y l1.y
        , z = min l.z l1.z
        }
    , upperBound =
        { x = max u.x u1.x
        , y = max u.y u1.y
        , z = max u.z u1.z
        }
    }


convex : Convex -> Frame3d Meters CenterOfMassCoordinates { defines : ShapeCoordinates } -> AABB
convex { vertices } frame3d =
    List.foldl
        (\point ->
            let
                p =
                    Point3d.toMeters (Point3d.placeIn frame3d (Point3d.fromMeters point))
            in
            extend { lowerBound = p, upperBound = p }
        )
        impossible
        vertices


dimensions : AABB -> Vec3
dimensions { lowerBound, upperBound } =
    Vec3.sub upperBound lowerBound


plane : Frame3d Meters CenterOfMassCoordinates { defines : ShapeCoordinates } -> AABB
plane frame3d =
    let
        { x, y, z } =
            Direction3d.unwrap (Direction3d.placeIn frame3d (Direction3d.unsafe Vec3.k))

        position =
            Point3d.toMeters (Frame3d.originPoint frame3d)
    in
    if abs x == 1 then
        { lowerBound = maximum.lowerBound
        , upperBound =
            { x = position.x
            , y = x * Const.maxNumber
            , z = x * Const.maxNumber
            }
        }

    else if abs y == 1 then
        { upperBound = maximum.upperBound
        , lowerBound =
            { x = y * Const.maxNumber
            , y = position.y
            , z = y * Const.maxNumber
            }
        }

    else if abs z == 1 then
        { upperBound = maximum.upperBound
        , lowerBound =
            { x = z * Const.maxNumber
            , y = z * Const.maxNumber
            , z = position.z
            }
        }

    else
        maximum


particle : Frame3d Meters CenterOfMassCoordinates { defines : ShapeCoordinates } -> AABB
particle frame3d =
    let
        position =
            Point3d.toMeters (Frame3d.originPoint frame3d)
    in
    { upperBound = position
    , lowerBound = position
    }


sphere : Float -> Frame3d Meters CenterOfMassCoordinates { defines : ShapeCoordinates } -> AABB
sphere radius frame3d =
    let
        c =
            Point3d.toMeters (Frame3d.originPoint frame3d)
    in
    { lowerBound =
        { x = c.x - radius
        , y = c.y - radius
        , z = c.z - radius
        }
    , upperBound =
        { x = c.x + radius
        , y = c.y + radius
        , z = c.z + radius
        }
    }
