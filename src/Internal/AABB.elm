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

import Internal.Const as Const
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex exposing (Convex)
import Shapes.Plane exposing (Plane)
import Shapes.Sphere exposing (Sphere)


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


convex : Convex -> AABB
convex { vertices } =
    List.foldl
        (\point ->
            extend { lowerBound = point, upperBound = point }
        )
        impossible
        vertices


dimensions : AABB -> Vec3
dimensions { lowerBound, upperBound } =
    Vec3.sub upperBound lowerBound


plane : Plane -> AABB
plane { normal, position } =
    let
        { x, y, z } =
            normal
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


particle : Vec3 -> AABB
particle position =
    { upperBound = position
    , lowerBound = position
    }


sphere : Sphere -> AABB
sphere { position, radius } =
    { lowerBound =
        { x = position.x - radius
        , y = position.y - radius
        , z = position.z - radius
        }
    , upperBound =
        { x = position.x + radius
        , y = position.y + radius
        , z = position.z + radius
        }
    }
