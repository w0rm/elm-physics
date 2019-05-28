module Internal.AABB exposing
    ( AABB
    , convex
    , dimensions
    , extend
    , impossible
    , plane
    , sphere
    )

import Internal.Const as Const
import Internal.Convex as Convex exposing (Convex)
import Internal.Quaternion as Quaternion
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias AABB =
    { upperBound : Vec3
    , lowerBound : Vec3
    }


zero : AABB
zero =
    { lowerBound = Vec3.zero
    , upperBound = Vec3.zero
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


convex : Convex -> Transform -> AABB
convex { vertices } transform =
    List.foldl
        (\point ->
            let
                p =
                    Transform.pointToWorldFrame transform point
            in
            extend { lowerBound = p, upperBound = p }
        )
        impossible
        vertices


dimensions : AABB -> Vec3
dimensions { lowerBound, upperBound } =
    Vec3.sub upperBound lowerBound


plane : Transform -> AABB
plane { position, orientation } =
    let
        { x, y, z } =
            Quaternion.rotate orientation Vec3.k
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


sphere : Float -> Transform -> AABB
sphere radius { position } =
    let
        c =
            position
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
