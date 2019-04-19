module Internal.World exposing
    ( Protected(..)
    , World
    , addGravityForces
    , getPairs
    , tick
    )

import Internal.Body as Body exposing (Body, BodyId)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


type Protected data
    = Protected (World data)


type alias World data =
    { bodies : List (Body data)
    , freeIds : List BodyId
    , nextBodyId : BodyId
    , gravity : Vec3
    }


addGravityForces : World data -> World data
addGravityForces world =
    { world
        | bodies = List.map (Body.addGravity world.gravity) world.bodies
    }


tick : Float -> World data -> World data
tick dt world =
    { world
        | bodies = List.map (Body.tick dt) world.bodies
    }


getPairs : World data -> List ( Body data, Body data )
getPairs { bodies } =
    getPairsHelp bodies []


getPairsHelp : List (Body data) -> List ( Body data, Body data ) -> List ( Body data, Body data )
getPairsHelp list result =
    case list of
        body1 :: rest ->
            getPairsHelp rest
                (List.foldl
                    (\body2 ->
                        if bodiesMayOverlap body1 body2 then
                            (::) ( body1, body2 )

                        else
                            identity
                    )
                    result
                    rest
                )

        [] ->
            result


bodiesMayOverlap : Body data -> Body data -> Bool
bodiesMayOverlap body1 body2 =
    (body1.boundingSphereRadius + body2.boundingSphereRadius)
        > Vec3.distance body1.position body2.position
