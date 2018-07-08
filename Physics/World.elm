module Physics.World
    exposing
        ( World
        , world
        , setGravity
        , addBody
        , tick
        , getPairs
        , addGravityForces
        )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time)
import Physics.Body as Body exposing (BodyId, Body)


type alias World =
    { bodies : Dict BodyId Body
    , nextBodyId : BodyId
    , gravity : Vec3
    , time : Time
    }


world : World
world =
    { bodies = Dict.empty
    , nextBodyId = 0
    , gravity = vec3 0 0 0
    , time = 0
    }


setGravity : Vec3 -> World -> World
setGravity gravity world =
    { world | gravity = gravity }


addGravityForces : World -> World
addGravityForces world =
    { world
        | bodies = Dict.map (\_ -> Body.addGravity world.gravity) world.bodies
    }


tick : Time -> World -> World
tick dt world =
    { world
        | bodies =
            Dict.map
                (\_ -> Body.tick dt >> Body.clearForces)
                world.bodies
        , time = world.time + dt
    }


addBody : Body -> World -> World
addBody body world =
    { world
        | bodies = Dict.insert world.nextBodyId body world.bodies
        , nextBodyId = world.nextBodyId + 1
    }


getPairs : World -> Set ( BodyId, BodyId )
getPairs { bodies } =
    Dict.foldl
        (\id1 body1 acc1 ->
            Dict.foldl
                (\id2 body2 ->
                    if
                        (id1 > id2)
                            && Vec3.distance body1.position body2.position
                            < (body1.boundingSphereRadius + body2.boundingSphereRadius)
                    then
                        Set.insert ( id2, id1 )
                    else
                        identity
                )
                acc1
                bodies
        )
        Set.empty
        bodies
