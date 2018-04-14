module Physics.World exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Dict exposing (Dict)
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


addBody : Body -> World -> World
addBody body world =
    { world
        | bodies = Dict.insert world.nextBodyId body world.bodies
        , nextBodyId = world.nextBodyId + 1
    }


step : Time -> World -> World
step dt world =
    let
        newWorld =
            internalStep dt world
    in
        { newWorld | time = newWorld.time + dt }


internalStep : Time -> World -> World
internalStep dt =
    identity
