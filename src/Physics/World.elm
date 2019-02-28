module Physics.World exposing
    ( World
    , addBody
    , addGravityForces
    , getNextBodyId
    , getPairs
    , setGravity
    , tick
    , world
    )

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Body as Body exposing (Body, BodyId)
import Dict exposing (Dict)
import Set exposing (Set)


type alias World =
    { bodies : Dict BodyId Body
    , nextBodyId : BodyId
    , gravity : Vec3
    , time : Float
    }


world : World
world =
    { bodies = Dict.empty
    , nextBodyId = 0
    , gravity = vec3 0 0 0
    , time = 0
    }


setGravity : Vec3 -> World -> World
setGravity gravity world_ =
    { world_ | gravity = gravity }


addGravityForces : World -> World
addGravityForces world_ =
    { world_
        | bodies = Dict.map (\_ -> Body.addGravity world_.gravity) world_.bodies
    }


tick : Float -> World -> World
tick dt world_ =
    { world_
        | bodies =
            Dict.map
                (\_ -> Body.tick dt >> Body.clearForces)
                world_.bodies
        , time = world_.time + dt
    }


{-| Predict the body id of the next body to be added
-}
getNextBodyId : World -> BodyId
getNextBodyId =
    .nextBodyId


addBody : Body -> World -> World
addBody body world_ =
    { world_
        | bodies = Dict.insert world_.nextBodyId body world_.bodies
        , nextBodyId = world_.nextBodyId + 1
    }


getPairs : World -> List ( BodyId, BodyId )
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
                        (::) ( id2, id1 )

                    else
                        identity
                )
                acc1
                bodies
        )
        []
        bodies
