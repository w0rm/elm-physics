module Internal.World exposing
    ( Protected(..)
    , World
    , addGravityForces
    , empty
    , getPairs
    , tick
    )

import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Dict exposing (Dict)
import Internal.Body as Body exposing (Body, BodyId)
import Set exposing (Set)


type Protected data
    = Protected (World data)


type alias World data =
    { bodies : List (Body data)
    , nextBodyId : BodyId
    , gravity : Vec3
    }


empty : World data
empty =
    { bodies = []
    , nextBodyId = 0
    , gravity = vec3 0 0 0
    }


addGravityForces : World data -> World data
addGravityForces world_ =
    { world_
        | bodies = List.map (Body.addGravity world_.gravity) world_.bodies
    }


tick : Float -> World data -> World data
tick dt world_ =
    { world_
        | bodies =
            List.map
                (Body.tick dt >> Body.clearForces)
                world_.bodies
    }


getPairs : World data -> List ( Body data, Body data )
getPairs { bodies } =
    List.foldl
        (\body1 acc1 ->
            List.foldl
                (\body2 ->
                    if
                        (body1.id > body2.id)
                            && Vec3.distance body1.position body2.position
                            < (body1.boundingSphereRadius + body2.boundingSphereRadius)
                    then
                        (::) ( body2, body1 )

                    else
                        identity
                )
                acc1
                bodies
        )
        []
        bodies
