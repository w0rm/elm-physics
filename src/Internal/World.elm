module Internal.World exposing
    ( Protected(..)
    , World
    , addGravityForces
    , getPairs
    , raycast
    , tick
    )

import Internal.Body as Body exposing (Body)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


type Protected data
    = Protected (World data)


type alias World data =
    { bodies : List (Body data)
    , freeIds : List Int
    , nextBodyId : Int
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
        - Vec3.distance body1.position body2.position
        > 0


raycast :
    { from : Vec3, direction : Vec3 }
    -> World data
    -> Maybe { distance : Float, point : Vec3, normal : Vec3, body : Body data }
raycast ray { bodies } =
    List.foldl
        (\body maybeClosestRaycastResult ->
            case Body.raycast ray body of
                Just raycastResult ->
                    case maybeClosestRaycastResult of
                        Just closestRaycastResult ->
                            if raycastResult.distance - closestRaycastResult.distance < 0 then
                                Just
                                    { body = body
                                    , distance = raycastResult.distance
                                    , normal = raycastResult.normal
                                    , point = raycastResult.point
                                    }

                            else
                                maybeClosestRaycastResult

                        Nothing ->
                            Just
                                { body = body
                                , distance = raycastResult.distance
                                , normal = raycastResult.normal
                                , point = raycastResult.point
                                }

                Nothing ->
                    maybeClosestRaycastResult
        )
        Nothing
        bodies
