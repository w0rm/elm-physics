module Internal.World exposing
    ( Protected(..)
    , World
    , addGravityForces
    , raycast
    )

import Internal.Body as Body exposing (Body)
import Internal.Constraint exposing (ConstraintGroup)
import Internal.Vector3 exposing (Vec3)


type Protected data
    = Protected (World data)


type alias World data =
    { bodies : List (Body data)
    , constraints : List ConstraintGroup
    , freeIds : List Int
    , nextBodyId : Int
    , gravity : Vec3
    }


addGravityForces : World data -> World data
addGravityForces world =
    { world
        | bodies = List.map (Body.addGravity world.gravity) world.bodies
    }


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
