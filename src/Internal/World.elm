module Internal.World exposing
    ( Protected(..)
    , World
    , addGravityForces
    , getPairs
    , raycast
    )

import Internal.Body as Body exposing (Body)
import Internal.Constraint as Constraint exposing (ConstraintGroup)
import Internal.Vector3 as Vec3 exposing (Vec3)


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


getPairs : World data -> List ( Body data, Body data )
getPairs { bodies } =
    case bodies of
        body :: restBodies ->
            getPairsHelp body restBodies restBodies []

        [] ->
            []


getPairsHelp : Body data -> List (Body data) -> List (Body data) -> List ( Body data, Body data ) -> List ( Body data, Body data )
getPairsHelp body1 currentBodies restBodies result =
    case restBodies of
        body2 :: newRestBodies ->
            getPairsHelp
                body1
                currentBodies
                newRestBodies
                (if bodiesMayOverlap body1 body2 then
                    ( body1, body2 ) :: result

                 else
                    result
                )

        [] ->
            case currentBodies of
                newBody1 :: newRestBodies ->
                    getPairsHelp
                        newBody1
                        newRestBodies
                        newRestBodies
                        result

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
