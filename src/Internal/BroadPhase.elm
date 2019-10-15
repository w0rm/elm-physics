module Internal.BroadPhase exposing (getContacts)

{-| This is very naive implementation of BroadPhase,
that checks if the bounding spheres of each two bodies overlap
-}

import Frame3d
import Internal.Body exposing (Body)
import Internal.Contact exposing (ContactGroup)
import Internal.NarrowPhase as NarrowPhase
import Internal.World exposing (World)
import Length
import Point3d


getContacts : World data -> List (ContactGroup data)
getContacts { bodies } =
    case bodies of
        body :: restBodies ->
            getContactsHelp body restBodies restBodies []

        [] ->
            []


getContactsHelp : Body data -> List (Body data) -> List (Body data) -> List (ContactGroup data) -> List (ContactGroup data)
getContactsHelp body1 currentBodies restBodies result =
    case restBodies of
        body2 :: newRestBodies ->
            getContactsHelp
                body1
                currentBodies
                newRestBodies
                (if bodiesMayOverlap body1 body2 then
                    case NarrowPhase.getContacts body1 body2 of
                        [] ->
                            result

                        contacts ->
                            { body1 = body1
                            , body2 = body2
                            , contacts = contacts
                            }
                                :: result

                 else
                    result
                )

        [] ->
            case currentBodies of
                newBody1 :: newRestBodies ->
                    getContactsHelp
                        newBody1
                        newRestBodies
                        newRestBodies
                        result

                [] ->
                    result


bodiesMayOverlap : Body data -> Body data -> Bool
bodiesMayOverlap body1 body2 =
    (body1.boundingSphereRadius + body2.boundingSphereRadius)
        - Length.inMeters (Point3d.distanceFrom (Frame3d.originPoint body1.frame3d) (Frame3d.originPoint body2.frame3d))
        > 0
