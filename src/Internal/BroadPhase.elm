module Internal.BroadPhase exposing (getContacts)

{-| This is very naive implementation of BroadPhase,
that checks if the bounding spheres of each two bodies overlap
-}

import Internal.Body exposing (Body)
import Internal.Contact exposing (ContactGroup)
import Internal.NarrowPhase as NarrowPhase
import Internal.Transform3d as Transform3d


getContacts : (id -> id -> Bool) -> List ( id, Body ) -> List ContactGroup
getContacts collide bodies =
    case bodies of
        ( id1, body1 ) :: restBodies ->
            getContactsHelp collide id1 body1 restBodies restBodies []

        [] ->
            []


{-| This will generate all pairs for body1, then all pairs for body2, etc.
We rely on this order in the Solver.elm
-}
getContactsHelp : (id -> id -> Bool) -> id -> Body -> List ( id, Body ) -> List ( id, Body ) -> List ContactGroup -> List ContactGroup
getContactsHelp collide id1 body1 currentBodies restBodies result =
    case restBodies of
        ( id2, body2 ) :: newRestBodies ->
            getContactsHelp
                collide
                id1
                body1
                currentBodies
                newRestBodies
                (if bodiesMayContact collide id1 body1 id2 body2 then
                    case
                        NarrowPhase.getContacts
                            body1.worldShapesWithMaterials
                            body2.worldShapesWithMaterials
                    of
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
                ( newId1, newBody1 ) :: newRestBodies ->
                    getContactsHelp
                        collide
                        newId1
                        newBody1
                        newRestBodies
                        newRestBodies
                        result

                [] ->
                    result


bodiesMayContact : (id -> id -> Bool) -> id -> Body -> id -> Body -> Bool
bodiesMayContact collide id1 body1 id2 body2 =
    let
        boundingRadiuses =
            body1.boundingSphereRadius + body2.boundingSphereRadius

        p1 =
            Transform3d.originPoint body1.transform3d

        p2 =
            Transform3d.originPoint body2.transform3d

        dx =
            p2.x - p1.x

        dy =
            p2.y - p1.y

        dz =
            p2.z - p1.z

        distanceSquared =
            dx * dx + dy * dy + dz * dz
    in
    (boundingRadiuses * boundingRadiuses - distanceSquared > 0)
        && (body1.mass + body2.mass /= 0)
        && collide id1 id2
