module Internal.AssignIds exposing (assignIds)

import Internal.Body as InternalBody


assignIds : List ( id, InternalBody.Protected ) -> ( List ( id, InternalBody.Body ), Int )
assignIds bodiesWithIds =
    let
        ( existingIds, newCount, mx ) =
            collect bodiesWithIds [] 0 -1

        sorted =
            List.sort existingIds

        ( dupIds, dupCount ) =
            findDups sorted -2 [] 0

        freeIds =
            findFree 0 sorted (newCount + dupCount) []
    in
    case freeIds of
        [] ->
            stable bodiesWithIds mx []

        _ ->
            assign bodiesWithIds freeIds dupIds -1 mx []


collect : List ( id, InternalBody.Protected ) -> List Int -> Int -> Int -> ( List Int, Int, Int )
collect bodies existingIds newCount mx =
    case bodies of
        [] ->
            ( existingIds, newCount, mx )

        ( _, InternalBody.Protected body ) :: rest ->
            if body.id == -1 then
                collect rest existingIds (newCount + 1) mx

            else
                collect rest (body.id :: existingIds) newCount (max mx body.id)


findDups : List Int -> Int -> List Int -> Int -> ( List Int, Int )
findDups sorted prev acc count =
    case sorted of
        [] ->
            ( acc, count )

        x :: rest ->
            if x - prev == 0 then
                findDups rest x (x :: acc) (count + 1)

            else
                findDups rest x acc count


findFree : Int -> List Int -> Int -> List Int -> List Int
findFree n sorted needed revAcc =
    if needed == 0 then
        List.reverse revAcc

    else
        case sorted of
            [] ->
                fillFrom n needed revAcc

            x :: rest ->
                if x > n then
                    findFree (n + 1) sorted (needed - 1) (n :: revAcc)

                else if x == n then
                    findFree (n + 1) rest needed revAcc

                else
                    findFree n rest needed revAcc


fillFrom : Int -> Int -> List Int -> List Int
fillFrom n needed revAcc =
    if needed == 0 then
        List.reverse revAcc

    else
        fillFrom (n + 1) (needed - 1) (n :: revAcc)


assign : List ( id, InternalBody.Protected ) -> List Int -> List Int -> Int -> Int -> List ( id, InternalBody.Body ) -> ( List ( id, InternalBody.Body ), Int )
assign bodies freeIds dupIds dir mx acc =
    case freeIds of
        [] ->
            stable bodies mx acc

        freshId :: remainingFree ->
            case bodies of
                [] ->
                    ( acc, mx )

                ( extId, InternalBody.Protected body ) :: rest ->
                    if body.id == -1 then
                        assign rest
                            remainingFree
                            dupIds
                            dir
                            (max mx freshId)
                            (( extId, withId freshId body ) :: acc)

                    else if memberSorted dir body.id dupIds then
                        case removeFirstReversing body.id [] dupIds of
                            [] ->
                                assign rest
                                    remainingFree
                                    []
                                    dir
                                    (max mx freshId)
                                    (( extId, withId freshId body ) :: acc)

                            newDupIds ->
                                assign rest
                                    remainingFree
                                    newDupIds
                                    (negate dir)
                                    (max mx freshId)
                                    (( extId, withId freshId body ) :: acc)

                    else
                        assign rest freeIds dupIds dir mx (( extId, body ) :: acc)


stable : List ( id, InternalBody.Protected ) -> Int -> List ( id, InternalBody.Body ) -> ( List ( id, InternalBody.Body ), Int )
stable bodies mx acc =
    case bodies of
        [] ->
            ( acc, mx )

        ( extId, InternalBody.Protected body ) :: rest ->
            stable rest mx (( extId, body ) :: acc)


withId : Int -> InternalBody.Body -> InternalBody.Body
withId freshId body =
    { id = freshId
    , transform3d = body.transform3d
    , centerOfMassTransform3d = body.centerOfMassTransform3d
    , velocity = body.velocity
    , angularVelocity = body.angularVelocity
    , mass = body.mass
    , volume = body.volume
    , shapesWithMaterials = body.shapesWithMaterials
    , worldShapesWithMaterials = body.worldShapesWithMaterials
    , force = body.force
    , torque = body.torque
    , boundingSphereRadius = body.boundingSphereRadius
    , linearDamping = body.linearDamping
    , angularDamping = body.angularDamping
    , invMass = body.invMass
    , invInertia = body.invInertia
    , invInertiaWorld = body.invInertiaWorld
    }


memberSorted : Int -> Int -> List Int -> Bool
memberSorted dir x list =
    case list of
        [] ->
            False

        y :: rest ->
            if y - x == 0 then
                True

            else if dir * (y - x) > 0 then
                False

            else
                memberSorted dir x rest


removeFirstReversing : Int -> List Int -> List Int -> List Int
removeFirstReversing x acc remaining =
    case remaining of
        [] ->
            acc

        y :: rest ->
            if y == x then
                accumulateReversed acc rest

            else
                removeFirstReversing x (y :: acc) rest


accumulateReversed : List Int -> List Int -> List Int
accumulateReversed acc remaining =
    case remaining of
        [] ->
            acc

        y :: rest ->
            accumulateReversed (y :: acc) rest
