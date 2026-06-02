module Internal.BroadPhase exposing (getPairs)

{-| Walks the gravity-sorted body list pairwise. For each pair, runs narrow
phase and the user `constrain` callback. Emits a `PairGroup` whenever either
contacts or constraints are non-empty, in CSR order over gravity-sort
positions.
-}

import Internal.Body exposing (Body)
import Internal.Constraint as Constraint exposing (Constraint)
import Internal.Contact exposing (PairGroup)
import Internal.Coordinates exposing (BodyCoordinates)
import Internal.NarrowPhase as NarrowPhase
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Transform3d as Transform3d


getPairs :
    (id -> id -> Bool)
    -> (id -> Maybe (id -> List (Constraint BodyCoordinates)))
    -> List ( id, Body )
    -> List PairGroup
getPairs collide constrain bodies =
    case bodies of
        ( id1, body1 ) :: restBodies ->
            -- One-time scan: if no body registered a constrain callback, skip
            -- the per-pair reverse-direction constraint lookup entirely
            -- (saves N²/2 calls into user's `constrain` for contact-only scenes).
            let
                anyConstraints =
                    hasAnyConstraints constrain bodies
            in
            getPairsHelp collide constrain anyConstraints id1 body1 (constrain id1) restBodies restBodies []

        [] ->
            []


hasAnyConstraints : (id -> Maybe (id -> List (Constraint BodyCoordinates))) -> List ( id, Body ) -> Bool
hasAnyConstraints constrain bodies =
    case bodies of
        [] ->
            False

        ( extId, _ ) :: rest ->
            case constrain extId of
                Just _ ->
                    True

                Nothing ->
                    hasAnyConstraints constrain rest


getPairsHelp :
    (id -> id -> Bool)
    -> (id -> Maybe (id -> List (Constraint BodyCoordinates)))
    -> Bool
    -> id
    -> Body
    -> Maybe (id -> List (Constraint BodyCoordinates))
    -> List ( id, Body )
    -> List ( id, Body )
    -> List PairGroup
    -> List PairGroup
getPairsHelp collide constrain anyConstraints id1 body1 constrainFn1 currentBodies restBodies result =
    case restBodies of
        ( id2, body2 ) :: newRestBodies ->
            let
                contacts =
                    if bodiesMayContact collide id1 body1 id2 body2 then
                        -- The contact id carries only the shape pair; the body
                        -- pair is the warm-start cache key, derived from body ids
                        -- in the solver, so nothing is threaded here.
                        NarrowPhase.getContacts
                            body1.worldShapesWithMaterials
                            body2.worldShapesWithMaterials

                    else
                        []

                constraints =
                    if anyConstraints then
                        constraintsBetween constrain constrainFn1 id1 body1 id2 body2

                    else
                        []

                newResult =
                    case contacts of
                        [] ->
                            case constraints of
                                [] ->
                                    result

                                _ ->
                                    { body1 = body1
                                    , body2 = body2
                                    , contacts = contacts
                                    , constraints = constraints
                                    }
                                        :: result

                        _ ->
                            { body1 = body1
                            , body2 = body2
                            , contacts = contacts
                            , constraints = constraints
                            }
                                :: result
            in
            getPairsHelp collide
                constrain
                anyConstraints
                id1
                body1
                constrainFn1
                currentBodies
                newRestBodies
                newResult

        [] ->
            case currentBodies of
                ( newId1, newBody1 ) :: newRestBodies ->
                    getPairsHelp collide
                        constrain
                        anyConstraints
                        newId1
                        newBody1
                        (constrain newId1)
                        newRestBodies
                        newRestBodies
                        result

                [] ->
                    result


{-| User can register the constrain callback on either body of a pair.
We try (body1 → body2) first; if empty, try (body2 → body1) and `flip` the
constraint so its body1 slot still refers to PairGroup's body1 (the
smaller-position body). Avoids duplicates because we only walk each pair
once (j > i in the outer/inner iteration).
-}
constraintsBetween :
    (id -> Maybe (id -> List (Constraint BodyCoordinates)))
    -> Maybe (id -> List (Constraint BodyCoordinates))
    -> id
    -> Body
    -> id
    -> Body
    -> List (Constraint CenterOfMassCoordinates)
constraintsBetween constrain constrainFn1 id1 body1 id2 body2 =
    if body1.kindInt /= 2 && body2.kindInt /= 2 then
        []

    else
        let
            forward =
                case constrainFn1 of
                    Just fn ->
                        fn id2

                    Nothing ->
                        []
        in
        case forward of
            _ :: _ ->
                List.map
                    (Constraint.relativeToCenterOfMass
                        body1.centerOfMassTransform3d
                        body2.centerOfMassTransform3d
                    )
                    forward

            [] ->
                case constrain id2 of
                    Just fn ->
                        fn id1
                            |> List.map
                                (Constraint.relativeToCenterOfMassFlipped
                                    body1.centerOfMassTransform3d
                                    body2.centerOfMassTransform3d
                                )

                    Nothing ->
                        []


bodiesMayContact : (id -> id -> Bool) -> id -> Body -> id -> Body -> Bool
bodiesMayContact collide id1 body1 id2 body2 =
    let
        boundingRadiuses =
            body1.geometry.boundingSphereRadius + body2.geometry.boundingSphereRadius

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
        && (body1.kindInt == 2 || body2.kindInt == 2)
        && collide id1 id2
