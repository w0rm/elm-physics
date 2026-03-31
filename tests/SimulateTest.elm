module SimulateTest exposing (assignIds)

{-| Tests for the ID assignment logic in Physics.simulate.

Bodies carry an internal id field. New bodies start with id = -1.
On each simulate call:

  - bodies with id = -1 are assigned fresh IDs, filling gaps first
  - bodies with existing IDs keep them unchanged
  - the order of bodies in the output matches the input

-}

import Expect
import Internal.Body as InternalBody
import Length exposing (Meters)
import Physics exposing (onEarth)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.Material as Material
import Point3d exposing (Point3d)
import Test exposing (Test, describe, test)
import Vector3d


{-| Run one simulation step with no forces, just to trigger ID assignment.
-}
step : List ( id, Physics.Body ) -> ( List ( id, Physics.Body ), List ( id, id, List (Point3d Meters WorldCoordinates) ) )
step bodies =
    Physics.simulate
        { onEarth | gravity = Vector3d.zero }
        bodies


{-| Extract both the external (user) id and the internal body id from a body entry.
-}
ids : ( id, Physics.Body ) -> ( id, Int )
ids ( extId, InternalBody.Protected body ) =
    ( extId, body.id )


{-| Manually set the internal id on a body, to simulate a body that
has already been through a simulation step.
-}
withInternalId : Int -> Physics.Body -> Physics.Body
withInternalId newId (InternalBody.Protected body) =
    InternalBody.Protected { body | id = newId }


assignIds : Test
assignIds =
    describe "Physics.simulate ID assignment"
        [ test "empty input produces empty output" <|
            \_ ->
                let
                    ( result, _ ) =
                        step []
                in
                Expect.equal [] (List.map ids result)
        , test "single new body: gets internal id 0, external id preserved, order kept" <|
            \_ ->
                let
                    ( result, _ ) =
                        step [ ( "a", Physics.plane Material.wood ) ]
                in
                Expect.equal [ ( "a", 0 ) ] (List.map ids result)
        , test "multiple new bodies: get consecutive internal ids, external ids and order preserved" <|
            \_ ->
                let
                    ( result, _ ) =
                        step
                            [ ( "a", Physics.plane Material.wood )
                            , ( "b", Physics.plane Material.wood )
                            , ( "c", Physics.plane Material.wood )
                            ]
                in
                -- foldl assigns ids left-to-right: "a"->0, "b"->1, "c"->2
                Expect.equal
                    [ ( "a", 0 ), ( "b", 1 ), ( "c", 2 ) ]
                    (List.map ids result)
        , test "internal and external ids are preserved across simulation steps" <|
            \_ ->
                let
                    ( step1, _ ) =
                        step
                            [ ( "a", Physics.plane Material.wood )
                            , ( "b", Physics.plane Material.wood )
                            ]

                    ( step2, _ ) =
                        step step1
                in
                Expect.equal (List.map ids step1) (List.map ids step2)
        , test "new body added to existing sim: existing ids unchanged, new body gets gap id, order kept" <|
            \_ ->
                let
                    ( step1, _ ) =
                        step
                            [ ( "a", Physics.plane Material.wood )
                            , ( "b", Physics.plane Material.wood )
                            ]

                    -- step1 produces: [("a", 0), ("b", 1)]
                    -- add "c" at the end; next available id is 2
                    ( step2, _ ) =
                        step (step1 ++ [ ( "c", Physics.plane Material.wood ) ])
                in
                Expect.equal
                    [ ( "a", 0 ), ( "b", 1 ), ( "c", 2 ) ]
                    (List.map ids step2)
        , test "new body fills gap left by a removed body, order kept" <|
            \_ ->
                let
                    -- Simulate [b1(id=0), b2(id=2)] — gap at 1
                    existing =
                        [ ( "a", withInternalId 0 (Physics.plane Material.wood) )
                        , ( "b", withInternalId 2 (Physics.plane Material.wood) )
                        ]

                    ( result, _ ) =
                        step (existing ++ [ ( "c", Physics.plane Material.wood ) ])
                in
                -- "c" fills the gap at id=1; array stays compact [0,1,2]
                Expect.equal
                    [ ( "a", 0 ), ( "b", 2 ), ( "c", 1 ) ]
                    (List.map ids result)
        , test "body re-added with :: gets a new id, original keeps its id, order kept" <|
            \_ ->
                let
                    ( step1, _ ) =
                        step [ ( "a", Physics.plane Material.wood ) ]

                    -- "a" now has some internal id; prepend it again as "b" using ::
                    bodyA =
                        step1
                            |> List.head
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault (Physics.plane Material.wood)

                    ( result, _ ) =
                        step (( "b", bodyA ) :: step1)
                in
                -- "b" is prepended with :: so it is the first occurrence and gets a fresh id (1);
                -- "a" is the last occurrence and keeps its id (0)
                Expect.equal
                    [ ( "b", 1 ), ( "a", 0 ) ]
                    (List.map ids result)
        , test "when two bodies share an id, the last in the list keeps it and the first gets a fresh one" <|
            \_ ->
                let
                    -- "three" is prepended (::) with the same id=0 as "one".
                    -- "three" is the first occurrence so gets a fresh id; "one" keeps id=0.
                    bodies =
                        [ ( "three", withInternalId 0 (Physics.plane Material.wood) )
                        , ( "two", withInternalId 1 (Physics.plane Material.wood) )
                        , ( "one", withInternalId 0 (Physics.plane Material.wood) )
                        ]

                    ( result, _ ) =
                        step bodies
                in
                Expect.equal
                    [ ( "three", 2 ), ( "two", 1 ), ( "one", 0 ) ]
                    (List.map ids result)
        , test "existing ids with gaps are preserved unchanged, order kept" <|
            \_ ->
                let
                    bodies =
                        [ ( "a", withInternalId 0 (Physics.plane Material.wood) )
                        , ( "b", withInternalId 5 (Physics.plane Material.wood) )
                        ]

                    ( result, _ ) =
                        step bodies
                in
                Expect.equal
                    [ ( "a", 0 ), ( "b", 5 ) ]
                    (List.map ids result)
        ]
