module NarrowPhase exposing (..)

import Expect exposing (Expectation)
import Fixtures.ConvexPolyhedron as HullFixtures
import Fixtures.NarrowPhase
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.Const as Const
import Physics.NarrowPhase as NarrowPhase
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform
import Test exposing (..)


addSphereConvexContacts : Test
addSphereConvexContacts =
    let

        radius =
            5

        boxHalfExtent =
            3

        boxHull =
            HullFixtures.boxHull boxHalfExtent

        (boxPositions, boxExpectedResults) =
            Fixtures.NarrowPhase.sphereContactBoxPositions radius boxHalfExtent
                |> listOfPairsToPairOfLists

        octoHalfExtent =
            1

        octoHull =
            HullFixtures.octoHull octoHalfExtent

        (octoPositions, octoExpectedResults) =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions radius octoHalfExtent
                |> listOfPairsToPairOfLists
                
    in
        describe "NarrowPhase.addSphereConvexContacts"
            [ test "for a box" <|
                \_ ->
                    boxPositions
                        |> List.map
                            (\position ->
                                NarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    boxHull
                                    1
                                    []
                            )
                        |> expectNormalizedEqual
                            (normalizeListTowards <| normalizeListTowards <| normalizeContactTowards)
                            boxExpectedResults
            , test "for an octohedron" <|
                \_ ->
                    octoPositions
                        |> List.map
                            (\position ->
                                NarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    octoHull
                                    1
                                    []
                            )
                        |> expectNormalizedEqual
                            (normalizeListTowards <| normalizeListTowards <| normalizeContactTowards)
                            octoExpectedResults
            ]



-- Test helpers

listOfPairsToPairOfLists : List (a, b) -> (List a, List b)
listOfPairsToPairOfLists list =
    ( List.map Tuple.first list
    , List.map Tuple.second list
    )

expectNormalizedEqual : (a -> a -> a) -> a -> a -> Expectation
expectNormalizedEqual normalizeTowards expected actual =
    actual
        |> normalizeTowards expected
        |> Expect.equal expected


normalizeListTowards : (a -> a -> a) -> List a -> List a -> List a
normalizeListTowards normalizeElementTowards expected actual =
    List.map2
        normalizeElementTowards
        expected
        actual


normalizeContactTowards : ContactEquation -> ContactEquation -> ContactEquation
normalizeContactTowards expected actual =
    -- optimize common case
    if actual == expected then
        actual
    else
        { actual
            | ni = normalizeVec3Towards expected.ni actual.ni
            , ri = normalizeVec3Towards expected.ri actual.ri
            , rj = normalizeVec3Towards expected.rj actual.rj
        }


normalizeVec3Towards : Vec3 -> Vec3 -> Vec3
normalizeVec3Towards expected actual =
    if (Vec3.distanceSquared expected actual) < Const.precision then
        -- ignore any negligible difference.
        expected
    else
        actual
