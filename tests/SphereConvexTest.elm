module SphereConvexTest exposing (addContacts)

import Collision.SphereConvex
import Expect exposing (Expectation)
import Fixtures.Convex
import Fixtures.NarrowPhase
import Frame3d
import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Point3d
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    let
        center =
            { x = 0, y = 0, z = 7 }

        radius =
            5

        boxHalfExtent =
            3

        boxHull =
            Fixtures.Convex.boxHull boxHalfExtent

        ( boxPositions, boxExpectedResults ) =
            Fixtures.NarrowPhase.sphereContactBoxPositions
                center
                radius
                boxHalfExtent
                |> listOfPairsToPairOfLists

        boxFarPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions
                center
                (radius * 2)
                boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            1

        octoHull =
            Fixtures.Convex.octoHull octoHalfExtent

        ( octoPositions, octoExpectedResults ) =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions
                center
                radius
                octoHalfExtent
                |> listOfPairsToPairOfLists

        octoFarPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions
                center
                (radius * 2)
                octoHalfExtent
                |> List.map Tuple.first
    in
    describe "Collision.SphereConvex.addContacts"
        [ test "for a box" <|
            \_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Frame3d.atPoint (Point3d.fromMeters center))
                                radius
                                (Frame3d.atPoint (Point3d.fromMeters position))
                                boxHull
                                []
                        )
                    |> expectNormalizedEqual
                        (normalizeListTowards <|
                            normalizeListTowards <|
                                normalizeContactTowards
                        )
                        boxExpectedResults
        , test "fail for a far box" <|
            \_ ->
                boxFarPositions
                    |> List.concatMap
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Frame3d.atPoint (Point3d.fromMeters center))
                                radius
                                (Frame3d.atPoint (Point3d.fromMeters position))
                                boxHull
                                []
                        )
                    |> Expect.equal []
        , test "for an octohedron" <|
            \_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Frame3d.atPoint (Point3d.fromMeters center))
                                radius
                                (Frame3d.atPoint (Point3d.fromMeters position))
                                octoHull
                                []
                        )
                    |> expectNormalizedEqual
                        (normalizeListTowards <|
                            normalizeListTowards <|
                                normalizeContactTowards
                        )
                        octoExpectedResults
        , test "fail for a far octohedron" <|
            \_ ->
                octoFarPositions
                    |> List.concatMap
                        (\position ->
                            Collision.SphereConvex.addContacts
                                identity
                                (Frame3d.atPoint (Point3d.fromMeters center))
                                radius
                                (Frame3d.atPoint (Point3d.fromMeters position))
                                octoHull
                                []
                        )
                    |> Expect.equal []
        ]



-- Test helpers


listOfPairsToPairOfLists : List ( a, b ) -> ( List a, List b )
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


normalizeContactTowards : Contact -> Contact -> Contact
normalizeContactTowards expected actual =
    -- optimize common case
    if actual == expected then
        actual

    else
        { ni = normalizeVec3Towards expected.ni actual.ni
        , pi = normalizeVec3Towards expected.pi actual.pi
        , pj = normalizeVec3Towards expected.pj actual.pj
        }


normalizeVec3Towards : Vec3 -> Vec3 -> Vec3
normalizeVec3Towards expected actual =
    if Vec3.distanceSquared expected actual - Const.precision < 0 then
        -- ignore any negligible difference.
        expected

    else
        actual
