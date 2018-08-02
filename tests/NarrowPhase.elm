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
            1

        boxHalfExtent =
            1

        boxHull =
            HullFixtures.boxHull boxHalfExtent

        originalBoxHull =
            HullFixtures.originalBoxHull boxHalfExtent

        positions =
            Fixtures.NarrowPhase.sphereContactBoxPositions radius boxHalfExtent

        invSqrt3 =
            1 / (sqrt 3)

        invSqrt2 =
            1 / (sqrt 2)
    in
        describe "NarrowPhase.addSphereConvexContacts"
            [ test "for a box" <|
                \_ ->
                    positions
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
                            -- 8 vertex contacts FIXME
                            [ [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt3 invSqrt3 invSqrt3), ri = (vec3 invSqrt3 invSqrt3 invSqrt3), rj = (vec3 -1 -1 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt3 invSqrt3 invSqrt3), ri = (vec3 -invSqrt3 invSqrt3 invSqrt3), rj = (vec3 1 -1 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt3 -invSqrt3 invSqrt3), ri = (vec3 invSqrt3 -invSqrt3 invSqrt3), rj = (vec3 -1 1 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt3 -invSqrt3 invSqrt3), ri = (vec3 -invSqrt3 -invSqrt3 invSqrt3), rj = (vec3 1 1 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt3 invSqrt3 -invSqrt3), ri = (vec3 invSqrt3 invSqrt3 -invSqrt3), rj = (vec3 -1 -1 1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt3 invSqrt3 -invSqrt3), ri = (vec3 -invSqrt3 invSqrt3 -invSqrt3), rj = (vec3 1 -1 1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt3 -invSqrt3 -invSqrt3), ri = (vec3 invSqrt3 -invSqrt3 -invSqrt3), rj = (vec3 -1 1 1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt3 -invSqrt3 -invSqrt3), ri = (vec3 -invSqrt3 -invSqrt3 -invSqrt3), rj = (vec3 1 1 1), restitution = 0 } ]

                            -- 12 edge midpoint contacts FIXME
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt2 invSqrt2 0), ri = (vec3 invSqrt2 invSqrt2 0), rj = (vec3 -1 -1 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 invSqrt2 invSqrt2), ri = (vec3 0 invSqrt2 invSqrt2), rj = (vec3 0 -1 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt2 0 invSqrt2), ri = (vec3 invSqrt2 0 invSqrt2), rj = (vec3 -1 0 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt2 invSqrt2 0), ri = (vec3 -invSqrt2 invSqrt2 0), rj = (vec3 1 -1 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 -invSqrt2 invSqrt2), ri = (vec3 0 -invSqrt2 invSqrt2), rj = (vec3 0 1 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt2 0 -invSqrt2), ri = (vec3 invSqrt2 0 -invSqrt2), rj = (vec3 -1 0 1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 invSqrt2 -invSqrt2 0), ri = (vec3 invSqrt2 -invSqrt2 0), rj = (vec3 -1 1 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 invSqrt2 -invSqrt2), ri = (vec3 0 invSqrt2 -invSqrt2), rj = (vec3 0 -1 1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt2 0 invSqrt2), ri = (vec3 -invSqrt2 0 invSqrt2), rj = (vec3 1 0 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt2 -invSqrt2 0), ri = (vec3 -invSqrt2 -invSqrt2 0), rj = (vec3 1 1 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 -invSqrt2 -invSqrt2), ri = (vec3 0 -invSqrt2 -invSqrt2), rj = (vec3 0 1 1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -invSqrt2 0 -invSqrt2), ri = (vec3 -invSqrt2 0 -invSqrt2), rj = (vec3 1 0 1), restitution = 0 } ]

                            -- 6 face center contacts
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 1 0 0), ri = (vec3 1 0 0), rj = (vec3 -1 0 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 1 0), ri = (vec3 0 1 0), rj = (vec3 0 -1 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 0 1), ri = (vec3 0 0 1), rj = (vec3 0 0 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -1 0 0), ri = (vec3 -1 0 0), rj = (vec3 1 0 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 -1 0), ri = (vec3 0 -1 0), rj = (vec3 0 1 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 0 -1), ri = (vec3 0 0 -1), rj = (vec3 0 0 1), restitution = 0 } ]

                            -- 3 near-vertex face contacts
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 1 0), ri = (vec3 0 1 0), rj = (vec3 -0.999997 -1 -0.999997), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 -1 0 0), ri = (vec3 -1 0 0), rj = (vec3 1 -0.999997 -0.999997), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 0 -1), ri = (vec3 0 0 -1), rj = (vec3 -0.999997 -0.999997 1), restitution = 0 } ]

                            -- 3 near-edge face contacts
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 1 0 0), ri = (vec3 1 0 0), rj = (vec3 -1 -0.999997 0), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 0 1), ri = (vec3 0 0 1), rj = (vec3 -0.999997 0 -1), restitution = 0 } ]
                            , [ { bodyId1 = 0, bodyId2 = 1, ni = (vec3 0 -1 0), ri = (vec3 0 -1 0), rj = (vec3 0 1 -0.999997), restitution = 0 } ]
                            ]
            ]



-- Test helpers


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
