module Collision.PlaneSphereTest exposing (addContacts)

import Collision.PlaneSphere
import Expect
import Extra.Expect as Expect
import Internal.Transform3d as Transform3d
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    let
        radius =
            1

        planeTransform3d =
            Transform3d.atOrigin

        sphereTransform3d =
            Transform3d.atPoint { x = 0, y = 0, z = radius }

        delta =
            0.3

        overlappingSphereTransform3d =
            Transform3d.atPoint { x = 0, y = 0, z = radius - delta }

        nonCollidingSphereTransform3d =
            Transform3d.atPoint { x = 0, y = 0, z = radius + delta }
    in
    describe "Collision.PlaneSphere.addContacts"
        [ test "exact collision" <|
            \_ ->
                Collision.PlaneSphere.addContacts identity planeTransform3d sphereTransform3d radius []
                    |> Expect.contacts
                        [ { ni = { x = 0, y = 0, z = 1 }
                          , pi = { x = 0, y = 0, z = 0 }
                          , pj = { x = 0, y = 0, z = 0 }
                          }
                        ]
        , test "overlapping collision" <|
            \_ ->
                Collision.PlaneSphere.addContacts identity planeTransform3d overlappingSphereTransform3d radius []
                    |> Expect.contacts
                        [ { ni = { x = 0, y = 0, z = 1 }
                          , pi = { x = 0, y = 0, z = 0 }
                          , pj = { x = 0, y = 0, z = -delta }
                          }
                        ]
        , test "no collision" <|
            \_ ->
                Collision.PlaneSphere.addContacts identity planeTransform3d nonCollidingSphereTransform3d radius []
                    |> Expect.contacts []
        ]
