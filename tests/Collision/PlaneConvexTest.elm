module Collision.PlaneConvexTest exposing (addContacts)

import Collision.PlaneConvex
import Expect
import Internal.Transform3d as Transform3d
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    describe "Collision.PlaneConvex.addContacts"
        [ test "a cylinder cap flat on a plane culls the dipped ring to 4 contacts" <|
            \_ ->
                let
                    plane =
                        { normal = { x = 0, y = 0, z = 1 }
                        , position = { x = 0, y = 0, z = 0 }
                        }

                    -- 12-gon cylinder, bottom cap a hair below z = 0, so all 12
                    -- bottom-ring vertices are within the contact margin.
                    cylinder =
                        Convex.fromCylinder 12 0.5 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0, y = 0, z = 0.4995 })
                in
                Collision.PlaneConvex.addContacts 0 identity plane cylinder []
                    |> List.length
                    |> Expect.equal 4
        ]
