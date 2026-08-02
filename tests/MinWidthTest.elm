module MinWidthTest exposing (minWidth)

import Array
import Expect
import Fixtures.Convex
import Internal.Transform3d as Transform3d
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


minWidth : Test
minWidth =
    describe "Convex.minWidth"
        [ test "block via the triangular-mesh path returns the smallest dimension" <|
            \_ ->
                Fixtures.Convex.block Transform3d.atOrigin 2 3 5
                    |> Convex.minWidth
                    |> Expect.within (Expect.Absolute 1.0e-6) 2
        , test "regular tetrahedron returns the edge-to-edge width, not the face-to-vertex height" <|
            \_ ->
                -- vertices of a regular tetrahedron with edge length 2*sqrt 2:
                -- opposite edges are 2 apart, face-to-vertex height is ~2.309
                Convex.fromTriangularMesh
                    [ ( 0, 1, 2 ), ( 0, 3, 1 ), ( 0, 2, 3 ), ( 1, 3, 2 ) ]
                    (Array.fromList
                        [ { x = 1, y = 1, z = 1 }
                        , { x = 1, y = -1, z = -1 }
                        , { x = -1, y = 1, z = -1 }
                        , { x = -1, y = -1, z = 1 }
                        ]
                    )
                    |> Convex.minWidth
                    |> Expect.within (Expect.Absolute 1.0e-6) 2
        ]
