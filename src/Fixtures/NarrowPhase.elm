module Fixtures.NarrowPhase exposing
    ( completeSphereContactEquation
    , sphereContactBoxPositions
    , sphereContactOctohedronPositions
    )

import Internal.Const as Const
import Internal.NarrowPhase exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


sphereContactBoxPositions : Vec3 -> Float -> Float -> List ( Vec3, List Contact )
sphereContactBoxPositions center radius boxHalfExtent =
    let
        delta =
            3 * Const.precision

        nearEdgeOffset =
            boxHalfExtent - delta

        -- Reposition the box so that it contacts the sphere at each:
        -- vertex
        -- edge (midpoint)
        -- face (center)
        -- face (at a point near a vertex)
        -- face (at a point near an edge midpoint).
        -- The adjustment of -Const.precision represents a minimum
        -- penetration value.
        vertexDimension =
            boxHalfExtent + radius / sqrt 3 - Const.precision

        offDiagonalFactor =
            (vertexDimension + delta) / vertexDimension

        edgeDimension =
            boxHalfExtent + radius / sqrt 2 - Const.precision

        faceDimension =
            boxHalfExtent + radius - Const.precision

        ceq vectors =
            ( vectors.cj, completeSphereContactEquation radius vectors )

        invSqrt3 =
            1 / sqrt 3

        invSqrt2 =
            1 / sqrt 2
    in
    -- Box positions and their resulting contacts:
    [ -- the 8 vertex contacts
      ceq { cj = vec3 vertexDimension vertexDimension vertexDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt3 invSqrt3 invSqrt3, rj = vec3 -boxHalfExtent -boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 -vertexDimension vertexDimension vertexDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt3 invSqrt3 invSqrt3, rj = vec3 boxHalfExtent -boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 vertexDimension -vertexDimension vertexDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt3 -invSqrt3 invSqrt3, rj = vec3 -boxHalfExtent boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 -vertexDimension -vertexDimension vertexDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt3 -invSqrt3 invSqrt3, rj = vec3 boxHalfExtent boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 vertexDimension vertexDimension -vertexDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt3 invSqrt3 -invSqrt3, rj = vec3 -boxHalfExtent -boxHalfExtent boxHalfExtent }
    , ceq { cj = vec3 -vertexDimension vertexDimension -vertexDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt3 invSqrt3 -invSqrt3, rj = vec3 boxHalfExtent -boxHalfExtent boxHalfExtent }
    , ceq { cj = vec3 vertexDimension -vertexDimension -vertexDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt3 -invSqrt3 -invSqrt3, rj = vec3 -boxHalfExtent boxHalfExtent boxHalfExtent }
    , ceq { cj = vec3 -vertexDimension -vertexDimension -vertexDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt3 -invSqrt3 -invSqrt3, rj = vec3 boxHalfExtent boxHalfExtent boxHalfExtent }

    -- the 12 edge (midpoint) contacts
    , ceq { cj = vec3 edgeDimension edgeDimension 0 |> Vec3.add center, ci = center, ni = vec3 invSqrt2 invSqrt2 0, rj = vec3 -boxHalfExtent -boxHalfExtent 0 }
    , ceq { cj = vec3 0 edgeDimension edgeDimension |> Vec3.add center, ci = center, ni = vec3 0 invSqrt2 invSqrt2, rj = vec3 0 -boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 edgeDimension 0 edgeDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt2 0 invSqrt2, rj = vec3 -boxHalfExtent 0 -boxHalfExtent }
    , ceq { cj = vec3 -edgeDimension edgeDimension 0 |> Vec3.add center, ci = center, ni = vec3 -invSqrt2 invSqrt2 0, rj = vec3 boxHalfExtent -boxHalfExtent 0 }
    , ceq { cj = vec3 0 -edgeDimension edgeDimension |> Vec3.add center, ci = center, ni = vec3 0 -invSqrt2 invSqrt2, rj = vec3 0 boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 edgeDimension 0 -edgeDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt2 0 -invSqrt2, rj = vec3 -boxHalfExtent 0 boxHalfExtent }
    , ceq { cj = vec3 edgeDimension -edgeDimension 0 |> Vec3.add center, ci = center, ni = vec3 invSqrt2 -invSqrt2 0, rj = vec3 -boxHalfExtent boxHalfExtent 0 }
    , ceq { cj = vec3 0 edgeDimension -edgeDimension |> Vec3.add center, ci = center, ni = vec3 0 invSqrt2 -invSqrt2, rj = vec3 0 -boxHalfExtent boxHalfExtent }
    , ceq { cj = vec3 -edgeDimension 0 edgeDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt2 0 invSqrt2, rj = vec3 boxHalfExtent 0 -boxHalfExtent }
    , ceq { cj = vec3 -edgeDimension -edgeDimension 0 |> Vec3.add center, ci = center, ni = vec3 -invSqrt2 -invSqrt2 0, rj = vec3 boxHalfExtent boxHalfExtent 0 }
    , ceq { cj = vec3 0 -edgeDimension -edgeDimension |> Vec3.add center, ci = center, ni = vec3 0 -invSqrt2 -invSqrt2, rj = vec3 0 boxHalfExtent boxHalfExtent }
    , ceq { cj = vec3 -edgeDimension 0 -edgeDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt2 0 -invSqrt2, rj = vec3 boxHalfExtent 0 boxHalfExtent }

    -- the 6 face (center) contacts
    , ceq { cj = vec3 faceDimension 0 0 |> Vec3.add center, ci = center, ni = vec3 1 0 0, rj = vec3 -boxHalfExtent 0 0 }
    , ceq { cj = vec3 0 faceDimension 0 |> Vec3.add center, ci = center, ni = vec3 0 1 0, rj = vec3 0 -boxHalfExtent 0 }
    , ceq { cj = vec3 0 0 faceDimension |> Vec3.add center, ci = center, ni = vec3 0 0 1, rj = vec3 0 0 -boxHalfExtent }
    , ceq { cj = vec3 -faceDimension 0 0 |> Vec3.add center, ci = center, ni = vec3 -1 0 0, rj = vec3 boxHalfExtent 0 0 }
    , ceq { cj = vec3 0 -faceDimension 0 |> Vec3.add center, ci = center, ni = vec3 0 -1 0, rj = vec3 0 boxHalfExtent 0 }
    , ceq { cj = vec3 0 0 -faceDimension |> Vec3.add center, ci = center, ni = vec3 0 0 -1, rj = vec3 0 0 boxHalfExtent }

    -- 3 sample face contacts very near a vertex
    , ceq { cj = vec3 nearEdgeOffset faceDimension nearEdgeOffset |> Vec3.add center, ci = center, ni = vec3 0 1 0, rj = vec3 -nearEdgeOffset -boxHalfExtent -nearEdgeOffset }
    , ceq { cj = vec3 -faceDimension nearEdgeOffset nearEdgeOffset |> Vec3.add center, ci = center, ni = vec3 -1 0 0, rj = vec3 boxHalfExtent -nearEdgeOffset -nearEdgeOffset }
    , ceq { cj = vec3 nearEdgeOffset nearEdgeOffset -faceDimension |> Vec3.add center, ci = center, ni = vec3 0 0 -1, rj = vec3 -nearEdgeOffset -nearEdgeOffset boxHalfExtent }

    -- 3 sample face contacts very near an edge (midpoint)
    , ceq { cj = vec3 faceDimension nearEdgeOffset 0 |> Vec3.add center, ci = center, ni = vec3 1 0 0, rj = vec3 -boxHalfExtent -nearEdgeOffset 0 }
    , ceq { cj = vec3 nearEdgeOffset 0 faceDimension |> Vec3.add center, ci = center, ni = vec3 0 0 1, rj = vec3 -nearEdgeOffset 0 -boxHalfExtent }
    , ceq { cj = vec3 0 -faceDimension nearEdgeOffset |> Vec3.add center, ci = center, ni = vec3 0 -1 0, rj = vec3 0 boxHalfExtent -nearEdgeOffset }

    -- 3 sample edge contacts very near a vertex
    , ceq { cj = vec3 edgeDimension edgeDimension nearEdgeOffset |> Vec3.add center, ci = center, ni = vec3 invSqrt2 invSqrt2 0, rj = vec3 -boxHalfExtent -boxHalfExtent -nearEdgeOffset }
    , ceq { cj = vec3 nearEdgeOffset edgeDimension -edgeDimension |> Vec3.add center, ci = center, ni = vec3 0 invSqrt2 -invSqrt2, rj = vec3 -nearEdgeOffset -boxHalfExtent boxHalfExtent }
    , ceq { cj = vec3 -edgeDimension -nearEdgeOffset edgeDimension |> Vec3.add center, ci = center, ni = vec3 -invSqrt2 0 invSqrt2, rj = vec3 boxHalfExtent nearEdgeOffset -boxHalfExtent }

    -- 3 sample off-diagonal vertex contacts
    , ceq { cj = vec3 vertexDimension (vertexDimension * offDiagonalFactor) (vertexDimension / offDiagonalFactor) |> Vec3.add center, ci = center, ni = vec3 invSqrt3 invSqrt3 invSqrt3, rj = vec3 -boxHalfExtent -boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 (-vertexDimension / offDiagonalFactor) -vertexDimension (vertexDimension * offDiagonalFactor) |> Vec3.add center, ci = center, ni = vec3 -invSqrt3 -invSqrt3 invSqrt3, rj = vec3 boxHalfExtent boxHalfExtent -boxHalfExtent }
    , ceq { cj = vec3 (vertexDimension / offDiagonalFactor) (-vertexDimension * offDiagonalFactor) -vertexDimension |> Vec3.add center, ci = center, ni = vec3 invSqrt3 -invSqrt3 -invSqrt3, rj = vec3 -boxHalfExtent boxHalfExtent boxHalfExtent }
    ]


sphereContactOctohedronPositions : Vec3 -> Float -> Float -> List ( Vec3, List Contact )
sphereContactOctohedronPositions center radius octoHalfExtent =
    let
        delta =
            3 * Const.precision

        -- Reposition the octohedron so that it contacts the sphere at each:
        -- vertex
        -- edge (midpoint)
        -- face (center)
        -- face (at a point near a vertex)
        -- face (at a point near an edge midpoint)
        -- The adjustment of -Const.precision represents a minimum
        -- penetration value.
        vertexDimension =
            octoHalfExtent + radius - Const.precision

        edgeDimension =
            octoHalfExtent / 2 + radius / sqrt 2 - Const.precision

        faceDimension =
            octoHalfExtent / 3 + radius / sqrt 3 - Const.precision

        ceq vectors =
            ( vectors.cj, completeSphereContactEquation radius vectors )

        invSqrt3 =
            1 / sqrt 3

        invSqrt2 =
            1 / sqrt 2
    in
    [ -- Octohedron positions and their contacts
      -- 6 vertex contacts
      ceq { ci = center, cj = vec3 vertexDimension 0 0 |> Vec3.add center, ni = vec3 1 0 0, rj = vec3 -octoHalfExtent 0 0 }
    , ceq { ci = center, cj = vec3 0 vertexDimension 0 |> Vec3.add center, ni = vec3 0 1 0, rj = vec3 0 -octoHalfExtent 0 }
    , ceq { ci = center, cj = vec3 0 0 vertexDimension |> Vec3.add center, ni = vec3 0 0 1, rj = vec3 0 0 -octoHalfExtent }
    , ceq { ci = center, cj = vec3 0 0 -vertexDimension |> Vec3.add center, ni = vec3 0 0 -1, rj = vec3 0 0 octoHalfExtent }
    , ceq { ci = center, cj = vec3 0 -vertexDimension 0 |> Vec3.add center, ni = vec3 0 -1 0, rj = vec3 0 octoHalfExtent 0 }
    , ceq { ci = center, cj = vec3 -vertexDimension 0 0 |> Vec3.add center, ni = vec3 -1 0 0, rj = vec3 octoHalfExtent 0 0 }

    -- 12 edge (midpoint) contacts
    , ceq { ci = center, cj = vec3 edgeDimension edgeDimension 0 |> Vec3.add center, ni = vec3 invSqrt2 invSqrt2 0, rj = vec3 (-octoHalfExtent / 2) (-octoHalfExtent / 2) 0 }
    , ceq { ci = center, cj = vec3 0 edgeDimension edgeDimension |> Vec3.add center, ni = vec3 0 invSqrt2 invSqrt2, rj = vec3 0 (-octoHalfExtent / 2) (-octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 edgeDimension 0 edgeDimension |> Vec3.add center, ni = vec3 invSqrt2 0 invSqrt2, rj = vec3 (-octoHalfExtent / 2) 0 (-octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 -edgeDimension edgeDimension 0 |> Vec3.add center, ni = vec3 -invSqrt2 invSqrt2 0, rj = vec3 (octoHalfExtent / 2) (-octoHalfExtent / 2) 0 }
    , ceq { ci = center, cj = vec3 0 -edgeDimension edgeDimension |> Vec3.add center, ni = vec3 0 -invSqrt2 invSqrt2, rj = vec3 0 (octoHalfExtent / 2) (-octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 edgeDimension 0 -edgeDimension |> Vec3.add center, ni = vec3 invSqrt2 0 -invSqrt2, rj = vec3 (-octoHalfExtent / 2) 0 (octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 edgeDimension -edgeDimension 0 |> Vec3.add center, ni = vec3 invSqrt2 -invSqrt2 0, rj = vec3 (-octoHalfExtent / 2) (octoHalfExtent / 2) 0 }
    , ceq { ci = center, cj = vec3 0 edgeDimension -edgeDimension |> Vec3.add center, ni = vec3 0 invSqrt2 -invSqrt2, rj = vec3 0 (-octoHalfExtent / 2) (octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 -edgeDimension 0 edgeDimension |> Vec3.add center, ni = vec3 -invSqrt2 0 invSqrt2, rj = vec3 (octoHalfExtent / 2) 0 (-octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 -edgeDimension -edgeDimension 0 |> Vec3.add center, ni = vec3 -invSqrt2 -invSqrt2 0, rj = vec3 (octoHalfExtent / 2) (octoHalfExtent / 2) 0 }
    , ceq { ci = center, cj = vec3 0 -edgeDimension -edgeDimension |> Vec3.add center, ni = vec3 0 -invSqrt2 -invSqrt2, rj = vec3 0 (octoHalfExtent / 2) (octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 -edgeDimension 0 -edgeDimension |> Vec3.add center, ni = vec3 -invSqrt2 0 -invSqrt2, rj = vec3 (octoHalfExtent / 2) 0 (octoHalfExtent / 2) }

    -- 8 face center contacts
    , ceq { ci = center, cj = vec3 faceDimension faceDimension faceDimension |> Vec3.add center, ni = vec3 invSqrt3 invSqrt3 invSqrt3, rj = vec3 (-octoHalfExtent / 3) (-octoHalfExtent / 3) (-octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 faceDimension faceDimension -faceDimension |> Vec3.add center, ni = vec3 invSqrt3 invSqrt3 -invSqrt3, rj = vec3 (-octoHalfExtent / 3) (-octoHalfExtent / 3) (octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 faceDimension -faceDimension faceDimension |> Vec3.add center, ni = vec3 invSqrt3 -invSqrt3 invSqrt3, rj = vec3 (-octoHalfExtent / 3) (octoHalfExtent / 3) (-octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 faceDimension -faceDimension -faceDimension |> Vec3.add center, ni = vec3 invSqrt3 -invSqrt3 -invSqrt3, rj = vec3 (-octoHalfExtent / 3) (octoHalfExtent / 3) (octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 -faceDimension faceDimension faceDimension |> Vec3.add center, ni = vec3 -invSqrt3 invSqrt3 invSqrt3, rj = vec3 (octoHalfExtent / 3) (-octoHalfExtent / 3) (-octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 -faceDimension faceDimension -faceDimension |> Vec3.add center, ni = vec3 -invSqrt3 invSqrt3 -invSqrt3, rj = vec3 (octoHalfExtent / 3) (-octoHalfExtent / 3) (octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 -faceDimension -faceDimension faceDimension |> Vec3.add center, ni = vec3 -invSqrt3 -invSqrt3 invSqrt3, rj = vec3 (octoHalfExtent / 3) (octoHalfExtent / 3) (-octoHalfExtent / 3) }
    , ceq { ci = center, cj = vec3 -faceDimension -faceDimension -faceDimension |> Vec3.add center, ni = vec3 -invSqrt3 -invSqrt3 -invSqrt3, rj = vec3 (octoHalfExtent / 3) (octoHalfExtent / 3) (octoHalfExtent / 3) }

    -- 3 face (near vertex) contacts
    , ceq { ci = center, cj = vec3 (vertexDimension - delta) delta delta |> Vec3.add center, ni = vec3 1 delta delta, rj = vec3 -octoHalfExtent 0 0 }
    , ceq { ci = center, cj = vec3 delta delta (vertexDimension - delta) |> Vec3.add center, ni = vec3 delta delta 1, rj = vec3 0 0 -octoHalfExtent }
    , ceq { ci = center, cj = vec3 (delta - vertexDimension) delta delta |> Vec3.add center, ni = vec3 -1 delta delta, rj = vec3 octoHalfExtent 0 0 }

    -- 3 face (near edge) contacts
    , ceq { ci = center, cj = vec3 (edgeDimension - delta) (edgeDimension - delta) delta |> Vec3.add center, ni = vec3 invSqrt2 invSqrt2 delta, rj = vec3 (delta - octoHalfExtent / 2) (-octoHalfExtent / 2) 0 }
    , ceq { ci = center, cj = vec3 delta (edgeDimension - delta) (edgeDimension - delta) |> Vec3.add center, ni = vec3 delta invSqrt2 invSqrt2, rj = vec3 0 (-octoHalfExtent / 2) (delta - octoHalfExtent / 2) }
    , ceq { ci = center, cj = vec3 (delta - edgeDimension) -delta (delta - edgeDimension) |> Vec3.add center, ni = vec3 -invSqrt2 -delta -invSqrt2, rj = vec3 (octoHalfExtent / 2) 0 (octoHalfExtent / 2) }
    ]


completeSphereContactEquation : Float -> { ni : Vec3, rj : Vec3, ci : Vec3, cj : Vec3 } -> List Contact
completeSphereContactEquation radius { ni, rj, ci, cj } =
    [ { ni = ni
      , pi = Vec3.add ci (Vec3.scale radius ni)
      , pj = Vec3.add cj rj
      }
    ]
