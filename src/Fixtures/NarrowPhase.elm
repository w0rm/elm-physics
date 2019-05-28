module Fixtures.NarrowPhase exposing
    ( completeSphereContactEquation
    , sphereContactBoxPositions
    , sphereContactOctohedronPositions
    )

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)


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
      ceq { cj = { x = vertexDimension, y = vertexDimension, z = vertexDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt3, y = invSqrt3, z = invSqrt3 }, rj = { x = -boxHalfExtent, y = -boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = -vertexDimension, y = vertexDimension, z = vertexDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt3, y = invSqrt3, z = invSqrt3 }, rj = { x = boxHalfExtent, y = -boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = vertexDimension, y = -vertexDimension, z = vertexDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt3, y = -invSqrt3, z = invSqrt3 }, rj = { x = -boxHalfExtent, y = boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = -vertexDimension, y = -vertexDimension, z = vertexDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt3, y = -invSqrt3, z = invSqrt3 }, rj = { x = boxHalfExtent, y = boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = vertexDimension, y = vertexDimension, z = -vertexDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt3, y = invSqrt3, z = -invSqrt3 }, rj = { x = -boxHalfExtent, y = -boxHalfExtent, z = boxHalfExtent } }
    , ceq { cj = { x = -vertexDimension, y = vertexDimension, z = -vertexDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt3, y = invSqrt3, z = -invSqrt3 }, rj = { x = boxHalfExtent, y = -boxHalfExtent, z = boxHalfExtent } }
    , ceq { cj = { x = vertexDimension, y = -vertexDimension, z = -vertexDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt3, y = -invSqrt3, z = -invSqrt3 }, rj = { x = -boxHalfExtent, y = boxHalfExtent, z = boxHalfExtent } }
    , ceq { cj = { x = -vertexDimension, y = -vertexDimension, z = -vertexDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt3, y = -invSqrt3, z = -invSqrt3 }, rj = { x = boxHalfExtent, y = boxHalfExtent, z = boxHalfExtent } }

    -- the 12 edge (midpoint) contacts
    , ceq { cj = { x = edgeDimension, y = edgeDimension, z = 0 } |> Vec3.add center, ci = center, ni = { x = invSqrt2, y = invSqrt2, z = 0 }, rj = { x = -boxHalfExtent, y = -boxHalfExtent, z = 0 } }
    , ceq { cj = { x = 0, y = edgeDimension, z = edgeDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = invSqrt2, z = invSqrt2 }, rj = { x = 0, y = -boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = edgeDimension, y = 0, z = edgeDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt2, y = 0, z = invSqrt2 }, rj = { x = -boxHalfExtent, y = 0, z = -boxHalfExtent } }
    , ceq { cj = { x = -edgeDimension, y = edgeDimension, z = 0 } |> Vec3.add center, ci = center, ni = { x = -invSqrt2, y = invSqrt2, z = 0 }, rj = { x = boxHalfExtent, y = -boxHalfExtent, z = 0 } }
    , ceq { cj = { x = 0, y = -edgeDimension, z = edgeDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = -invSqrt2, z = invSqrt2 }, rj = { x = 0, y = boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = edgeDimension, y = 0, z = -edgeDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt2, y = 0, z = -invSqrt2 }, rj = { x = -boxHalfExtent, y = 0, z = boxHalfExtent } }
    , ceq { cj = { x = edgeDimension, y = -edgeDimension, z = 0 } |> Vec3.add center, ci = center, ni = { x = invSqrt2, y = -invSqrt2, z = 0 }, rj = { x = -boxHalfExtent, y = boxHalfExtent, z = 0 } }
    , ceq { cj = { x = 0, y = edgeDimension, z = -edgeDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = invSqrt2, z = -invSqrt2 }, rj = { x = 0, y = -boxHalfExtent, z = boxHalfExtent } }
    , ceq { cj = { x = -edgeDimension, y = 0, z = edgeDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt2, y = 0, z = invSqrt2 }, rj = { x = boxHalfExtent, y = 0, z = -boxHalfExtent } }
    , ceq { cj = { x = -edgeDimension, y = -edgeDimension, z = 0 } |> Vec3.add center, ci = center, ni = { x = -invSqrt2, y = -invSqrt2, z = 0 }, rj = { x = boxHalfExtent, y = boxHalfExtent, z = 0 } }
    , ceq { cj = { x = 0, y = -edgeDimension, z = -edgeDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = -invSqrt2, z = -invSqrt2 }, rj = { x = 0, y = boxHalfExtent, z = boxHalfExtent } }
    , ceq { cj = { x = -edgeDimension, y = 0, z = -edgeDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt2, y = 0, z = -invSqrt2 }, rj = { x = boxHalfExtent, y = 0, z = boxHalfExtent } }

    -- the 6 face (center) contacts
    , ceq { cj = { x = faceDimension, y = 0, z = 0 } |> Vec3.add center, ci = center, ni = Vec3.i, rj = { x = -boxHalfExtent, y = 0, z = 0 } }
    , ceq { cj = { x = 0, y = faceDimension, z = 0 } |> Vec3.add center, ci = center, ni = Vec3.j, rj = { x = 0, y = -boxHalfExtent, z = 0 } }
    , ceq { cj = { x = 0, y = 0, z = faceDimension } |> Vec3.add center, ci = center, ni = Vec3.k, rj = { x = 0, y = 0, z = -boxHalfExtent } }
    , ceq { cj = { x = -faceDimension, y = 0, z = 0 } |> Vec3.add center, ci = center, ni = { x = -1, y = 0, z = 0 }, rj = { x = boxHalfExtent, y = 0, z = 0 } }
    , ceq { cj = { x = 0, y = -faceDimension, z = 0 } |> Vec3.add center, ci = center, ni = { x = 0, y = -1, z = 0 }, rj = { x = 0, y = boxHalfExtent, z = 0 } }
    , ceq { cj = { x = 0, y = 0, z = -faceDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = 0, z = -1 }, rj = { x = 0, y = 0, z = boxHalfExtent } }

    -- 3 sample face contacts very near a vertex
    , ceq { cj = { x = nearEdgeOffset, y = faceDimension, z = nearEdgeOffset } |> Vec3.add center, ci = center, ni = Vec3.j, rj = { x = -nearEdgeOffset, y = -boxHalfExtent, z = -nearEdgeOffset } }
    , ceq { cj = { x = -faceDimension, y = nearEdgeOffset, z = nearEdgeOffset } |> Vec3.add center, ci = center, ni = { x = -1, y = 0, z = 0 }, rj = { x = boxHalfExtent, y = -nearEdgeOffset, z = -nearEdgeOffset } }
    , ceq { cj = { x = nearEdgeOffset, y = nearEdgeOffset, z = -faceDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = 0, z = -1 }, rj = { x = -nearEdgeOffset, y = -nearEdgeOffset, z = boxHalfExtent } }

    -- 3 sample face contacts very near an edge (midpoint)
    , ceq { cj = { x = faceDimension, y = nearEdgeOffset, z = 0 } |> Vec3.add center, ci = center, ni = Vec3.i, rj = { x = -boxHalfExtent, y = -nearEdgeOffset, z = 0 } }
    , ceq { cj = { x = nearEdgeOffset, y = 0, z = faceDimension } |> Vec3.add center, ci = center, ni = Vec3.k, rj = { x = -nearEdgeOffset, y = 0, z = -boxHalfExtent } }
    , ceq { cj = { x = 0, y = -faceDimension, z = nearEdgeOffset } |> Vec3.add center, ci = center, ni = { x = 0, y = -1, z = 0 }, rj = { x = 0, y = boxHalfExtent, z = -nearEdgeOffset } }

    -- 3 sample edge contacts very near a vertex
    , ceq { cj = { x = edgeDimension, y = edgeDimension, z = nearEdgeOffset } |> Vec3.add center, ci = center, ni = { x = invSqrt2, y = invSqrt2, z = 0 }, rj = { x = -boxHalfExtent, y = -boxHalfExtent, z = -nearEdgeOffset } }
    , ceq { cj = { x = nearEdgeOffset, y = edgeDimension, z = -edgeDimension } |> Vec3.add center, ci = center, ni = { x = 0, y = invSqrt2, z = -invSqrt2 }, rj = { x = -nearEdgeOffset, y = -boxHalfExtent, z = boxHalfExtent } }
    , ceq { cj = { x = -edgeDimension, y = -nearEdgeOffset, z = edgeDimension } |> Vec3.add center, ci = center, ni = { x = -invSqrt2, y = 0, z = invSqrt2 }, rj = { x = boxHalfExtent, y = nearEdgeOffset, z = -boxHalfExtent } }

    -- 3 sample off-diagonal vertex contacts
    , ceq { cj = { x = vertexDimension, y = vertexDimension * offDiagonalFactor, z = vertexDimension / offDiagonalFactor } |> Vec3.add center, ci = center, ni = { x = invSqrt3, y = invSqrt3, z = invSqrt3 }, rj = { x = -boxHalfExtent, y = -boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = -vertexDimension / offDiagonalFactor, y = -vertexDimension, z = vertexDimension * offDiagonalFactor } |> Vec3.add center, ci = center, ni = { x = -invSqrt3, y = -invSqrt3, z = invSqrt3 }, rj = { x = boxHalfExtent, y = boxHalfExtent, z = -boxHalfExtent } }
    , ceq { cj = { x = vertexDimension / offDiagonalFactor, y = -vertexDimension * offDiagonalFactor, z = -vertexDimension } |> Vec3.add center, ci = center, ni = { x = invSqrt3, y = -invSqrt3, z = -invSqrt3 }, rj = { x = -boxHalfExtent, y = boxHalfExtent, z = boxHalfExtent } }
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
      ceq { ci = center, cj = { x = vertexDimension, y = 0, z = 0 } |> Vec3.add center, ni = Vec3.i, rj = { x = -octoHalfExtent, y = 0, z = 0 } }
    , ceq { ci = center, cj = { x = 0, y = vertexDimension, z = 0 } |> Vec3.add center, ni = Vec3.j, rj = { x = 0, y = -octoHalfExtent, z = 0 } }
    , ceq { ci = center, cj = { x = 0, y = 0, z = vertexDimension } |> Vec3.add center, ni = Vec3.k, rj = { x = 0, y = 0, z = -octoHalfExtent } }
    , ceq { ci = center, cj = { x = 0, y = 0, z = -vertexDimension } |> Vec3.add center, ni = { x = 0, y = 0, z = -1 }, rj = { x = 0, y = 0, z = octoHalfExtent } }
    , ceq { ci = center, cj = { x = 0, y = -vertexDimension, z = 0 } |> Vec3.add center, ni = { x = 0, y = -1, z = 0 }, rj = { x = 0, y = octoHalfExtent, z = 0 } }
    , ceq { ci = center, cj = { x = -vertexDimension, y = 0, z = 0 } |> Vec3.add center, ni = { x = -1, y = 0, z = 0 }, rj = { x = octoHalfExtent, y = 0, z = 0 } }

    -- 12 edge (midpoint) contacts
    , ceq { ci = center, cj = { x = edgeDimension, y = edgeDimension, z = 0 } |> Vec3.add center, ni = { x = invSqrt2, y = invSqrt2, z = 0 }, rj = { x = -octoHalfExtent / 2, y = -octoHalfExtent / 2, z = 0 } }
    , ceq { ci = center, cj = { x = 0, y = edgeDimension, z = edgeDimension } |> Vec3.add center, ni = { x = 0, y = invSqrt2, z = invSqrt2 }, rj = { x = 0, y = -octoHalfExtent / 2, z = -octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = edgeDimension, y = 0, z = edgeDimension } |> Vec3.add center, ni = { x = invSqrt2, y = 0, z = invSqrt2 }, rj = { x = -octoHalfExtent / 2, y = 0, z = -octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = -edgeDimension, y = edgeDimension, z = 0 } |> Vec3.add center, ni = { x = -invSqrt2, y = invSqrt2, z = 0 }, rj = { x = octoHalfExtent / 2, y = -octoHalfExtent / 2, z = 0 } }
    , ceq { ci = center, cj = { x = 0, y = -edgeDimension, z = edgeDimension } |> Vec3.add center, ni = { x = 0, y = -invSqrt2, z = invSqrt2 }, rj = { x = 0, y = octoHalfExtent / 2, z = -octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = edgeDimension, y = 0, z = -edgeDimension } |> Vec3.add center, ni = { x = invSqrt2, y = 0, z = -invSqrt2 }, rj = { x = -octoHalfExtent / 2, y = 0, z = octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = edgeDimension, y = -edgeDimension, z = 0 } |> Vec3.add center, ni = { x = invSqrt2, y = -invSqrt2, z = 0 }, rj = { x = -octoHalfExtent / 2, y = octoHalfExtent / 2, z = 0 } }
    , ceq { ci = center, cj = { x = 0, y = edgeDimension, z = -edgeDimension } |> Vec3.add center, ni = { x = 0, y = invSqrt2, z = -invSqrt2 }, rj = { x = 0, y = -octoHalfExtent / 2, z = octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = -edgeDimension, y = 0, z = edgeDimension } |> Vec3.add center, ni = { x = -invSqrt2, y = 0, z = invSqrt2 }, rj = { x = octoHalfExtent / 2, y = 0, z = -octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = -edgeDimension, y = -edgeDimension, z = 0 } |> Vec3.add center, ni = { x = -invSqrt2, y = -invSqrt2, z = 0 }, rj = { x = octoHalfExtent / 2, y = octoHalfExtent / 2, z = 0 } }
    , ceq { ci = center, cj = { x = 0, y = -edgeDimension, z = -edgeDimension } |> Vec3.add center, ni = { x = 0, y = -invSqrt2, z = -invSqrt2 }, rj = { x = 0, y = octoHalfExtent / 2, z = octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = -edgeDimension, y = 0, z = -edgeDimension } |> Vec3.add center, ni = { x = -invSqrt2, y = 0, z = -invSqrt2 }, rj = { x = octoHalfExtent / 2, y = 0, z = octoHalfExtent / 2 } }

    -- 8 face center contacts
    , ceq { ci = center, cj = { x = faceDimension, y = faceDimension, z = faceDimension } |> Vec3.add center, ni = { x = invSqrt3, y = invSqrt3, z = invSqrt3 }, rj = { x = -octoHalfExtent / 3, y = -octoHalfExtent / 3, z = -octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = faceDimension, y = faceDimension, z = -faceDimension } |> Vec3.add center, ni = { x = invSqrt3, y = invSqrt3, z = -invSqrt3 }, rj = { x = -octoHalfExtent / 3, y = -octoHalfExtent / 3, z = octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = faceDimension, y = -faceDimension, z = faceDimension } |> Vec3.add center, ni = { x = invSqrt3, y = -invSqrt3, z = invSqrt3 }, rj = { x = -octoHalfExtent / 3, y = octoHalfExtent / 3, z = -octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = faceDimension, y = -faceDimension, z = -faceDimension } |> Vec3.add center, ni = { x = invSqrt3, y = -invSqrt3, z = -invSqrt3 }, rj = { x = -octoHalfExtent / 3, y = octoHalfExtent / 3, z = octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = -faceDimension, y = faceDimension, z = faceDimension } |> Vec3.add center, ni = { x = -invSqrt3, y = invSqrt3, z = invSqrt3 }, rj = { x = octoHalfExtent / 3, y = -octoHalfExtent / 3, z = -octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = -faceDimension, y = faceDimension, z = -faceDimension } |> Vec3.add center, ni = { x = -invSqrt3, y = invSqrt3, z = -invSqrt3 }, rj = { x = octoHalfExtent / 3, y = -octoHalfExtent / 3, z = octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = -faceDimension, y = -faceDimension, z = faceDimension } |> Vec3.add center, ni = { x = -invSqrt3, y = -invSqrt3, z = invSqrt3 }, rj = { x = octoHalfExtent / 3, y = octoHalfExtent / 3, z = -octoHalfExtent / 3 } }
    , ceq { ci = center, cj = { x = -faceDimension, y = -faceDimension, z = -faceDimension } |> Vec3.add center, ni = { x = -invSqrt3, y = -invSqrt3, z = -invSqrt3 }, rj = { x = octoHalfExtent / 3, y = octoHalfExtent / 3, z = octoHalfExtent / 3 } }

    -- 3 face (near vertex) contacts
    , ceq { ci = center, cj = { x = vertexDimension - delta, y = delta, z = delta } |> Vec3.add center, ni = { x = 1, y = delta, z = delta }, rj = { x = -octoHalfExtent, y = 0, z = 0 } }
    , ceq { ci = center, cj = { x = delta, y = delta, z = vertexDimension - delta } |> Vec3.add center, ni = { x = delta, y = delta, z = 1 }, rj = { x = 0, y = 0, z = -octoHalfExtent } }
    , ceq { ci = center, cj = { x = delta - vertexDimension, y = delta, z = delta } |> Vec3.add center, ni = { x = -1, y = delta, z = delta }, rj = { x = octoHalfExtent, y = 0, z = 0 } }

    -- 3 face (near edge) contacts
    , ceq { ci = center, cj = { x = edgeDimension - delta, y = edgeDimension - delta, z = delta } |> Vec3.add center, ni = { x = invSqrt2, y = invSqrt2, z = delta }, rj = { x = delta - octoHalfExtent / 2, y = -octoHalfExtent / 2, z = 0 } }
    , ceq { ci = center, cj = { x = delta, y = edgeDimension - delta, z = edgeDimension - delta } |> Vec3.add center, ni = { x = delta, y = invSqrt2, z = invSqrt2 }, rj = { x = 0, y = -octoHalfExtent / 2, z = delta - octoHalfExtent / 2 } }
    , ceq { ci = center, cj = { x = delta - edgeDimension, y = -delta, z = delta - edgeDimension } |> Vec3.add center, ni = { x = -invSqrt2, y = -delta, z = -invSqrt2 }, rj = { x = octoHalfExtent / 2, y = 0, z = octoHalfExtent / 2 } }
    ]


completeSphereContactEquation : Float -> { ni : Vec3, rj : Vec3, ci : Vec3, cj : Vec3 } -> List Contact
completeSphereContactEquation radius { ni, rj, ci, cj } =
    [ { ni = ni
      , pi = Vec3.add ci (Vec3.scale radius ni)
      , pj = Vec3.add cj rj
      }
    ]
