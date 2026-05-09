module Collision.CapsuleConvexTest exposing (addContacts, supportFeature)

import Collision.CapsuleConvex
import Expect
import Extra.Expect as Expect
import Internal.Matrix3 as Mat3
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Shapes.Capsule as Capsule
import Shapes.Convex as Convex
import Test exposing (Test, describe, test)


{-| Tests covering all rows of the contact-id table in
`Collision.CapsuleConvex.addContacts`:

    | Path                                          | Id                |
    | --------------------------------------------- | ----------------- |
    | Cap on face F                                 | `e1-fF` / `e2-fF` |
    | Cap on convex edge                            | `e1-e` / `e2-e`   |
    | Cap on convex vertex                          | `e1-v` / `e2-v`   |
    | Cylinder on face F (segment clip)             | `c1-fF` / `c2-fF` |
    | Cylinder on face F (closest-edge fallback)    | `c-fF`            |
    | Cylinder on convex edge, parallel             | `c1-e` / `c2-e`   |
    | Cylinder on convex edge, skew                 | `c-e`             |
    | Cylinder on convex vertex                     | `c-v`             |

After `Convex.placeIn` (with identity transform) the box's faces are
stored grouped as `[(+x, [+xf, -xf]), (+y, [+yf, -yf]), (+z, [+zf, -zf])]`,
and `bestFace` numbers them in flat traversal order 1..6 — so +x is `f1`
and +z is `f5`. That's where the `-fF` suffixes below come from.

-}
addContacts : Test
addContacts =
    let
        boxSize =
            2

        box =
            Convex.placeIn (Transform3d.atPoint { x = 0, y = 0, z = 0 })
                (Convex.fromBlock boxSize boxSize boxSize)

        radius =
            0.5

        -- ─── Cap on face F: capsule axis ∥ +z, lower cap dents +z face ──────
        capsuleAxisZ =
            Capsule.atOrigin radius 0.5
                |> Capsule.placeIn (Transform3d.atPoint { x = 0, y = 0, z = 1.9 })

        -- ─── Cylinder on face F (segment clip): horizontal capsule on +z face
        capsuleAxisY =
            Capsule.atOrigin radius 1.0
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 2)
                        |> Transform3d.translateBy { x = 0, y = 0, z = 1.4 }
                    )

        -- ─── No contact: capsule too far above the box ──────────────────────
        capsuleAxisZFar =
            Capsule.atOrigin radius 0.5
                |> Capsule.placeIn (Transform3d.atPoint { x = 0, y = 0, z = 2.6 })

        -- ─── Cap on face F + body contact on the same face ─────────────────
        -- Tilted capsule whose lower cap pokes the +z face. SAT axis is +z,
        -- so the cap branch fires (t > 0); the upper end is too far above
        -- the face plane to add a body contact, so we get a single cap
        -- contact "-e1-f5".
        capsuleTilted =
            Capsule.atOrigin radius 1.0
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 4)
                        |> Transform3d.translateBy { x = 0, y = 0, z = 2 }
                    )

        -- ─── Cylinder on face F (segment clip), TWO contacts on +x face ────
        -- Vertical capsule overhanging the +x edge of the box top — the
        -- segment clip yields p1 (deeper cylinder, z=0.9) and p2 (clipped
        -- against the face's top edge, z=1.0).
        capsuleOverhang =
            Capsule.atOrigin radius 0.5
                |> Capsule.placeIn (Transform3d.atPoint { x = 1.4, y = 0, z = 1.4 })

        -- ─── Cylinder on convex edge, parallel ──────────────────────────────
        -- Capsule axis = +x, lying along the box's +y +z top edge.
        capsuleParallelToEdge =
            Capsule.atOrigin radius 1.0
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)
                        |> Transform3d.translateBy { x = 0, y = 1.3, z = 1.3 }
                    )

        -- ─── No contact: capsule outside a box corner along a SAT axis ─────
        capsuleNearCorner =
            Capsule.atOrigin radius 0.5
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 4)
                        |> Transform3d.translateBy { x = 1.8, y = 0, z = 1.8 }
                    )

        -- ─── Cap on convex edge (`e1-e`) ────────────────────────────────────
        -- Capsule axis = (1,0,1)/√2 (45° tilt in xz), centred at (1.8,0,1.8).
        -- Lower cap pokes the +x +z box edge (the closest feature). SAT
        -- minimum is the edge axis (1,0,1)/√2 itself; t = 1 (axis aligned)
        -- so the cap branch fires with featureTag = "-e".
        capsuleCapOnEdge =
            Capsule.atOrigin radius 1.0
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 4)
                        |> Transform3d.translateBy { x = 1.8, y = 0, z = 1.8 }
                    )

        -- ─── Cap on convex vertex (`e1-v`) ──────────────────────────────────
        -- Capsule axis = (1,1,1)/√3 (body diagonal). Lower cap pokes the
        -- +x +y +z box corner. SAT minimum is the vertex axis (1,1,1)/√3;
        -- t = 1 and only v6 = (1,1,1) supports → "-v" tag.
        capsuleCapOnVertex =
            Capsule.atOrigin radius 0.5
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (acos (1 / sqrt 3))
                        |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                        |> Transform3d.translateBy { x = 1.4, y = 1.4, z = 1.4 }
                    )

        -- ─── Cylinder on face F, closest-edge fallback (`c-fF`) ─────────────
        -- Horizontal capsule axis = +x at (1.5, 0.95, 1.4). SAT picks the
        -- +z face axis (depth 0.1), but the capsule axis projects onto
        -- the +z face at x ∈ [1, 2], y = 0.95 — the +x edge plane drops
        -- the segment. We fall back to the closest face edge (the +x edge
        -- of the +z face) and emit one contact at (1, 0.95, 1.4).
        capsuleClipFallback =
            Capsule.atOrigin radius 0.5
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)
                        |> Transform3d.translateBy { x = 1.5, y = 0.95, z = 1.4 }
                    )

        -- ─── Cylinder on convex edge, skew (`c-e`) ──────────────────────────
        -- Capsule axis = (1, 0, -1)/√2 — perpendicular to the +x +z edge
        -- direction (+y) and to the SAT axis (1, 0, 1)/√2. The cylinder
        -- body grazes the box's +x +z edge from the diagonal direction;
        -- the supporting feature is that edge but the segments are skew.
        capsuleCylinderSkew =
            Capsule.atOrigin radius 1.0
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (3 * pi / 4)
                        |> Transform3d.translateBy { x = 1.3, y = 0, z = 1.3 }
                    )

        -- ─── Cylinder on convex vertex (`c-v`) ──────────────────────────────
        -- Capsule axis = (1, -1, 0)/√2 — perpendicular to the body
        -- diagonal SAT axis (1,1,1)/√3, so the cylinder body (not a cap)
        -- grazes the +x +y +z corner.
        capsuleCylinderVertex =
            Capsule.atOrigin radius 1.0
                |> Capsule.placeIn
                    (Transform3d.atOrigin
                        |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)
                        |> Transform3d.rotateAroundOwn Vec3.zAxis (-pi / 4)
                        |> Transform3d.translateBy { x = 1.2, y = 1.2, z = 1.2 }
                    )

        -- Convenient builder for parallel-edge expected contacts (test 8).
        -- ni = (pEdge - pCapsule)/|pEdge - pCapsule| with the +y +z box edge
        -- at (?, 1, 1) and capsule axis at (?, 1.3, 1.3).
        invSqrt2 =
            1 / sqrt 2
    in
    describe "Collision.CapsuleConvex.addContacts"
        [ test "Cap on face F: cap pressing into +z face of a box" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" identity capsuleAxisZ box []
                    |> Expect.contactsWithIds
                        [ { id = "-e1-f5"
                          , ni = { x = 0, y = 0, z = -1 }
                          , pi = { x = 0, y = 0, z = 0.9 }
                          , pj = { x = 0, y = 0, z = 1.0 }
                          }
                        ]
        , test "Cylinder on face F (segment clip): horizontal capsule on +z face" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" identity capsuleAxisY box []
                    |> Expect.contactsWithIds
                        [ { id = "-c2-f5"
                          , ni = { x = 0, y = 0, z = -1 }
                          , pi = { x = 0, y = -1, z = 0.9 }
                          , pj = { x = 0, y = -1, z = 1.0 }
                          }
                        , { id = "-c1-f5"
                          , ni = { x = 0, y = 0, z = -1 }
                          , pi = { x = 0, y = 1, z = 0.9 }
                          , pj = { x = 0, y = 1, z = 1.0 }
                          }
                        ]
        , test "no contact when capsule is too far away" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" identity capsuleAxisZFar box []
                    |> Expect.equal []
        , test "Cap on face F: contact is flipped correctly when convex is body1" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" (\c -> { c | ni = Vec3.negate c.ni, pi = c.pj, pj = c.pi }) capsuleAxisZ box []
                    |> Expect.contactsWithIds
                        [ { id = "-e1-f5"
                          , ni = { x = 0, y = 0, z = 1 }
                          , pi = { x = 0, y = 0, z = 1.0 }
                          , pj = { x = 0, y = 0, z = 0.9 }
                          }
                        ]
        , test "Cap on face F: tilted capsule's lower cap penetrates +z face" <|
            \_ ->
                let
                    half =
                        sqrt 2 / 2
                in
                Collision.CapsuleConvex.addContacts "" identity capsuleTilted box []
                    |> Expect.contactsWithIds
                        [ { id = "-e1-f5"
                          , ni = { x = 0, y = 0, z = -1 }
                          , pi = { x = -half, y = 0, z = 2 - half - 0.5 }
                          , pj = { x = -half, y = 0, z = 1 }
                          }
                        ]
        , test "Cylinder on face F (segment clip): vertical capsule overhanging +x edge" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" identity capsuleOverhang box []
                    |> Expect.contactsWithIds
                        [ { id = "-c2-f1"
                          , ni = { x = -1, y = 0, z = 0 }
                          , pi = { x = 0.9, y = 0, z = 1 }
                          , pj = { x = 1, y = 0, z = 1 }
                          }
                        , { id = "-c1-f1"
                          , ni = { x = -1, y = 0, z = 0 }
                          , pi = { x = 0.9, y = 0, z = 0.9 }
                          , pj = { x = 1, y = 0, z = 0.9 }
                          }
                        ]
        , test "no contact when capsule is just outside a box corner" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" identity capsuleNearCorner box []
                    |> Expect.equal []
        , test "Cylinder on convex edge, parallel: capsule axis ∥ +y +z box edge" <|
            \_ ->
                let
                    -- ni = (pEdge - pCapsule)/|pEdge - pCapsule|
                    --    = (0, -0.3, -0.3) / sqrt(0.18)
                    invLen =
                        1 / sqrt 0.18

                    ni =
                        { x = 0, y = -0.3 * invLen, z = -0.3 * invLen }

                    contactAt id_ x =
                        { id = id_
                        , ni = ni
                        , pi =
                            { x = x
                            , y = 1.3 + 0.5 * ni.y
                            , z = 1.3 + 0.5 * ni.z
                            }
                        , pj = { x = x, y = 1, z = 1 }
                        }
                in
                Collision.CapsuleConvex.addContacts "" identity capsuleParallelToEdge box []
                    |> Expect.contactsWithIds
                        [ contactAt "-c2-e" 1
                        , contactAt "-c1-e" -1
                        ]
        , test "Cap on convex edge: lower cap pokes +x +z box edge" <|
            \_ ->
                let
                    -- ep1 = (1.8 - 1/√2, 0, 1.8 - 1/√2)
                    -- ni  = -(1, 0, 1)/√2
                    -- pi  = ep1 + 0.5*ni = (1.8 - 1.5/√2, 0, 1.8 - 1.5/√2)
                    -- pj  = pi - penetration*ni = (1, 0, 1)
                    pic =
                        1.8 - 1.5 * invSqrt2
                in
                Collision.CapsuleConvex.addContacts "" identity capsuleCapOnEdge box []
                    |> Expect.contactsWithIds
                        [ { id = "-e1-e"
                          , ni = { x = -invSqrt2, y = 0, z = -invSqrt2 }
                          , pi = { x = pic, y = 0, z = pic }
                          , pj = { x = 1, y = 0, z = 1 }
                          }
                        ]
        , test "Cap on convex vertex: lower cap pokes +x +y +z box corner" <|
            \_ ->
                let
                    invSqrt3 =
                        1 / sqrt 3

                    -- ep1 = (1.4 - 0.5/√3) * (1, 1, 1)
                    -- pi  = ep1 + 0.5*ni = (1.4 - 1/√3) * (1, 1, 1)
                    pic =
                        1.4 - invSqrt3
                in
                Collision.CapsuleConvex.addContacts "" identity capsuleCapOnVertex box []
                    |> Expect.contactsWithIds
                        [ { id = "-e1-v"
                          , ni = { x = -invSqrt3, y = -invSqrt3, z = -invSqrt3 }
                          , pi = { x = pic, y = pic, z = pic }
                          , pj = { x = 1, y = 1, z = 1 }
                          }
                        ]
        , test "Cylinder on face F (closest-edge fallback): clip drops, edge contact emitted" <|
            \_ ->
                Collision.CapsuleConvex.addContacts "" identity capsuleClipFallback box []
                    |> Expect.contactsWithIds
                        [ { id = "-c-f5"
                          , ni = { x = 0, y = 0, z = -1 }
                          , pi = { x = 1, y = 0.95, z = 0.9 }
                          , pj = { x = 1, y = 0.95, z = 1.0 }
                          }
                        ]
        , test "Cylinder on convex edge, skew: cylinder body grazes +x +z edge diagonally" <|
            \_ ->
                let
                    -- pCapsule = (1.3, 0, 1.3) (closest pt on capsule axis)
                    -- ni = -(1, 0, 1)/√2
                    -- pi = pCapsule + 0.5*ni = (1.3 - 0.5/√2, 0, 1.3 - 0.5/√2)
                    pic =
                        1.3 - 0.5 * invSqrt2
                in
                Collision.CapsuleConvex.addContacts "" identity capsuleCylinderSkew box []
                    |> Expect.contactsWithIds
                        [ { id = "-c-e"
                          , ni = { x = -invSqrt2, y = 0, z = -invSqrt2 }
                          , pi = { x = pic, y = 0, z = pic }
                          , pj = { x = 1, y = 0, z = 1 }
                          }
                        ]
        , test "Cylinder on convex vertex: cylinder body grazes +x +y +z corner" <|
            \_ ->
                let
                    invSqrt3 =
                        1 / sqrt 3

                    -- pCapsule = (1.2, 1.2, 1.2)
                    -- ni = -(1, 1, 1)/√3
                    -- pi = pCapsule + 0.5*ni
                    pic =
                        1.2 - 0.5 * invSqrt3
                in
                Collision.CapsuleConvex.addContacts "" identity capsuleCylinderVertex box []
                    |> Expect.contactsWithIds
                        [ { id = "-c-v"
                          , ni = { x = -invSqrt3, y = -invSqrt3, z = -invSqrt3 }
                          , pi = { x = pic, y = pic, z = pic }
                          , pj = { x = 1, y = 1, z = 1 }
                          }
                        ]
        ]


{-| Regression test for the support-edge correctness fix.

When 3+ vertices of a convex are tied at maximum projection along an axis,
the prior implementation returned the first 2 vertices in `convex.vertices`
order. For convexes whose vertices list happens to be ordered such that the
first two tied vertices are face-diagonal (not connected by a real edge),
this produced a "virtual edge" cutting through the convex's interior — and
downstream contact geometry would derive an off-surface contact point.

This test constructs a square pyramid where the four base vertices are
ordered `[V_diag1, V_diag2, V_adj1, V_adj2]` so that the first two scanned
along axis = -z (perpendicular to the base) are diagonal, not adjacent.
The fix walks `convex.faces` and returns a real edge of the base face.

-}
supportFeature : Test
supportFeature =
    let
        vDiag1 =
            { x = 1, y = 1, z = 0 }

        vDiag2 =
            { x = -1, y = -1, z = 0 }

        vAdj1 =
            { x = -1, y = 1, z = 0 }

        vAdj2 =
            { x = 1, y = -1, z = 0 }

        vApex =
            { x = 0, y = 0, z = 1 }

        -- Square pyramid. `vertices` is hand-ordered so the first two tied
        -- along -z are face-diagonal, exposing the bug. The base face's
        -- vertex order is the natural CCW-from-below winding: starting at
        -- vDiag1 and going around `(1,1) → (1,-1) → (-1,-1) → (-1,1)`.
        baseFace =
            { vertices = [ vDiag1, vAdj2, vDiag2, vAdj1 ]
            , normal = { x = 0, y = 0, z = -1 }
            }

        sideFace1 =
            { vertices = [ vDiag1, vApex, vAdj2 ], normal = Vec3.normalize { x = 1, y = 0, z = 1 } }

        sideFace2 =
            { vertices = [ vAdj2, vApex, vDiag2 ], normal = Vec3.normalize { x = 0, y = -1, z = 1 } }

        sideFace3 =
            { vertices = [ vDiag2, vApex, vAdj1 ], normal = Vec3.normalize { x = -1, y = 0, z = 1 } }

        sideFace4 =
            { vertices = [ vAdj1, vApex, vDiag1 ], normal = Vec3.normalize { x = 0, y = 1, z = 1 } }

        pyramid =
            { faces =
                -- Each face has a unique normal direction in this pyramid,
                -- so each is its own group with no antipodal partner.
                [ ( baseFace, Nothing )
                , ( sideFace1, Nothing )
                , ( sideFace2, Nothing )
                , ( sideFace3, Nothing )
                , ( sideFace4, Nothing )
                ]
            , vertices = [ vDiag1, vDiag2, vAdj1, vAdj2, vApex ]
            , uniqueEdges = []
            , position = Vec3.zero
            , inertia = Mat3.zero
            , volume = 0
            }
    in
    describe "Collision.CapsuleConvex.supportFeature"
        [ test "returns an edge-adjacent pair when 3+ vertices are tied at max projection" <|
            \_ ->
                Collision.CapsuleConvex.supportFeature { x = 0, y = 0, z = -1 } pyramid
                    |> Expect.equal [ vDiag1, vAdj2 ]
        ]
