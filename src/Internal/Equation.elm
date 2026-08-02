module Internal.Equation exposing
    ( ConstraintEquation
    , ContactEquations
    , Ctx
    , EquationsGroup
    , Jacobian
    , ManifoldData
    , PointEquation
    , WarmStart
    , equationsForPair
    , initCtx
    , restitutionThreshold
    )

import Internal.Body exposing (Body)
import Internal.Constraint exposing (Constraint(..))
import Internal.Contact exposing (PairGroup, SolverContact)
import Internal.ContactCache as Cache exposing (ContactCache)
import Internal.ContactId as ContactId
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.SolverBody exposing (SolverBody)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| A constraint's G-matrix row: wA, vB, wB flattened to Floats (one property
lookup, not two — the solver reads these millions of times per step). vA = -vB.
-}
type alias Jacobian =
    { wAx : Float
    , wAy : Float
    , wAz : Float
    , vBx : Float
    , vBy : Float
    , vBz : Float
    , wBx : Float
    , wBy : Float
    , wBz : Float
    }


{-| Per-step solver context: dt and the soft-constraint coefficients for
contact rows — a dynamic-dynamic set and a stiffer static-pair set — plus a
stiffer-still set for joint rows.
-}
type alias Ctx =
    { dt : Float
    , invDt : Float
    , contactBiasRate : Float
    , contactMassScale : Float
    , contactImpulseScale : Float
    , staticBiasRate : Float
    , staticMassScale : Float
    , staticImpulseScale : Float
    , jointBiasRate : Float
    , jointMassScale : Float
    , jointImpulseScale : Float
    , warmStart : ContactCache WarmStart
    }


initCtx : Float -> ContactCache WarmStart -> Ctx
initCtx dt warmStart =
    let
        -- Clamp stiffness relative to the step rate: at 1/2 of it the row
        -- update overshoots under a starved iteration budget (a warm 5-box
        -- stack at 7 iterations buzzes at 0.21 m/s); 3/8 is quiet there with
        -- margin (6e-9 m/s). Stiffness sets the visible softness: resting
        -- overlap on the dropped cylinder stack is 6.6 mm at 1/4, 2.9 mm at
        -- 3/8, vs 1.4 mm in the stiff-Baumgarte solver.
        hertz =
            min contactHertz (0.375 / dt)

        contact =
            makeSoft hertz contactDampingRatio dt

        static =
            makeSoft (2 * hertz) (0.5 * contactDampingRatio) dt

        joint =
            makeSoft (min jointHertz (1 / dt)) jointDampingRatio dt
    in
    { dt = dt
    , invDt = 1 / dt
    , contactBiasRate = contact.biasRate
    , contactMassScale = contact.massScale
    , contactImpulseScale = contact.impulseScale
    , staticBiasRate = static.biasRate
    , staticMassScale = static.massScale
    , staticImpulseScale = static.impulseScale
    , jointBiasRate = joint.biasRate
    , jointMassScale = joint.massScale
    , jointImpulseScale = joint.impulseScale
    , warmStart = warmStart
    }


{-| Soft-constraint coefficients from stiffness (hertz) and damping ratio
(zeta): the row update is
`Δλ = -mass·(massScale·vn + massScale·biasRate·s) - impulseScale·λ`.
`massScale + impulseScale == 1`; the impulseScale term bleeds accumulated
impulse each visit, which resolves redundant-row load distribution and keeps
the bias from pumping energy into warm-started rows.
-}
makeSoft : Float -> Float -> Float -> { biasRate : Float, massScale : Float, impulseScale : Float }
makeSoft hertz zeta h =
    let
        omega =
            2 * pi * hertz

        a1 =
            2 * zeta + h * omega

        a2 =
            h * omega * a1

        a3 =
            1 / (1 + a2)
    in
    { biasRate = omega / a1
    , massScale = a2 * a3
    , impulseScale = a3
    }


{-| Warm-start payload within a body-pair node: a point's solved normal
lambda keyed by `(shapeKey, featureKey)`, or a manifold friction component
keyed by `(shapeKey, -1..-4)` — the world-space tangent impulse (x, y, z) and
the twist lambda. The tangent impulse is stored as a world vector with a
body-id-canonical sign, so it survives the gravity sort swapping body1/body2
and reprojects onto the next frame's tangent basis.
-}
type alias WarmStart =
    Float


{-| One manifold — a shape pair's contiguous run of contact points, sharing one
normal. Normals are solved per point; friction is solved once per manifold: a
coupled tangent pair anchored at the friction center plus a twist equation
about the normal. Lambdas at the top are mutated each iteration over static
data shared by reference, so each solver sweep re-allocates only the small
mutable records.
-}
type alias ContactEquations =
    { points : List PointEquation
    , friction1Lambda : Float
    , friction2Lambda : Float
    , twistLambda : Float
    , rolling1Lambda : Float
    , rolling2Lambda : Float
    , data : ManifoldData
    }


{-| `normalLambda` accumulates the point's normal impulse;
`maxNormalLambda` tracks the largest accumulated value seen, gating the
restitution pass (a point that never carried impulse never collided).
-}
type alias PointEquation =
    { normalLambda : Float
    , maxNormalLambda : Float
    , data : PointData
    }


{-| Soft normal row, clamped to accumulated λ ≥ 0:
`Δλ = -normalMass·(normalMassScale·vn + normalBias) - normalImpulseScale·λ`.
Separation is fixed per step, so the bias (soft for penetration, speculative
`s/dt` for gaps, capped at `contactSpeed` pushout) is precomputed.
`relativeVelocity` is the pre-solve approach speed for the restitution pass.
-}
type alias PointData =
    { normal : Jacobian
    , normalMass : Float
    , normalBias : Float
    , normalMassScale : Float
    , normalImpulseScale : Float
    , relativeVelocity : Float
    , leverArm : Float
    , shapeKey : Int
    , featureKey : Int
    }


{-| Build-once data for a manifold's friction: two tangent rows anchored at the
friction center (the averaged contact point) and a twist row about the shared
normal. `tangentInv*` is the inverted 2x2 tangent mass, coupling the tangent
rows in one solve so the Coulomb clamp can be circular. The twist row resists relative spin about the normal — invisible
to central friction because pure spin has no tangent velocity at the center;
its cone is sized in the solver from the points' lever arms.
-}
type alias ManifoldData =
    { friction1 : Jacobian
    , friction2 : Jacobian
    , twist : Jacobian
    , twistMass : Float
    , tangentInv11 : Float
    , tangentInv12 : Float
    , tangentInv22 : Float
    , frictionCoefficient : Float
    , bounciness : Float
    , rollingResistance : Float
    , rollingInv11 : Float
    , rollingInv12 : Float
    , rollingInv22 : Float
    }


{-| The solver's per-pair record. Carries the two SolverBody refs so the
solver can look up body state, kind, and id without any Array.get. For 2-body
islands the solver consumes these refs directly; for multi-body islands the
refs become stale after the first iteration and the solver falls back to
`Array.get` on the body ids via `body1.body.id` / `body2.body.id`.

Equations are split: `contacts` (manifolds: per-point normals + one central
friction block) and `constraints` (joints, non-friction). `deltalambdaTot` is a
per-pass scratch field, reset to 0 at the start of every iteration.

-}
type alias EquationsGroup id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , contacts : List ContactEquations
    , constraints : List ConstraintEquation
    , deltalambdaTot : Float
    }


equationsForPair : Ctx -> PairGroup -> { contacts : List ContactEquations, constraints : List ConstraintEquation }
equationsForPair ctx { body1, body2, contacts, constraints } =
    -- Multistep warm-start: fetch this body pair's cached warm-start list once
    -- (the cache is keyed by body pair), then scan it per equation — instead of
    -- walking the cache tree for every contact point.
    let
        warmStartList =
            Cache.getGroup (ContactId.bodyKey body1.id body2.id) ctx.warmStart

        -- canonicalize cached joint lambdas to body-id order: a gravity-sort
        -- swap flips the pivot rows' jacobians, so the seed flips with them
        sign =
            if body1.id - body2.id < 0 then
                1

            else
                -1
    in
    { contacts = buildContactEquations ctx body1 body2 warmStartList contacts []
    , constraints =
        (List.foldl (addConstraintEquations ctx body1 body2 warmStartList sign)
            { equations = [], pointToPoint = 0, hinge = 0, lock = 0, distance = 0 }
            constraints
        ).equations
    }


buildContactEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> List SolverContact -> List ContactEquations -> List ContactEquations
buildContactEquations ctx body1 body2 warmStartList contacts acc =
    case contacts of
        [] ->
            acc

        first :: _ ->
            let
                manifold =
                    takeManifold first.contact.shapeKey contacts []
            in
            buildContactEquations ctx
                body1
                body2
                warmStartList
                manifold.rest
                (manifoldEquations ctx body1 body2 warmStartList first manifold.points :: acc)


{-| Split off the leading run of contacts that share a shape pair. The narrow
phase emits each shape pair's points contiguously with one shared normal, so a
run is a manifold.
-}
takeManifold : Int -> List SolverContact -> List SolverContact -> { points : List SolverContact, rest : List SolverContact }
takeManifold shapeKey contacts acc =
    case contacts of
        [] ->
            { points = acc, rest = [] }

        solverContact :: rest ->
            if solverContact.contact.shapeKey - shapeKey == 0 then
                takeManifold shapeKey rest (solverContact :: acc)

            else
                { points = acc, rest = contacts }


{-| Equations plus per-type constraint counters: a warm-start key is
`typeTag·1024 + ordinal·8 + row`, so a seed follows its constraint by body
pair, type and per-type ordinal — reconfiguring constraints of another type
between the same bodies can't shift or cross-seed it.
-}
type alias ConstraintsAcc =
    { equations : List ConstraintEquation
    , pointToPoint : Int
    , hinge : Int
    , lock : Int
    , distance : Int
    }


addConstraintEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> Float -> Constraint CenterOfMassCoordinates -> ConstraintsAcc -> ConstraintsAcc
addConstraintEquations ctx body1 body2 warmStartList sign constraint acc =
    case constraint of
        PointToPoint pivot1 pivot2 ->
            { acc
                | equations = addPointToPointConstraintEquations ctx body1 body2 warmStartList sign (acc.pointToPoint * 8) pivot1 pivot2 acc.equations
                , pointToPoint = acc.pointToPoint + 1
            }

        Hinge pivot1 axis1 pivot2 axis2 ->
            { acc
                | equations =
                    acc.equations
                        |> addPointToPointConstraintEquations ctx body1 body2 warmStartList sign (1024 + acc.hinge * 8) pivot1 pivot2
                        |> addHingeRotationalConstraintEquations ctx body1 body2 (1024 + acc.hinge * 8 + 3) axis1 axis2
                , hinge = acc.hinge + 1
            }

        Lock pivot1 x1 y1 z1 pivot2 x2 y2 z2 ->
            { acc
                | equations =
                    acc.equations
                        |> addPointToPointConstraintEquations ctx body1 body2 warmStartList sign (2048 + acc.lock * 8) pivot1 pivot2
                        |> addLockRotationalConstraintEquations ctx body1 body2 (2048 + acc.lock * 8 + 3) x1 x2 y1 y2 z1 z2
                , lock = acc.lock + 1
            }

        Distance distance ->
            { acc
                | equations = addDistanceConstraintEquations ctx body1 body2 warmStartList sign (3072 + acc.distance * 8) distance acc.equations
                , distance = acc.distance + 1
            }


addDistanceConstraintEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> Float -> Int -> Float -> List ConstraintEquation -> List ConstraintEquation
addDistanceConstraintEquations ctx body1 body2 warmStartList sign key distance equations =
    let
        halfDistance =
            distance / 2

        origin1 =
            Transform3d.originPoint body1.transform3d

        origin2 =
            Transform3d.originPoint body2.transform3d

        ni =
            Vec3.direction origin2 origin1

        ri =
            Vec3.scale halfDistance ni

        rj =
            Vec3.scale -halfDistance ni

        -- signed violation along ni
        c =
            ((origin2.x + rj.x - origin1.x - ri.x) * ni.x)
                + ((origin2.y + rj.y - origin1.y - ri.y) * ni.y)
                + ((origin2.z + rj.z - origin1.z - ri.z) * ni.z)

        -- wA = Vec3.cross ni ri, vB = ni, wB = Vec3.cross rj ni
        jacobian =
            { wAx = ni.y * ri.z - ni.z * ri.y
            , wAy = ni.z * ri.x - ni.x * ri.z
            , wAz = ni.x * ri.y - ni.y * ri.x
            , vBx = ni.x
            , vBy = ni.y
            , vBz = ni.z
            , wBx = rj.y * ni.z - rj.z * ni.y
            , wBy = rj.z * ni.x - rj.x * ni.z
            , wBz = rj.x * ni.y - rj.y * ni.x
            }

        seed =
            sign * Cache.lookup -1 key 0 warmStartList
    in
    softConstraintEquation ctx body1 body2 jacobian c key seed :: equations


addHingeRotationalConstraintEquations : Ctx -> Body -> Body -> Int -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addHingeRotationalConstraintEquations ctx body1 body2 key axis1 axis2 equations =
    let
        worldAxis2 =
            Transform3d.directionPlaceIn body2.transform3d axis2

        ( ni1, ni2 ) =
            Vec3.tangents (Transform3d.directionPlaceIn body1.transform3d axis1)
    in
    equations
        |> addRotationalEquation ctx body1 body2 key ni1 worldAxis2
        |> addRotationalEquation ctx body1 body2 (key + 1) ni2 worldAxis2


addLockRotationalConstraintEquations : Ctx -> Body -> Body -> Int -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addLockRotationalConstraintEquations ctx body1 body2 key x1 x2 y1 y2 z1 z2 equations =
    let
        worldX1 =
            Transform3d.directionPlaceIn body1.transform3d x1

        worldY1 =
            Transform3d.directionPlaceIn body1.transform3d y1

        worldZ1 =
            Transform3d.directionPlaceIn body1.transform3d z1

        worldX2 =
            Transform3d.directionPlaceIn body2.transform3d x2

        worldY2 =
            Transform3d.directionPlaceIn body2.transform3d y2

        worldZ2 =
            Transform3d.directionPlaceIn body2.transform3d z2
    in
    equations
        |> addRotationalEquation ctx body1 body2 key worldX1 worldY2
        |> addRotationalEquation ctx body1 body2 (key + 1) worldY1 worldZ2
        |> addRotationalEquation ctx body1 body2 (key + 2) worldZ1 worldX2


addRotationalEquation : Ctx -> Body -> Body -> Int -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addRotationalEquation ctx body1 body2 key ni nj equations =
    let
        -- violation: the axes should stay perpendicular
        c =
            -(Vec3.dot ni nj)

        -- wA = Vec3.cross nj ni, vB = Vec3.zero, wB = Vec3.cross ni nj
        jacobian =
            { wAx = nj.y * ni.z - nj.z * ni.y
            , wAy = nj.z * ni.x - nj.x * ni.z
            , wAz = nj.x * ni.y - nj.y * ni.x
            , vBx = 0
            , vBy = 0
            , vBz = 0
            , wBx = ni.y * nj.z - ni.z * nj.y
            , wBy = ni.z * nj.x - ni.x * nj.z
            , wBz = ni.x * nj.y - ni.y * nj.x
            }
    in
    softConstraintEquation ctx body1 body2 jacobian c key 0 :: equations


addPointToPointConstraintEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> Float -> Int -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addPointToPointConstraintEquations ctx body1 body2 warmStartList sign key pivot1 pivot2 equations =
    let
        origin1 =
            Transform3d.originPoint body1.transform3d

        origin2 =
            Transform3d.originPoint body2.transform3d

        ri =
            Transform3d.directionPlaceIn body1.transform3d pivot1

        rj =
            Transform3d.directionPlaceIn body2.transform3d pivot2
    in
    List.foldl
        (\( row, ni ) acc ->
            let
                -- signed pivot separation along ni
                c =
                    ((origin2.x + rj.x - origin1.x - ri.x) * ni.x)
                        + ((origin2.y + rj.y - origin1.y - ri.y) * ni.y)
                        + ((origin2.z + rj.z - origin1.z - ri.z) * ni.z)

                -- wA = Vec3.cross ni ri, vB = ni, wB = Vec3.cross rj ni
                jacobian =
                    { wAx = ni.y * ri.z - ni.z * ri.y
                    , wAy = ni.z * ri.x - ni.x * ri.z
                    , wAz = ni.x * ri.y - ni.y * ri.x
                    , vBx = ni.x
                    , vBy = ni.y
                    , vBz = ni.z
                    , wBx = rj.y * ni.z - rj.z * ni.y
                    , wBy = rj.z * ni.x - rj.x * ni.z
                    , wBz = rj.x * ni.y - rj.y * ni.x
                    }

                seed =
                    sign * Cache.lookup -1 (key + row) 0 warmStartList
            in
            softConstraintEquation ctx body1 body2 jacobian c (key + row) seed :: acc
        )
        equations
        (List.indexedMap Tuple.pair Vec3.basis)


manifoldEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> SolverContact -> List SolverContact -> ContactEquations
manifoldEquations ctx body1 body2 warmStartList { friction, bounciness, rollingResistance, contact } points =
    let
        center =
            averageContactPoints points 0 0 0 0 0 0 0

        origin1 =
            Transform3d.originPoint body1.transform3d

        origin2 =
            Transform3d.originPoint body2.transform3d

        -- friction center relative to each body origin
        rc1 =
            { x = center.pix - origin1.x, y = center.piy - origin1.y, z = center.piz - origin1.z }

        rc2 =
            { x = center.pjx - origin2.x, y = center.pjy - origin2.y, z = center.pjz - origin2.z }

        ni =
            contact.ni

        ( t1, t2 ) =
            Vec3.tangents ni

        -- wA = Vec3.cross t1 rc1, vB = t1, wB = Vec3.cross rc2 t1
        friction1Jacobian =
            { wAx = t1.y * rc1.z - t1.z * rc1.y
            , wAy = t1.z * rc1.x - t1.x * rc1.z
            , wAz = t1.x * rc1.y - t1.y * rc1.x
            , vBx = t1.x
            , vBy = t1.y
            , vBz = t1.z
            , wBx = rc2.y * t1.z - rc2.z * t1.y
            , wBy = rc2.z * t1.x - rc2.x * t1.z
            , wBz = rc2.x * t1.y - rc2.y * t1.x
            }

        -- wA = Vec3.cross t2 rc1, vB = t2, wB = Vec3.cross rc2 t2
        friction2Jacobian =
            { wAx = t2.y * rc1.z - t2.z * rc1.y
            , wAy = t2.z * rc1.x - t2.x * rc1.z
            , wAz = t2.x * rc1.y - t2.y * rc1.x
            , vBx = t2.x
            , vBy = t2.y
            , vBz = t2.z
            , wBx = rc2.y * t2.z - rc2.z * t2.y
            , wBy = rc2.z * t2.x - rc2.x * t2.z
            , wBz = rc2.x * t2.y - rc2.y * t2.x
            }

        -- pure angular row about the shared normal
        twistJacobian =
            { wAx = -ni.x
            , wAy = -ni.y
            , wAz = -ni.z
            , vBx = 0
            , vBy = 0
            , vBz = 0
            , wBx = ni.x
            , wBy = ni.y
            , wBz = ni.z
            }

        twistK =
            computeAngularGimgt body1 body2 ni ni

        k11 =
            computeGimgt body1 body2 friction1Jacobian

        k22 =
            computeGimgt body1 body2 friction2Jacobian

        k12 =
            computeGimgtCross body1 body2 friction1Jacobian friction2Jacobian

        detK =
            k11 * k22 - k12 * k12

        pointEquations =
            buildPointEquations ctx
                body1
                body2
                warmStartList
                { x = center.pix, y = center.piy, z = center.piz }
                points
                []

        -- Friction warm start: the cached world-space tangent impulse,
        -- reprojected onto this frame's tangent basis. Sign is canonical to
        -- body-id order (a body1/body2 swap flips the normal and the roles);
        -- the twist lambda is swap-invariant.
        manifoldShapeKey =
            contact.shapeKey

        tangentSign =
            if body1.id - body2.id < 0 then
                1

            else
                -1

        wTx =
            Cache.lookup manifoldShapeKey -1 0 warmStartList

        wTy =
            Cache.lookup manifoldShapeKey -2 0 warmStartList

        wTz =
            Cache.lookup manifoldShapeKey -3 0 warmStartList

        -- Rolling resistance: an angular-only tangent pair braking relative
        -- rotation, torque cone μr·R·Σλn. Only round shapes set a nonzero
        -- resistance; everything else shares the zero block and skips the
        -- inertia products and cache scans. The warm-start impulse is a world
        -- vector under (shapeKey, -5..-7), reprojected like the tangent one.
        rolling =
            if rollingResistance > 0 then
                let
                    kr11 =
                        computeAngularGimgt body1 body2 t1 t1

                    kr22 =
                        computeAngularGimgt body1 body2 t2 t2

                    kr12 =
                        computeAngularGimgt body1 body2 t1 t2

                    detR =
                        kr11 * kr22 - kr12 * kr12

                    rWx =
                        Cache.lookup manifoldShapeKey -5 0 warmStartList

                    rWy =
                        Cache.lookup manifoldShapeKey -6 0 warmStartList

                    rWz =
                        Cache.lookup manifoldShapeKey -7 0 warmStartList
                in
                if detR > 0 then
                    { i11 = kr22 / detR
                    , i12 = -kr12 / detR
                    , i22 = kr11 / detR
                    , lambda1 = tangentSign * (rWx * t1.x + rWy * t1.y + rWz * t1.z)
                    , lambda2 = tangentSign * (rWx * t2.x + rWy * t2.y + rWz * t2.z)
                    }

                else
                    noRolling

            else
                noRolling
    in
    { points = pointEquations
    , friction1Lambda = tangentSign * (wTx * t1.x + wTy * t1.y + wTz * t1.z)
    , friction2Lambda = tangentSign * (wTx * t2.x + wTy * t2.y + wTz * t2.z)
    , twistLambda = Cache.lookup manifoldShapeKey -4 0 warmStartList
    , rolling1Lambda = rolling.lambda1
    , rolling2Lambda = rolling.lambda2
    , data =
        { friction1 = friction1Jacobian
        , friction2 = friction2Jacobian
        , twist = twistJacobian
        , twistMass =
            -- particles have no inertia
            if twistK > 0 then
                1 / twistK

            else
                0
        , tangentInv11 = k22 / detK
        , tangentInv12 = -k12 / detK
        , tangentInv22 = k11 / detK
        , frictionCoefficient = friction
        , bounciness = bounciness
        , rollingResistance = rollingResistance
        , rollingInv11 = rolling.i11
        , rollingInv12 = rolling.i12
        , rollingInv22 = rolling.i22
        }
    }


noRolling : { i11 : Float, i12 : Float, i22 : Float, lambda1 : Float, lambda2 : Float }
noRolling =
    { i11 = 0, i12 = 0, i22 = 0, lambda1 = 0, lambda2 = 0 }


{-| Averaged pi/pj over a manifold's points: the friction center on each body.
-}
averageContactPoints : List SolverContact -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> { count : Float, pix : Float, piy : Float, piz : Float, pjx : Float, pjy : Float, pjz : Float }
averageContactPoints points count pix piy piz pjx pjy pjz =
    case points of
        [] ->
            { count = count
            , pix = pix / count
            , piy = piy / count
            , piz = piz / count
            , pjx = pjx / count
            , pjy = pjy / count
            , pjz = pjz / count
            }

        { contact } :: rest ->
            averageContactPoints rest
                (count + 1)
                (pix + contact.pi.x)
                (piy + contact.pi.y)
                (piz + contact.pi.z)
                (pjx + contact.pj.x)
                (pjy + contact.pj.y)
                (pjz + contact.pj.z)


buildPointEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> Vec3 -> List SolverContact -> List PointEquation -> List PointEquation
buildPointEquations ctx body1 body2 warmStartList centerP1 points acc =
    case points of
        [] ->
            acc

        { contact } :: rest ->
            let
                ri =
                    Vec3.sub contact.pi (Transform3d.originPoint body1.transform3d)

                rj =
                    Vec3.sub contact.pj (Transform3d.originPoint body2.transform3d)

                -- wA = Vec3.cross contact.ni ri, vB = contact.ni, wB = Vec3.cross rj contact.ni
                normalJacobian =
                    { wAx = contact.ni.y * ri.z - contact.ni.z * ri.y
                    , wAy = contact.ni.z * ri.x - contact.ni.x * ri.z
                    , wAz = contact.ni.x * ri.y - contact.ni.y * ri.x
                    , vBx = contact.ni.x
                    , vBy = contact.ni.y
                    , vBz = contact.ni.z
                    , wBx = rj.y * contact.ni.z - rj.z * contact.ni.y
                    , wBy = rj.z * contact.ni.x - rj.x * contact.ni.z
                    , wBz = rj.x * contact.ni.y - rj.y * contact.ni.x
                    }

                cached =
                    Cache.lookup contact.shapeKey contact.featureKey 0 warmStartList

                -- signed separation along the normal (negative when penetrating)
                g =
                    ((contact.pj.x - contact.pi.x) * contact.ni.x)
                        + ((contact.pj.y - contact.pi.y) * contact.ni.y)
                        + ((contact.pj.z - contact.pi.z) * contact.ni.z)

                -- static pairs get the stiffer softness so stacks don't get
                -- pressed into the ground
                static =
                    body1.kindInt /= 2 || body2.kindInt /= 2

                bias =
                    if g > 0 then
                        -- speculative: allow approach to close the gap within
                        -- the step, then block
                        g * ctx.invDt

                    else if static then
                        max (ctx.staticMassScale * ctx.staticBiasRate * g) -contactSpeed

                    else
                        max (ctx.contactMassScale * ctx.contactBiasRate * g) -contactSpeed
            in
            buildPointEquations ctx
                body1
                body2
                warmStartList
                centerP1
                rest
                ({ normalLambda = cached
                 , maxNormalLambda = 0
                 , data =
                    { normal = normalJacobian
                    , normalMass = 1 / computeGimgt body1 body2 normalJacobian
                    , normalBias = bias
                    , normalMassScale =
                        if g > 0 then
                            1

                        else if static then
                            ctx.staticMassScale

                        else
                            ctx.contactMassScale
                    , normalImpulseScale =
                        if g > 0 then
                            0

                        else if static then
                            ctx.staticImpulseScale

                        else
                            ctx.contactImpulseScale
                    , relativeVelocity = computeGW body1 body2 normalJacobian
                    , leverArm = Vec3.distance contact.pi centerP1
                    , shapeKey = contact.shapeKey
                    , featureKey = contact.featureKey
                    }
                 }
                    :: acc
                )


{-| Bound on a constraint/normal equation's accumulated solver impulse (`lambda`).
Large enough to be effectively unbounded for the masses used here, while still
capping pathological blow-ups. Because it bounds an impulse, not a force, it does
not scale with `dt`: a finite limit would clip at a different effective force per
timestep, so a real per-material force cap must be multiplied by `dt` at the clamp.
-}
defaultMaxImpulse : Float
defaultMaxImpulse =
    1000000


{-| Contact stiffness in hertz for the soft normal rows. Should stay well
under the simulation rate; box-stack sag at rest scales inversely with it.
-}
contactHertz : Float
contactHertz =
    30


{-| Contact damping ratio (zeta) for the soft normal rows: heavily
overdamped, so penetration recovery doesn't bounce.
-}
contactDampingRatio : Float
contactDampingRatio =
    10


{-| Cap (m/s) on the penetration-recovery velocity a contact bias may
request, so deep overlap resolves over a few frames instead of exploding.
-}
contactSpeed : Float
contactSpeed =
    3


{-| Approach speed (m/s) below which restitution is not applied — resting
contacts must not bounce.
-}
restitutionThreshold : Float
restitutionThreshold =
    1


{-| Joint stiffness in hertz for the soft constraint rows. A statically loaded
row's resting violation scales as `impulseScale/(massScale·biasRate)` — the
bias must re-earn what the bleed drains — so joints run much stiffer than
contacts: 60 Hz leaves a loaded hinge sagging under 1 mm/1 m at 60 fps, and
the whipped 5-box chain is calm at the `1/dt` clamp down to 30 fps.
-}
jointHertz : Float
jointHertz =
    60


{-| Joint damping ratio (zeta): stiff correction with little overshoot.
-}
jointDampingRatio : Float
jointDampingRatio =
    2


{-| A joint equation: a soft row like the contact normals but without the
λ ≥ 0 clamp: `Δλ = -mass·(gW + bias) - impulseScale·λ`, with
`mass = massScale/K` and `bias = biasRate·C` folded in at build time.
`featureKey` identifies the row in the warm-start cache (see `ConstraintsAcc`).
-}
type alias ConstraintEquation =
    { jacobian : Jacobian
    , mass : Float
    , bias : Float
    , impulseScale : Float
    , minImpulse : Float
    , maxImpulse : Float
    , featureKey : Int
    , solverLambda : Float
    }


softConstraintEquation : Ctx -> Body -> Body -> Jacobian -> Float -> Int -> Float -> ConstraintEquation
softConstraintEquation ctx body1 body2 jacobian c featureKey seed =
    let
        k =
            computeGimgt body1 body2 jacobian
    in
    { jacobian = jacobian
    , mass =
        -- particles have no inertia for purely angular rows
        if k > 0 then
            ctx.jointMassScale / k

        else
            0
    , bias = ctx.jointBiasRate * c
    , impulseScale = ctx.jointImpulseScale
    , minImpulse = -defaultMaxImpulse
    , maxImpulse = defaultMaxImpulse
    , featureKey = featureKey
    , solverLambda = seed
    }


{-| a·(I1⁻¹+I2⁻¹)·b — the effective inverse mass coupling two angular-only
rows (twist, rolling); computeGimgt would add invMass terms that only apply
to rows with a unit linear part.
-}
computeAngularGimgt : Body -> Body -> Vec3 -> Vec3 -> Float
computeAngularGimgt bi bj a b =
    let
        invI1 =
            bi.invInertiaWorld

        invI2 =
            bj.invInertiaWorld
    in
    (a.x * (invI1.m11 * b.x + invI1.m12 * b.y + invI1.m13 * b.z))
        + (a.y * (invI1.m21 * b.x + invI1.m22 * b.y + invI1.m23 * b.z))
        + (a.z * (invI1.m31 * b.x + invI1.m32 * b.y + invI1.m33 * b.z))
        + (a.x * (invI2.m11 * b.x + invI2.m12 * b.y + invI2.m13 * b.z))
        + (a.y * (invI2.m21 * b.x + invI2.m22 * b.y + invI2.m23 * b.z))
        + (a.z * (invI2.m31 * b.x + invI2.m32 * b.y + invI2.m33 * b.z))


{-| Compute G x inv(M) x G', the effective inverse mass for this constraint.
-}
computeGimgt : Body -> Body -> Jacobian -> Float
computeGimgt bi bj jacobian =
    bi.invMass
        + bj.invMass
        + (jacobian.wAx * (bi.invInertiaWorld.m11 * jacobian.wAx + bi.invInertiaWorld.m12 * jacobian.wAy + bi.invInertiaWorld.m13 * jacobian.wAz))
        + (jacobian.wAy * (bi.invInertiaWorld.m21 * jacobian.wAx + bi.invInertiaWorld.m22 * jacobian.wAy + bi.invInertiaWorld.m23 * jacobian.wAz))
        + (jacobian.wAz * (bi.invInertiaWorld.m31 * jacobian.wAx + bi.invInertiaWorld.m32 * jacobian.wAy + bi.invInertiaWorld.m33 * jacobian.wAz))
        + (jacobian.wBx * (bj.invInertiaWorld.m11 * jacobian.wBx + bj.invInertiaWorld.m12 * jacobian.wBy + bj.invInertiaWorld.m13 * jacobian.wBz))
        + (jacobian.wBy * (bj.invInertiaWorld.m21 * jacobian.wBx + bj.invInertiaWorld.m22 * jacobian.wBy + bj.invInertiaWorld.m23 * jacobian.wBz))
        + (jacobian.wBz * (bj.invInertiaWorld.m31 * jacobian.wBx + bj.invInertiaWorld.m32 * jacobian.wBy + bj.invInertiaWorld.m33 * jacobian.wBz))


{-| G1 x inv(M) x G2', coupling the two tangent rows. Their linear parts are
orthogonal (t1·t2 = 0), so only the angular terms survive.
-}
computeGimgtCross : Body -> Body -> Jacobian -> Jacobian -> Float
computeGimgtCross bi bj j1 j2 =
    (j1.wAx * (bi.invInertiaWorld.m11 * j2.wAx + bi.invInertiaWorld.m12 * j2.wAy + bi.invInertiaWorld.m13 * j2.wAz))
        + (j1.wAy * (bi.invInertiaWorld.m21 * j2.wAx + bi.invInertiaWorld.m22 * j2.wAy + bi.invInertiaWorld.m23 * j2.wAz))
        + (j1.wAz * (bi.invInertiaWorld.m31 * j2.wAx + bi.invInertiaWorld.m32 * j2.wAy + bi.invInertiaWorld.m33 * j2.wAz))
        + (j1.wBx * (bj.invInertiaWorld.m11 * j2.wBx + bj.invInertiaWorld.m12 * j2.wBy + bj.invInertiaWorld.m13 * j2.wBz))
        + (j1.wBy * (bj.invInertiaWorld.m21 * j2.wBx + bj.invInertiaWorld.m22 * j2.wBy + bj.invInertiaWorld.m23 * j2.wBz))
        + (j1.wBz * (bj.invInertiaWorld.m31 * j2.wBx + bj.invInertiaWorld.m32 * j2.wBy + bj.invInertiaWorld.m33 * j2.wBz))


{-| Computes G x W, where W are the body velocities
-}
computeGW : Body -> Body -> Jacobian -> Float
computeGW bi bj jacobian =
    -(jacobian.vBx * bi.velocity.x + jacobian.vBy * bi.velocity.y + jacobian.vBz * bi.velocity.z)
        + (jacobian.wAx * bi.angularVelocity.x + jacobian.wAy * bi.angularVelocity.y + jacobian.wAz * bi.angularVelocity.z)
        + (jacobian.vBx * bj.velocity.x + jacobian.vBy * bj.velocity.y + jacobian.vBz * bj.velocity.z)
        + (jacobian.wBx * bj.angularVelocity.x + jacobian.wBy * bj.angularVelocity.y + jacobian.wBz * bj.angularVelocity.z)
