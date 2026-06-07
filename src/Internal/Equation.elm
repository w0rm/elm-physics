module Internal.Equation exposing
    ( ConstraintEquation
    , ContactEquations
    , Ctx
    , EquationsGroup
    , Jacobian
    , WarmStart
    , equationsForPair
    )

import Internal.Body exposing (Body)
import Internal.Constraint exposing (Constraint(..))
import Internal.Contact exposing (Contact, PairGroup, SolverContact)
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


type alias Ctx =
    { dt : Float
    , gravity : Vec3
    , gravityLength : Float
    , warmStart : ContactCache WarmStart
    }


{-| One contact's warm-start payload: the normal's solved lambda and the
friction1 (t1) direction. Stored together — both are keyed by the same contact
id within the same body-pair node, so one cache and one lookup serve both.
-}
type alias WarmStart =
    { lambda : Float
    , t1 : Vec3
    }


defaultWarmStart : WarmStart
defaultWarmStart =
    { lambda = 0, t1 = Vec3.zero }


{-| One contact's three solved lambdas over a static `ContactData` shared by
reference, so each solver sweep re-allocates only this small record.
-}
type alias ContactEquations =
    { normalLambda : Float
    , friction1Lambda : Float
    , friction2Lambda : Float
    , data : ContactData
    }


{-| Build-once data for a contact's normal + two friction equations: three
jacobians and their precomputed solver scalars. spookEps/frictionCoefficient are
shared across the three; minImpulse/maxImpulse/keys are the normal's (frictions
clamp to the Coulomb cone, see Solver).
-}
type alias ContactData =
    { normal : Jacobian
    , friction1 : Jacobian
    , friction2 : Jacobian
    , normalSolverB : Float
    , normalSolverInvC : Float
    , normalMinImpulse : Float
    , normalMaxImpulse : Float
    , friction1SolverB : Float
    , friction1SolverInvC : Float
    , friction2SolverB : Float
    , friction2SolverInvC : Float
    , spookEps : Float
    , frictionCoefficient : Float
    , shapeKey : Int
    , featureKey : Int
    }


{-| The solver's per-pair record. Carries the two SolverBody refs so the
solver can look up body state, kind, and id without any Array.get. For 2-body
islands the solver consumes these refs directly; for multi-body islands the
refs become stale after the first iteration and the solver falls back to
`Array.get` on the body ids via `body1.body.id` / `body2.body.id`.

Equations are split: `contacts` (each a normal + its two frictions) and
`constraints` (joints, non-friction). `deltalambdaTot` is a per-pass scratch
field, reset to 0 at the start of every iteration.

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
    -- (the cache is keyed by body pair), then scan it per contact — instead of
    -- walking the cache tree for every contact point.
    let
        warmStartList =
            case contacts of
                [] ->
                    []

                _ ->
                    Cache.getGroup (ContactId.bodyKey body1.id body2.id) ctx.warmStart
    in
    { contacts = buildContactEquations ctx body1 body2 warmStartList contacts []
    , constraints = List.foldl (addConstraintEquations ctx body1 body2) [] constraints
    }


buildContactEquations : Ctx -> Body -> Body -> List ( Int, Int, WarmStart ) -> List SolverContact -> List ContactEquations -> List ContactEquations
buildContactEquations ctx body1 body2 warmStartList contacts acc =
    case contacts of
        [] ->
            acc

        solverContact :: rest ->
            let
                contact =
                    solverContact.contact

                cached =
                    Cache.lookup contact.shapeKey contact.featureKey defaultWarmStart warmStartList
            in
            buildContactEquations ctx
                body1
                body2
                warmStartList
                rest
                (contactEquations (cached.lambda * warmStartFactor) cached.t1 ctx body1 body2 solverContact :: acc)


addConstraintEquations : Ctx -> Body -> Body -> Constraint CenterOfMassCoordinates -> List ConstraintEquation -> List ConstraintEquation
addConstraintEquations ctx body1 body2 constraint =
    case constraint of
        PointToPoint pivot1 pivot2 ->
            addPointToPointConstraintEquations ctx body1 body2 pivot1 pivot2

        Hinge pivot1 axis1 pivot2 axis2 ->
            addPointToPointConstraintEquations ctx body1 body2 pivot1 pivot2
                >> addHingeRotationalConstraintEquations ctx body1 body2 axis1 axis2

        Lock pivot1 x1 y1 z1 pivot2 x2 y2 z2 ->
            addPointToPointConstraintEquations ctx body1 body2 pivot1 pivot2
                >> addLockRotationalConstraintEquations ctx body1 body2 x1 x2 y1 y2 z1 z2

        Distance distance ->
            addDistanceConstraintEquations ctx body1 body2 distance


addDistanceConstraintEquations : Ctx -> Body -> Body -> Float -> List ConstraintEquation -> List ConstraintEquation
addDistanceConstraintEquations ctx body1 body2 distance =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

        halfDistance =
            distance / 2

        ni =
            Vec3.direction (Transform3d.originPoint body2.transform3d) (Transform3d.originPoint body1.transform3d)

        ri =
            Vec3.scale halfDistance ni

        rj =
            Vec3.scale -halfDistance ni

        contact =
            { shapeKey = 0
            , featureKey = 0
            , pi = Vec3.add ri (Transform3d.originPoint body1.transform3d)
            , pj = Vec3.add rj (Transform3d.originPoint body2.transform3d)
            , ni = ni
            }

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
    in
    (::)
        { jacobian = jacobian
        , solverB = computeSolverB ctx body1 body2 jacobian (computeContactB spookA spookB 0 contact body1 body2 jacobian)
        , solverInvC = computeSolverInvC spookEps body1 body2 jacobian
        , spookEps = spookEps
        , minImpulse = -defaultMaxImpulse
        , maxImpulse = defaultMaxImpulse
        , solverLambda = 0
        }


addHingeRotationalConstraintEquations : Ctx -> Body -> Body -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addHingeRotationalConstraintEquations ctx body1 body2 axis1 axis2 equations =
    let
        worldAxis2 =
            Transform3d.directionPlaceIn body2.transform3d axis2

        ( ni1, ni2 ) =
            Vec3.tangents (Transform3d.directionPlaceIn body1.transform3d axis1)
    in
    equations
        |> addRotationalEquation ctx body1 body2 ni1 worldAxis2
        |> addRotationalEquation ctx body1 body2 ni2 worldAxis2


addLockRotationalConstraintEquations : Ctx -> Body -> Body -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addLockRotationalConstraintEquations ctx body1 body2 x1 x2 y1 y2 z1 z2 equations =
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
        |> addRotationalEquation ctx body1 body2 worldX1 worldY2
        |> addRotationalEquation ctx body1 body2 worldY1 worldZ2
        |> addRotationalEquation ctx body1 body2 worldZ1 worldX2


addRotationalEquation : Ctx -> Body -> Body -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addRotationalEquation ctx body1 body2 ni nj equations =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

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
    { jacobian = jacobian
    , solverB = computeSolverB ctx body1 body2 jacobian (computeRotationalB spookA spookB { ni = ni, nj = nj, maxAngleCos = 0 } body1 body2 jacobian)
    , solverInvC = computeSolverInvC spookEps body1 body2 jacobian
    , spookEps = spookEps
    , minImpulse = -defaultMaxImpulse
    , maxImpulse = defaultMaxImpulse
    , solverLambda = 0
    }
        :: equations


addPointToPointConstraintEquations : Ctx -> Body -> Body -> Vec3 -> Vec3 -> List ConstraintEquation -> List ConstraintEquation
addPointToPointConstraintEquations ctx body1 body2 pivot1 pivot2 equations =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

        ri =
            Transform3d.directionPlaceIn body1.transform3d pivot1

        rj =
            Transform3d.directionPlaceIn body2.transform3d pivot2
    in
    List.foldl
        (\ni ->
            let
                contact =
                    { shapeKey = 0
                    , featureKey = 0
                    , pi = Vec3.add (Transform3d.originPoint body1.transform3d) ri
                    , pj = Vec3.add (Transform3d.originPoint body2.transform3d) rj
                    , ni = ni
                    }

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
            in
            (::)
                { jacobian = jacobian
                , solverB = computeSolverB ctx body1 body2 jacobian (computeContactB spookA spookB 0 contact body1 body2 jacobian)
                , solverInvC = computeSolverInvC spookEps body1 body2 jacobian
                , spookEps = spookEps
                , minImpulse = -defaultMaxImpulse
                , maxImpulse = defaultMaxImpulse
                , solverLambda = 0
                }
        )
        equations
        Vec3.basis


contactEquations : Float -> Vec3 -> Ctx -> Body -> Body -> SolverContact -> ContactEquations
contactEquations seedLambda cachedT1 ctx body1 body2 { friction, bounciness, contact } =
    let
        -- Spook parameters for this contact (module defaults for now; the seam
        -- where per-contact material stiffness/relaxation would feed in).
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

        ri =
            Vec3.sub contact.pi (Transform3d.originPoint body1.transform3d)

        rj =
            Vec3.sub contact.pj (Transform3d.originPoint body2.transform3d)

        -- cachedT1 is Vec3.zero when uncached; stableTangents then falls back to
        -- Vec3.tangents, so that is exactly the cached/uncached split.
        ( t1, t2 ) =
            Vec3.stableTangents cachedT1 contact.ni

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

        -- wA = Vec3.cross t1 ri, vB = t1, wB = Vec3.cross rj t1
        friction1Jacobian =
            { wAx = t1.y * ri.z - t1.z * ri.y
            , wAy = t1.z * ri.x - t1.x * ri.z
            , wAz = t1.x * ri.y - t1.y * ri.x
            , vBx = t1.x
            , vBy = t1.y
            , vBz = t1.z
            , wBx = rj.y * t1.z - rj.z * t1.y
            , wBy = rj.z * t1.x - rj.x * t1.z
            , wBz = rj.x * t1.y - rj.y * t1.x
            }

        -- wA = Vec3.cross t2 ri, vB = t2, wB = Vec3.cross rj t2
        friction2Jacobian =
            { wAx = t2.y * ri.z - t2.z * ri.y
            , wAy = t2.z * ri.x - t2.x * ri.z
            , wAz = t2.x * ri.y - t2.y * ri.x
            , vBx = t2.x
            , vBy = t2.y
            , vBz = t2.z
            , wBx = rj.y * t2.z - rj.z * t2.y
            , wBy = rj.z * t2.x - rj.x * t2.z
            , wBz = rj.x * t2.y - rj.y * t2.x
            }
    in
    -- Lambdas at the top are mutated each iteration; ContactData is static, so
    -- each sweep re-allocates only this small outer record.
    { normalLambda = seedLambda
    , friction1Lambda = 0
    , friction2Lambda = 0
    , data =
        { normal = normalJacobian
        , friction1 = friction1Jacobian
        , friction2 = friction2Jacobian
        , normalSolverB = computeSolverB ctx body1 body2 normalJacobian (computeContactB spookA spookB bounciness contact body1 body2 normalJacobian)
        , normalSolverInvC = computeSolverInvC spookEps body1 body2 normalJacobian
        , normalMinImpulse = 0
        , normalMaxImpulse = defaultMaxImpulse
        , friction1SolverB = computeSolverB ctx body1 body2 friction1Jacobian (computeFrictionB spookB body1 body2 friction1Jacobian)
        , friction1SolverInvC = computeSolverInvC spookEps body1 body2 friction1Jacobian
        , friction2SolverB = computeSolverB ctx body1 body2 friction2Jacobian (computeFrictionB spookB body1 body2 friction2Jacobian)
        , friction2SolverInvC = computeSolverInvC spookEps body1 body2 friction2Jacobian
        , spookEps = spookEps
        , frictionCoefficient = friction
        , shapeKey = contact.shapeKey
        , featureKey = contact.featureKey
        }
    }


{-| Bound on a constraint/normal equation's accumulated solver impulse (`lambda`).
Large enough to be effectively unbounded for the masses used here, while still
capping pathological blow-ups. Because it bounds an impulse, not a force, it does
not scale with `dt`: a finite limit would clip at a different effective force per
timestep, so a real per-material force cap must be multiplied by `dt` at the clamp.
-}
defaultMaxImpulse : Float
defaultMaxImpulse =
    1000000


{-| Scale cached lambdas at warm-start to absorb the risk of stale impulses
when contact configuration drifts between steps.
-}
warmStartFactor : Float
warmStartFactor =
    0.85


{-| The Spook soft-constraint parameters, derived from a stiffness and a
relaxation (the number of timesteps over which a constraint violation is
relaxed), exactly as cannon.js does:

    spookA   = 4 / (dt · (1 + 4·relaxation))                 -- position feedback
    spookB   = 4·relaxation / (1 + 4·relaxation)             -- velocity coefficient
    spookEps = 4 / (dt² · stiffness · (1 + 4·relaxation))    -- regularization

`spookB` < 1 leaves a little of the relative velocity uncanceled each step
(velocity-level damping), and `spookEps` gives the constraint a small
compliance that dissipates the position-feedback energy — together they keep
resting stacks calm instead of buzzing.

Each builder computes the three values as plain locals (no wrapper record) from
`dt` and the relaxation/stiffness it has on hand, passes `spookA`/`spookB` into
`computeB`, and stores `spookEps` on the equation for the solver's
per-iteration regularizer. Contacts source relaxation/stiffness here from the
module defaults, but this is the seam to combine them per contact from the two
shapes' materials (the way `friction`/`bounciness` already are) — which is why
the values are not cached globally on `Ctx`.

-}
defaultRelaxation : Float
defaultRelaxation =
    3


defaultStiffness : Float
defaultStiffness =
    10000000


{-| A joint equation: a static jacobian + solver scalars plus its accumulated
`solverLambda` (re-allocated each iteration to update the lambda).
-}
type alias ConstraintEquation =
    { jacobian : Jacobian
    , solverB : Float
    , solverInvC : Float
    , spookEps : Float
    , minImpulse : Float
    , maxImpulse : Float
    , solverLambda : Float
    }


{-| Fold a velocity-bias RHS (`velocityB`) with the -dt·GiMf force term.
-}
computeSolverB : Ctx -> Body -> Body -> Jacobian -> Float -> Float
computeSolverB ctx bi bj jacobian velocityB =
    velocityB - (ctx.dt * computeGiMf ctx.gravity bi bj jacobian)


{-| Constant 1 / (G·M⁻¹·Gᵀ + ε) scaling each iteration's correction.
-}
computeSolverInvC : Float -> Body -> Body -> Jacobian -> Float
computeSolverInvC spookEps bi bj jacobian =
    1 / (computeGimgt bi bj jacobian + spookEps)


computeContactB : Float -> Float -> Float -> Contact -> Body -> Body -> Jacobian -> Float
computeContactB spookA spookB bounciness { pi, pj, ni } bi bj jacobian =
    let
        g =
            ((pj.x - pi.x) * ni.x)
                + ((pj.y - pi.y) * ni.y)
                + ((pj.z - pi.z) * ni.z)

        gW =
            (bounciness + 1)
                * (Vec3.dot bj.velocity ni - Vec3.dot bi.velocity ni)
                + (bj.angularVelocity.x * jacobian.wBx + bj.angularVelocity.y * jacobian.wBy + bj.angularVelocity.z * jacobian.wBz)
                + (bi.angularVelocity.x * jacobian.wAx + bi.angularVelocity.y * jacobian.wAy + bi.angularVelocity.z * jacobian.wAz)
    in
    -g * spookA - gW * spookB


type alias RotationalEquation =
    { ni : Vec3
    , nj : Vec3
    , maxAngleCos : Float
    }


computeRotationalB : Float -> Float -> RotationalEquation -> Body -> Body -> Jacobian -> Float
computeRotationalB spookA spookB { ni, nj, maxAngleCos } bi bj jacobian =
    let
        g =
            maxAngleCos - Vec3.dot ni nj

        gW =
            computeGW bi bj jacobian
    in
    -g * spookA - gW * spookB


computeFrictionB : Float -> Body -> Body -> Jacobian -> Float
computeFrictionB spookB bi bj jacobian =
    let
        gW =
            computeGW bi bj jacobian
    in
    -gW * spookB


{-| Computes G x inv(M) x f, where

  - M is the mass matrix with diagonal blocks for each body
  - f are the forces on the bodies

-}
computeGiMf : Vec3 -> Body -> Body -> Jacobian -> Float
computeGiMf gravity bi bj jacobian =
    let
        gravityi =
            if bi.kindInt == 2 then
                gravity

            else
                Vec3.zero

        gravityj =
            if bj.kindInt == 2 then
                gravity

            else
                Vec3.zero
    in
    -(jacobian.vBx * (bi.invMass * bi.force.x + gravityi.x) + jacobian.vBy * (bi.invMass * bi.force.y + gravityi.y) + jacobian.vBz * (bi.invMass * bi.force.z + gravityi.z))
        + (jacobian.vBx * (bj.invMass * bj.force.x + gravityj.x) + jacobian.vBy * (bj.invMass * bj.force.y + gravityj.y) + jacobian.vBz * (bj.invMass * bj.force.z + gravityj.z))
        + (jacobian.wAx * (bi.invInertiaWorld.m11 * bi.torque.x + bi.invInertiaWorld.m12 * bi.torque.y + bi.invInertiaWorld.m13 * bi.torque.z))
        + (jacobian.wAy * (bi.invInertiaWorld.m21 * bi.torque.x + bi.invInertiaWorld.m22 * bi.torque.y + bi.invInertiaWorld.m23 * bi.torque.z))
        + (jacobian.wAz * (bi.invInertiaWorld.m31 * bi.torque.x + bi.invInertiaWorld.m32 * bi.torque.y + bi.invInertiaWorld.m33 * bi.torque.z))
        + (jacobian.wBx * (bj.invInertiaWorld.m11 * bj.torque.x + bj.invInertiaWorld.m12 * bj.torque.y + bj.invInertiaWorld.m13 * bj.torque.z))
        + (jacobian.wBy * (bj.invInertiaWorld.m21 * bj.torque.x + bj.invInertiaWorld.m22 * bj.torque.y + bj.invInertiaWorld.m23 * bj.torque.z))
        + (jacobian.wBz * (bj.invInertiaWorld.m31 * bj.torque.x + bj.invInertiaWorld.m32 * bj.torque.y + bj.invInertiaWorld.m33 * bj.torque.z))


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


{-| Computes G x W, where W are the body velocities
-}
computeGW : Body -> Body -> Jacobian -> Float
computeGW bi bj jacobian =
    -(jacobian.vBx * bi.velocity.x + jacobian.vBy * bi.velocity.y + jacobian.vBz * bi.velocity.z)
        + (jacobian.wAx * bi.angularVelocity.x + jacobian.wAy * bi.angularVelocity.y + jacobian.wAz * bi.angularVelocity.z)
        + (jacobian.vBx * bj.velocity.x + jacobian.vBy * bj.velocity.y + jacobian.vBz * bj.velocity.z)
        + (jacobian.wBx * bj.angularVelocity.x + jacobian.wBy * bj.angularVelocity.y + jacobian.wBz * bj.angularVelocity.z)
