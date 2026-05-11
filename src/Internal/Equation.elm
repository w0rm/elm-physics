module Internal.Equation exposing
    ( Ctx
    , Equation
    , EquationsGroup
    , SolverEquation
    , equationsForPair
    )

import Dict exposing (Dict)
import Internal.Body as Body exposing (Body)
import Internal.Constraint exposing (Constraint(..))
import Internal.Contact exposing (Contact, PairGroup, SolverContact)
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.SolverBody exposing (SolverBody)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias Equation =
    { id : String
    , minForce : Float
    , maxForce : Float
    , solverB : Float
    , solverInvC : Float
    , spookA : Float
    , spookB : Float
    , spookEps : Float

    -- wA, vB, wB are conceptually Vec3, flattened to Floats so the JS
    -- runtime does one property lookup per access instead of two. The
    -- solver reads these millions of times per step. vA = -vB.
    , wAx : Float
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
    , lambdas : Dict String Float
    }


{-| The solver's per-pair record. Carries the two SolverBody refs so the
solver can look up body state, kind, and id without any Array.get. For 2-body
islands the solver consumes these refs directly; for multi-body islands the
refs become stale after the first iteration and the solver falls back to
`Array.get` on the body ids via `body1.body.id` / `body2.body.id`.
-}
type alias EquationsGroup id =
    { body1 : SolverBody id
    , body2 : SolverBody id
    , equations : List SolverEquation
    }


equationsForPair : Ctx -> PairGroup -> List SolverEquation
equationsForPair ctx { body1, body2, contacts, constraints } =
    []
        |> (\eqs -> List.foldl (addContactEquations ctx body1 body2) eqs contacts)
        |> (\eqs -> List.foldl (addConstraintEquations ctx body1 body2) eqs constraints)


addConstraintEquations : Ctx -> Body -> Body -> Constraint CenterOfMassCoordinates -> List SolverEquation -> List SolverEquation
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


addDistanceConstraintEquations : Ctx -> Body -> Body -> Float -> List SolverEquation -> List SolverEquation
addDistanceConstraintEquations ctx body1 body2 distance =
    let
        halfDistance =
            distance / 2

        ni =
            Vec3.direction (Transform3d.originPoint body2.transform3d) (Transform3d.originPoint body1.transform3d)

        ri =
            Vec3.scale halfDistance ni

        rj =
            Vec3.scale -halfDistance ni

        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))
    in
    (::)
        (initSolverParams
            (computeContactB 0
                { id = ""
                , pi = Vec3.add ri (Transform3d.originPoint body1.transform3d)
                , pj = Vec3.add rj (Transform3d.originPoint body2.transform3d)
                , ni = ni
                }
            )
            ctx
            body1
            body2
            { id = ""
            , minForce = -1000000
            , maxForce = 1000000
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            -- wA = Vec3.cross ni ri, vB = ni, wB = Vec3.cross rj ni
            , wAx = ni.y * ri.z - ni.z * ri.y
            , wAy = ni.z * ri.x - ni.x * ri.z
            , wAz = ni.x * ri.y - ni.y * ri.x
            , vBx = ni.x
            , vBy = ni.y
            , vBz = ni.z
            , wBx = rj.y * ni.z - rj.z * ni.y
            , wBy = rj.z * ni.x - rj.x * ni.z
            , wBz = rj.x * ni.y - rj.y * ni.x
            }
        )


addHingeRotationalConstraintEquations : Ctx -> Body -> Body -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addHingeRotationalConstraintEquations ctx body1 body2 axis1 axis2 equations =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

        worldAxis2 =
            Transform3d.directionPlaceIn body2.transform3d axis2

        ( ni1, ni2 ) =
            Vec3.tangents (Transform3d.directionPlaceIn body1.transform3d axis1)
    in
    equations
        |> addRotationalEquation spookA spookB spookEps ctx body1 body2 ni1 worldAxis2
        |> addRotationalEquation spookA spookB spookEps ctx body1 body2 ni2 worldAxis2


addLockRotationalConstraintEquations : Ctx -> Body -> Body -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addLockRotationalConstraintEquations ctx body1 body2 x1 x2 y1 y2 z1 z2 equations =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

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
        |> addRotationalEquation spookA spookB spookEps ctx body1 body2 worldX1 worldY2
        |> addRotationalEquation spookA spookB spookEps ctx body1 body2 worldY1 worldZ2
        |> addRotationalEquation spookA spookB spookEps ctx body1 body2 worldZ1 worldX2


addRotationalEquation : Float -> Float -> Float -> Ctx -> Body -> Body -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addRotationalEquation spookA spookB spookEps ctx body1 body2 ni nj equations =
    initSolverParams
        (computeRotationalB
            { ni = ni
            , nj = nj
            , maxAngleCos = 0 -- cos (pi / 2)
            }
        )
        ctx
        body1
        body2
        { id = ""
        , minForce = -1000000
        , maxForce = 1000000
        , solverB = 0
        , solverInvC = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        -- wA = Vec3.cross nj ni, vB = Vec3.zero, wB = Vec3.cross ni nj
        , wAx = nj.y * ni.z - nj.z * ni.y
        , wAy = nj.z * ni.x - nj.x * ni.z
        , wAz = nj.x * ni.y - nj.y * ni.x
        , vBx = 0
        , vBy = 0
        , vBz = 0
        , wBx = ni.y * nj.z - ni.z * nj.y
        , wBy = ni.z * nj.x - ni.x * nj.z
        , wBz = ni.x * nj.y - ni.y * nj.x
        }
        :: equations


addPointToPointConstraintEquations : Ctx -> Body -> Body -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addPointToPointConstraintEquations ctx body1 body2 pivot1 pivot2 equations =
    let
        ri =
            Transform3d.directionPlaceIn body1.transform3d pivot1

        rj =
            Transform3d.directionPlaceIn body2.transform3d pivot2

        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))
    in
    List.foldl
        (\ni ->
            (::)
                (initSolverParams
                    (computeContactB 0
                        { id = ""
                        , pi = Vec3.add (Transform3d.originPoint body1.transform3d) ri
                        , pj = Vec3.add (Transform3d.originPoint body2.transform3d) rj
                        , ni = ni
                        }
                    )
                    ctx
                    body1
                    body2
                    { id = ""
                    , minForce = -1000000
                    , maxForce = 1000000
                    , solverB = 0
                    , solverInvC = 0
                    , spookA = spookA
                    , spookB = spookB
                    , spookEps = spookEps
                    -- wA = Vec3.cross ni ri, vB = ni, wB = Vec3.cross rj ni
                    , wAx = ni.y * ri.z - ni.z * ri.y
                    , wAy = ni.z * ri.x - ni.x * ri.z
                    , wAz = ni.x * ri.y - ni.y * ri.x
                    , vBx = ni.x
                    , vBy = ni.y
                    , vBz = ni.z
                    , wBx = rj.y * ni.z - rj.z * ni.y
                    , wBy = rj.z * ni.x - rj.x * ni.z
                    , wBz = rj.x * ni.y - rj.y * ni.x
                    }
                )
        )
        equations
        Vec3.basis


addContactEquations : Ctx -> Body -> Body -> SolverContact -> List SolverEquation -> List SolverEquation
addContactEquations ctx body1 body2 { friction, bounciness, contact } equations =
    let
        maxFrictionForce =
            if body1.invMass + body2.invMass > 0 then
                friction * ctx.gravityLength / (body1.invMass + body2.invMass)

            else
                0

        ri =
            Vec3.sub contact.pi (Transform3d.originPoint body1.transform3d)

        rj =
            Vec3.sub contact.pj (Transform3d.originPoint body2.transform3d)

        ( t1, t2 ) =
            Vec3.tangents contact.ni

        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))
    in
    initSolverParams
        (computeContactB bounciness contact)
        ctx
        body1
        body2
        { id = contact.id
        , minForce = 0
        , maxForce = 1000000
        , solverB = 0
        , solverInvC = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        -- wA = Vec3.cross contact.ni ri, vB = contact.ni, wB = Vec3.cross rj contact.ni
        , wAx = contact.ni.y * ri.z - contact.ni.z * ri.y
        , wAy = contact.ni.z * ri.x - contact.ni.x * ri.z
        , wAz = contact.ni.x * ri.y - contact.ni.y * ri.x
        , vBx = contact.ni.x
        , vBy = contact.ni.y
        , vBz = contact.ni.z
        , wBx = rj.y * contact.ni.z - rj.z * contact.ni.y
        , wBy = rj.z * contact.ni.x - rj.x * contact.ni.z
        , wBz = rj.x * contact.ni.y - rj.y * contact.ni.x
        }
        :: initSolverParams
            computeFrictionB
            ctx
            body1
            body2
            { id = ""
            , minForce = -maxFrictionForce
            , maxForce = maxFrictionForce
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps

            -- wA = Vec3.cross t1 ri, vB = t1, wB = Vec3.cross rj t1
            , wAx = t1.y * ri.z - t1.z * ri.y
            , wAy = t1.z * ri.x - t1.x * ri.z
            , wAz = t1.x * ri.y - t1.y * ri.x
            , vBx = t1.x
            , vBy = t1.y
            , vBz = t1.z
            , wBx = rj.y * t1.z - rj.z * t1.y
            , wBy = rj.z * t1.x - rj.x * t1.z
            , wBz = rj.x * t1.y - rj.y * t1.x
            }
        :: initSolverParams
            computeFrictionB
            ctx
            body1
            body2
            { id = ""
            , minForce = -maxFrictionForce
            , maxForce = maxFrictionForce
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            -- wA = Vec3.cross t2 ri, vB = t2, wB = Vec3.cross rj t2
            , wAx = t2.y * ri.z - t2.z * ri.y
            , wAy = t2.z * ri.x - t2.x * ri.z
            , wAz = t2.x * ri.y - t2.y * ri.x
            , vBx = t2.x
            , vBy = t2.y
            , vBz = t2.z
            , wBx = rj.y * t2.z - rj.z * t2.y
            , wBy = rj.z * t2.x - rj.x * t2.z
            , wBz = rj.x * t2.y - rj.y * t2.x
            }
        :: equations


defaultRelaxation : Float
defaultRelaxation =
    3


defaultStiffness : Float
defaultStiffness =
    10000000


type alias SolverEquation =
    { equation : Equation
    , solverLambda : Float
    }


initSolverParams : ComputeB -> Ctx -> Body -> Body -> Equation -> SolverEquation
initSolverParams computeB ctx bi bj solverEquation =
    { solverLambda =
        if solverEquation.id == "" then
            0

        else
            case Dict.get solverEquation.id ctx.lambdas of
                Just lambda ->
                    lambda

                Nothing ->
                    0
    , equation =
        { id = solverEquation.id
        , minForce = solverEquation.minForce
        , maxForce = solverEquation.maxForce
        , solverB =
            -- the RHS of the SPOOK equation
            computeB bi bj solverEquation
                - (ctx.dt * computeGiMf ctx.gravity bi bj solverEquation)
        , solverInvC = 1 / (computeGimgt bi bj solverEquation + solverEquation.spookEps)
        , spookA = solverEquation.spookA
        , spookB = solverEquation.spookB
        , spookEps = solverEquation.spookEps
        , wAx = solverEquation.wAx
        , wAy = solverEquation.wAy
        , wAz = solverEquation.wAz
        , vBx = solverEquation.vBx
        , vBy = solverEquation.vBy
        , vBz = solverEquation.vBz
        , wBx = solverEquation.wBx
        , wBy = solverEquation.wBy
        , wBz = solverEquation.wBz
        }
    }


type alias ComputeB =
    Body -> Body -> Equation -> Float


computeContactB : Float -> Contact -> ComputeB
computeContactB bounciness { pi, pj, ni } bi bj equation =
    let
        g =
            ((pj.x - pi.x) * ni.x)
                + ((pj.y - pi.y) * ni.y)
                + ((pj.z - pi.z) * ni.z)

        gW =
            (bounciness + 1)
                * (Vec3.dot bj.velocity ni - Vec3.dot bi.velocity ni)
                + (bj.angularVelocity.x * equation.wBx + bj.angularVelocity.y * equation.wBy + bj.angularVelocity.z * equation.wBz)
                + (bi.angularVelocity.x * equation.wAx + bi.angularVelocity.y * equation.wAy + bi.angularVelocity.z * equation.wAz)
    in
    -g * equation.spookA - gW * equation.spookB


type alias RotationalEquation =
    { ni : Vec3
    , nj : Vec3
    , maxAngleCos : Float
    }


computeRotationalB : RotationalEquation -> ComputeB
computeRotationalB { ni, nj, maxAngleCos } bi bj ({ spookA, spookB } as solverEquation) =
    let
        g =
            maxAngleCos - Vec3.dot ni nj

        gW =
            computeGW bi bj solverEquation
    in
    -g * spookA - gW * spookB


computeFrictionB : ComputeB
computeFrictionB bi bj ({ spookB } as solverEquation) =
    let
        gW =
            computeGW bi bj solverEquation
    in
    -gW * spookB


{-| Computes G x inv(M) x f, where

  - M is the mass matrix with diagonal blocks for each body
  - f are the forces on the bodies

-}
computeGiMf : Vec3 -> Body -> Body -> Equation -> Float
computeGiMf gravity bi bj equation =
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
    -(equation.vBx * (bi.invMass * bi.force.x + gravityi.x) + equation.vBy * (bi.invMass * bi.force.y + gravityi.y) + equation.vBz * (bi.invMass * bi.force.z + gravityi.z))
        + (equation.vBx * (bj.invMass * bj.force.x + gravityj.x) + equation.vBy * (bj.invMass * bj.force.y + gravityj.y) + equation.vBz * (bj.invMass * bj.force.z + gravityj.z))
        + (equation.wAx * (bi.invInertiaWorld.m11 * bi.torque.x + bi.invInertiaWorld.m12 * bi.torque.y + bi.invInertiaWorld.m13 * bi.torque.z))
        + (equation.wAy * (bi.invInertiaWorld.m21 * bi.torque.x + bi.invInertiaWorld.m22 * bi.torque.y + bi.invInertiaWorld.m23 * bi.torque.z))
        + (equation.wAz * (bi.invInertiaWorld.m31 * bi.torque.x + bi.invInertiaWorld.m32 * bi.torque.y + bi.invInertiaWorld.m33 * bi.torque.z))
        + (equation.wBx * (bj.invInertiaWorld.m11 * bj.torque.x + bj.invInertiaWorld.m12 * bj.torque.y + bj.invInertiaWorld.m13 * bj.torque.z))
        + (equation.wBy * (bj.invInertiaWorld.m21 * bj.torque.x + bj.invInertiaWorld.m22 * bj.torque.y + bj.invInertiaWorld.m23 * bj.torque.z))
        + (equation.wBz * (bj.invInertiaWorld.m31 * bj.torque.x + bj.invInertiaWorld.m32 * bj.torque.y + bj.invInertiaWorld.m33 * bj.torque.z))


{-| Compute G x inv(M) x G', the effective inverse mass for this constraint.
-}
computeGimgt : Body -> Body -> Equation -> Float
computeGimgt bi bj equation =
    bi.invMass
        + bj.invMass
        + (equation.wAx * (bi.invInertiaWorld.m11 * equation.wAx + bi.invInertiaWorld.m12 * equation.wAy + bi.invInertiaWorld.m13 * equation.wAz))
        + (equation.wAy * (bi.invInertiaWorld.m21 * equation.wAx + bi.invInertiaWorld.m22 * equation.wAy + bi.invInertiaWorld.m23 * equation.wAz))
        + (equation.wAz * (bi.invInertiaWorld.m31 * equation.wAx + bi.invInertiaWorld.m32 * equation.wAy + bi.invInertiaWorld.m33 * equation.wAz))
        + (equation.wBx * (bj.invInertiaWorld.m11 * equation.wBx + bj.invInertiaWorld.m12 * equation.wBy + bj.invInertiaWorld.m13 * equation.wBz))
        + (equation.wBy * (bj.invInertiaWorld.m21 * equation.wBx + bj.invInertiaWorld.m22 * equation.wBy + bj.invInertiaWorld.m23 * equation.wBz))
        + (equation.wBz * (bj.invInertiaWorld.m31 * equation.wBx + bj.invInertiaWorld.m32 * equation.wBy + bj.invInertiaWorld.m33 * equation.wBz))


{-| Computes G x W, where W are the body velocities
-}
computeGW : Body -> Body -> Equation -> Float
computeGW bi bj equation =
    -(equation.vBx * bi.velocity.x + equation.vBy * bi.velocity.y + equation.vBz * bi.velocity.z)
        + (equation.wAx * bi.angularVelocity.x + equation.wAy * bi.angularVelocity.y + equation.wAz * bi.angularVelocity.z)
        + (equation.vBx * bj.velocity.x + equation.vBy * bj.velocity.y + equation.vBz * bj.velocity.z)
        + (equation.wBx * bj.angularVelocity.x + equation.wBy * bj.angularVelocity.y + equation.wBz * bj.angularVelocity.z)
