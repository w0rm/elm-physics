module Internal.Equation exposing
    ( Equation
    , EquationsGroup
    , SolverEquation
    , constraintEquationsGroup
    , contactEquationsGroup
    )

import Internal.Body exposing (Body)
import Internal.Constraint exposing (Constraint(..))
import Internal.Contact exposing (Contact, ContactGroup)
import Internal.Material as Material
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias Equation =
    { minForce : Float
    , maxForce : Float
    , solverB : Float
    , solverInvC : Float
    , spookA : Float
    , spookB : Float
    , spookEps : Float
    , wA : Vec3
    , vB : Vec3 -- vA = Vec3.negate vB
    , wB : Vec3
    }


type alias Ctx =
    { dt : Float
    , gravity : Vec3
    , gravityLength : Float
    }


type alias EquationsGroup =
    { bodyId1 : Int
    , bodyId2 : Int
    , equations : List SolverEquation
    }


constraintEquationsGroup : Ctx -> Body data -> Body data -> List (Constraint CenterOfMassCoordinates) -> EquationsGroup
constraintEquationsGroup ctx body1 body2 constraints =
    { bodyId1 = body1.id
    , bodyId2 = body2.id
    , equations = List.foldl (addConstraintEquations ctx body1 body2) [] constraints
    }


contactEquationsGroup : Ctx -> ContactGroup data -> EquationsGroup
contactEquationsGroup ctx { body1, body2, contacts } =
    let
        maxFrictionForce =
            if (body1.invMass + body2.invMass) > 0 then
                Material.contactFriction body1.material body2.material
                    * ctx.gravityLength
                    / (body1.invMass + body2.invMass)

            else
                0

        bounciness =
            Material.contactBounciness
                body1.material
                body2.material
    in
    { bodyId1 = body1.id
    , bodyId2 = body2.id
    , equations = List.foldl (addContactEquations ctx maxFrictionForce bounciness body1 body2) [] contacts
    }


addConstraintEquations : Ctx -> Body data -> Body data -> Constraint CenterOfMassCoordinates -> List SolverEquation -> List SolverEquation
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


addDistanceConstraintEquations : Ctx -> Body data -> Body data -> Float -> List SolverEquation -> List SolverEquation
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
            (computeContactB
                0
                { pi = Vec3.add ri (Transform3d.originPoint body1.transform3d)
                , pj = Vec3.add rj (Transform3d.originPoint body2.transform3d)
                , ni = ni
                }
            )
            ctx
            body1
            body2
            { minForce = -1000000
            , maxForce = 1000000
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.cross ni ri
            , vB = ni
            , wB = Vec3.cross rj ni
            }
        )


addHingeRotationalConstraintEquations : Ctx -> Body data -> Body data -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addHingeRotationalConstraintEquations ctx body1 body2 axis1 axis2 equations =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

        worldAxis1 =
            Transform3d.directionPlaceIn body1.transform3d axis1

        worldAxis2 =
            Transform3d.directionPlaceIn body2.transform3d axis2

        ( ni1, ni2 ) =
            Vec3.tangents worldAxis1

        nj1 =
            worldAxis2

        nj2 =
            worldAxis2
    in
    initSolverParams
        (computeRotationalB
            { ni = ni1
            , nj = nj1
            , maxAngleCos = 0 -- cos (pi / 2)
            }
        )
        ctx
        body1
        body2
        { minForce = -1000000
        , maxForce = 1000000
        , solverB = 0
        , solverInvC = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        , wA = Vec3.cross nj1 ni1
        , vB = Vec3.zero
        , wB = Vec3.cross ni1 nj1
        }
        :: initSolverParams
            (computeRotationalB
                { ni = ni2
                , nj = nj2
                , maxAngleCos = 0 -- cos (pi / 2)
                }
            )
            ctx
            body1
            body2
            { minForce = -1000000
            , maxForce = 1000000
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.cross nj2 ni2
            , vB = Vec3.zero
            , wB = Vec3.cross ni2 nj2
            }
        :: equations


addLockRotationalConstraintEquations : Ctx -> Body data -> Body data -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addLockRotationalConstraintEquations ctx body1 body2 x1 x2 y1 y2 z1 z2 equations =
    let
        spookA =
            4.0 / (ctx.dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (ctx.dt * ctx.dt * defaultStiffness * (1 + 4 * defaultRelaxation))

        ni1 =
            Transform3d.directionPlaceIn body1.transform3d x1

        nj1 =
            Transform3d.directionPlaceIn body2.transform3d y2

        ni2 =
            Transform3d.directionPlaceIn body1.transform3d y1

        nj2 =
            Transform3d.directionPlaceIn body2.transform3d z2

        ni3 =
            Transform3d.directionPlaceIn body1.transform3d z1

        nj3 =
            Transform3d.directionPlaceIn body2.transform3d x2
    in
    initSolverParams
        (computeRotationalB
            { ni = ni1
            , nj = nj1
            , maxAngleCos = 0 -- cos (pi / 2)
            }
        )
        ctx
        body1
        body2
        { minForce = -1000000
        , maxForce = 1000000
        , solverB = 0
        , solverInvC = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        , wA = Vec3.cross nj1 ni1
        , vB = Vec3.zero
        , wB = Vec3.cross ni1 nj1
        }
        :: initSolverParams
            (computeRotationalB
                { ni = ni2
                , nj = nj2
                , maxAngleCos = 0 -- cos (pi / 2)
                }
            )
            ctx
            body1
            body2
            { minForce = -1000000
            , maxForce = 1000000
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.cross nj2 ni2
            , vB = Vec3.zero
            , wB = Vec3.cross ni2 nj2
            }
        :: initSolverParams
            (computeRotationalB
                { ni = ni3
                , nj = nj3
                , maxAngleCos = 0 -- cos (pi / 2)
                }
            )
            ctx
            body1
            body2
            { minForce = -1000000
            , maxForce = 1000000
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.cross nj3 ni3
            , vB = Vec3.zero
            , wB = Vec3.cross ni3 nj3
            }
        :: equations


addPointToPointConstraintEquations : Ctx -> Body data -> Body data -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
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
                        { pi = Vec3.add (Transform3d.originPoint body1.transform3d) ri
                        , pj = Vec3.add (Transform3d.originPoint body2.transform3d) rj
                        , ni = ni
                        }
                    )
                    ctx
                    body1
                    body2
                    { minForce = -1000000
                    , maxForce = 1000000
                    , solverB = 0
                    , solverInvC = 0
                    , spookA = spookA
                    , spookB = spookB
                    , spookEps = spookEps
                    , wA = Vec3.cross ni ri
                    , vB = ni
                    , wB = Vec3.cross rj ni
                    }
                )
        )
        equations
        Vec3.basis


addContactEquations : Ctx -> Float -> Float -> Body data -> Body data -> Contact -> List SolverEquation -> List SolverEquation
addContactEquations ctx maxFrictionForce bounciness body1 body2 contact equations =
    let
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
        { minForce = 0
        , maxForce = 1000000
        , solverB = 0
        , solverInvC = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        , wA = Vec3.cross contact.ni ri
        , vB = contact.ni
        , wB = Vec3.cross rj contact.ni
        }
        :: initSolverParams
            computeFrictionB
            ctx
            body1
            body2
            { minForce = -maxFrictionForce
            , maxForce = maxFrictionForce
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.cross t1 ri
            , vB = t1
            , wB = Vec3.cross rj t1
            }
        :: initSolverParams
            computeFrictionB
            ctx
            body1
            body2
            { minForce = -maxFrictionForce
            , maxForce = maxFrictionForce
            , solverB = 0
            , solverInvC = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.cross t2 ri
            , vB = t2
            , wB = Vec3.cross rj t2
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


initSolverParams : ComputeB data -> Ctx -> Body data -> Body data -> Equation -> SolverEquation
initSolverParams computeB ctx bi bj solverEquation =
    { solverLambda = 0
    , equation =
        { minForce = solverEquation.minForce
        , maxForce = solverEquation.maxForce
        , solverB =
            -- the RHS of the SPOOK equation
            computeB bi bj solverEquation
                - (ctx.dt * computeGiMf ctx.gravity bi bj solverEquation)
        , solverInvC = 1 / computeC bi bj solverEquation
        , spookA = solverEquation.spookA
        , spookB = solverEquation.spookB
        , spookEps = solverEquation.spookEps
        , wA = solverEquation.wA
        , vB = solverEquation.vB
        , wB = solverEquation.wB
        }
    }


type alias ComputeB data =
    Body data -> Body data -> Equation -> Float


computeContactB : Float -> Contact -> ComputeB data
computeContactB bounciness { pi, pj, ni } bi bj { spookA, spookB, wA, wB } =
    let
        g =
            ((pj.x - pi.x) * ni.x)
                + ((pj.y - pi.y) * ni.y)
                + ((pj.z - pi.z) * ni.z)

        gW =
            (bounciness + 1)
                * (Vec3.dot bj.velocity ni - Vec3.dot bi.velocity ni)
                + Vec3.dot bj.angularVelocity wB
                + Vec3.dot bi.angularVelocity wA
    in
    -g * spookA - gW * spookB


type alias RotationalEquation =
    { ni : Vec3
    , nj : Vec3
    , maxAngleCos : Float
    }


computeRotationalB : RotationalEquation -> ComputeB data
computeRotationalB { ni, nj, maxAngleCos } bi bj ({ spookA, spookB } as solverEquation) =
    let
        g =
            maxAngleCos - Vec3.dot ni nj

        gW =
            computeGW bi bj solverEquation
    in
    -g * spookA - gW * spookB


computeFrictionB : ComputeB data
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
computeGiMf : Vec3 -> Body data -> Body data -> Equation -> Float
computeGiMf gravity bi bj { wA, vB, wB } =
    let
        gravityi =
            if bi.mass > 0 then
                gravity

            else
                Vec3.zero

        gravityj =
            if bj.mass > 0 then
                gravity

            else
                Vec3.zero
    in
    -(vB.x * (bi.invMass * bi.force.x + gravityi.x) + vB.y * (bi.invMass * bi.force.y + gravityi.y) + vB.z * (bi.invMass * bi.force.z + gravityi.z))
        + (vB.x * (bj.invMass * bj.force.x + gravityj.x) + vB.y * (bj.invMass * bj.force.y + gravityj.y) + vB.z * (bj.invMass * bj.force.z + gravityj.z))
        + (wA.x * (bi.invInertiaWorld.m11 * bi.torque.x + bi.invInertiaWorld.m12 * bi.torque.y + bi.invInertiaWorld.m13 * bi.torque.z))
        + (wA.y * (bi.invInertiaWorld.m21 * bi.torque.x + bi.invInertiaWorld.m22 * bi.torque.y + bi.invInertiaWorld.m23 * bi.torque.z))
        + (wA.z * (bi.invInertiaWorld.m31 * bi.torque.x + bi.invInertiaWorld.m32 * bi.torque.y + bi.invInertiaWorld.m33 * bi.torque.z))
        + (wB.x * (bj.invInertiaWorld.m11 * bj.torque.x + bj.invInertiaWorld.m12 * bj.torque.y + bj.invInertiaWorld.m13 * bj.torque.z))
        + (wB.y * (bj.invInertiaWorld.m21 * bj.torque.x + bj.invInertiaWorld.m22 * bj.torque.y + bj.invInertiaWorld.m23 * bj.torque.z))
        + (wB.z * (bj.invInertiaWorld.m31 * bj.torque.x + bj.invInertiaWorld.m32 * bj.torque.y + bj.invInertiaWorld.m33 * bj.torque.z))


{-| Compute G x inv(M) x G' + eps, the denominator part of the SPOOK equation:
-}
computeC : Body data -> Body data -> Equation -> Float
computeC bi bj { wA, wB, spookEps } =
    bi.invMass
        + bj.invMass
        + (wA.x * (bi.invInertiaWorld.m11 * wA.x + bi.invInertiaWorld.m12 * wA.y + bi.invInertiaWorld.m13 * wA.z))
        + (wA.y * (bi.invInertiaWorld.m21 * wA.x + bi.invInertiaWorld.m22 * wA.y + bi.invInertiaWorld.m23 * wA.z))
        + (wA.z * (bi.invInertiaWorld.m31 * wA.x + bi.invInertiaWorld.m32 * wA.y + bi.invInertiaWorld.m33 * wA.z))
        + (wB.x * (bj.invInertiaWorld.m11 * wB.x + bj.invInertiaWorld.m12 * wB.y + bj.invInertiaWorld.m13 * wB.z))
        + (wB.y * (bj.invInertiaWorld.m21 * wB.x + bj.invInertiaWorld.m22 * wB.y + bj.invInertiaWorld.m23 * wB.z))
        + (wB.z * (bj.invInertiaWorld.m31 * wB.x + bj.invInertiaWorld.m32 * wB.y + bj.invInertiaWorld.m33 * wB.z))
        + spookEps


{-| Computes G x W, where W are the body velocities
-}
computeGW : Body data -> Body data -> Equation -> Float
computeGW bi bj { wA, vB, wB } =
    -(vB.x * bi.velocity.x + vB.y * bi.velocity.y + vB.z * bi.velocity.z)
        + (wA.x * bi.angularVelocity.x + wA.y * bi.angularVelocity.y + wA.z * bi.angularVelocity.z)
        + (vB.x * bj.velocity.x + vB.y * bj.velocity.y + vB.z * bj.velocity.z)
        + (wB.x * bj.angularVelocity.x + wB.y * bj.angularVelocity.y + wB.z * bj.angularVelocity.z)
