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


type alias EquationsGroup =
    { bodyId1 : Int
    , bodyId2 : Int
    , equations : List SolverEquation
    }


constraintEquationsGroup : Float -> Body data -> Body data -> List (Constraint CenterOfMassCoordinates) -> EquationsGroup
constraintEquationsGroup dt body1 body2 constraints =
    { bodyId1 = body1.id
    , bodyId2 = body2.id
    , equations = List.foldl (addConstraintEquations dt body1 body2) [] constraints
    }


contactEquationsGroup : Float -> Float -> ContactGroup data -> EquationsGroup
contactEquationsGroup dt gravityLength { body1, body2, contacts } =
    let
        maxFrictionForce =
            if (body1.invMass + body2.invMass) > 0 then
                Material.contactFriction body1.material body2.material
                    * gravityLength
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
    , equations = List.foldl (addContactEquations dt maxFrictionForce bounciness body1 body2) [] contacts
    }


addConstraintEquations : Float -> Body data -> Body data -> Constraint CenterOfMassCoordinates -> List SolverEquation -> List SolverEquation
addConstraintEquations dt body1 body2 constraint =
    case constraint of
        PointToPoint pivot1 pivot2 ->
            addPointToPointConstraintEquations dt body1 body2 pivot1 pivot2

        Hinge pivot1 axis1 pivot2 axis2 ->
            addPointToPointConstraintEquations dt body1 body2 pivot1 pivot2
                >> addRotationalConstraintEquations dt body1 body2 axis1 axis2

        Distance distance ->
            addDistanceConstraintEquations dt body1 body2 distance


addDistanceConstraintEquations : Float -> Body data -> Body data -> Float -> List SolverEquation -> List SolverEquation
addDistanceConstraintEquations dt body1 body2 distance =
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
            4.0 / (dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (dt * dt * defaultStiffness * (1 + 4 * defaultRelaxation))
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
            dt
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


addRotationalConstraintEquations : Float -> Body data -> Body data -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addRotationalConstraintEquations dt body1 body2 axis1 axis2 equations =
    let
        spookA =
            4.0 / (dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (dt * dt * defaultStiffness * (1 + 4 * defaultRelaxation))

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
        dt
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
            dt
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


addPointToPointConstraintEquations : Float -> Body data -> Body data -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addPointToPointConstraintEquations dt body1 body2 pivot1 pivot2 equations =
    let
        ri =
            Transform3d.directionPlaceIn body1.transform3d pivot1

        rj =
            Transform3d.directionPlaceIn body2.transform3d pivot2

        spookA =
            4.0 / (dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (dt * dt * defaultStiffness * (1 + 4 * defaultRelaxation))
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
                    dt
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


addContactEquations : Float -> Float -> Float -> Body data -> Body data -> Contact -> List SolverEquation -> List SolverEquation
addContactEquations dt maxFrictionForce bounciness body1 body2 contact equations =
    let
        ri =
            Vec3.sub contact.pi (Transform3d.originPoint body1.transform3d)

        rj =
            Vec3.sub contact.pj (Transform3d.originPoint body2.transform3d)

        ( t1, t2 ) =
            Vec3.tangents contact.ni

        spookA =
            4.0 / (dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (dt * dt * defaultStiffness * (1 + 4 * defaultRelaxation))
    in
    initSolverParams
        (computeContactB bounciness contact)
        dt
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
            dt
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
            dt
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


initSolverParams : ComputeB data -> Float -> Body data -> Body data -> Equation -> SolverEquation
initSolverParams computeB dt bi bj solverEquation =
    { solverLambda = 0
    , equation =
        { minForce = solverEquation.minForce
        , maxForce = solverEquation.maxForce
        , solverB =
            -- the RHS of the SPOOK equation
            computeB bi bj solverEquation
                - (dt * computeGiMf bi bj solverEquation)
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
computeGiMf : Body data -> Body data -> Equation -> Float
computeGiMf bi bj { wA, vB, wB } =
    -(vB.x * bi.invMass * bi.force.x + vB.y * bi.invMass * bi.force.y + vB.z * bi.invMass * bi.force.z)
        + (vB.x * bj.invMass * bj.force.x + vB.y * bj.invMass * bj.force.y + vB.z * bj.invMass * bj.force.z)
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
