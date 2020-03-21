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
import Internal.Coordinates exposing (CenterOfMassCoordinates)
import Internal.Material as Material
import Internal.Matrix3 as Mat3
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias ContactEquation =
    { ri : Vec3 -- vector from the center of body1 to the contact point
    , rj : Vec3 -- vector from body2 position to the contact point
    , ni : Vec3 -- contact normal, pointing out of body1
    , bounciness : Float -- u1 = -e*u0
    }


type alias RotationalEquation =
    { ni : Vec3
    , nj : Vec3
    , maxAngle : Float
    }


type alias FrictionEquation =
    { t : Vec3 -- tangent
    , ri : Vec3
    , rj : Vec3
    }


type EquationKind
    = Contact ContactEquation
    | Rotational RotationalEquation
    | Friction FrictionEquation


type alias Equation =
    { kind : EquationKind
    , minForce : Float
    , maxForce : Float
    , solverBs : Float
    , solverInvCs : Float
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


axes : List Vec3
axes =
    [ Vec3.i, Vec3.j, Vec3.k ]


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
        (initSolverParams dt
            body1
            body2
            { kind =
                Contact
                    { ri = ri
                    , rj = rj
                    , ni = ni
                    , bounciness = 0
                    }
            , minForce = -1000000
            , maxForce = 1000000
            , solverBs = 0
            , solverInvCs = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.negate (Vec3.cross ri ni)
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
    initSolverParams dt
        body1
        body2
        { kind =
            Rotational
                { ni = ni1
                , nj = nj1
                , maxAngle = pi / 2
                }
        , minForce = -1000000
        , maxForce = 1000000
        , solverBs = 0
        , solverInvCs = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        , wA = Vec3.cross nj1 ni1
        , vB = Vec3.zero
        , wB = Vec3.cross ni1 nj1
        }
        :: initSolverParams dt
            body1
            body2
            { kind =
                Rotational
                    { ni = ni2
                    , nj = nj2
                    , maxAngle = pi / 2
                    }
            , minForce = -1000000
            , maxForce = 1000000
            , solverBs = 0
            , solverInvCs = 0
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
                (initSolverParams dt
                    body1
                    body2
                    { kind =
                        Contact
                            { ri = ri
                            , rj = rj
                            , ni = ni
                            , bounciness = 0
                            }
                    , minForce = -1000000
                    , maxForce = 1000000
                    , solverBs = 0
                    , solverInvCs = 0
                    , spookA = spookA
                    , spookB = spookB
                    , spookEps = spookEps
                    , wA = Vec3.negate (Vec3.cross ri ni)
                    , vB = ni
                    , wB = Vec3.cross rj ni
                    }
                )
        )
        equations
        axes


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
    initSolverParams dt
        body1
        body2
        { kind =
            Contact
                { ri = ri
                , rj = rj
                , ni = contact.ni
                , bounciness = bounciness
                }
        , minForce = 0
        , maxForce = 1000000
        , solverBs = 0
        , solverInvCs = 0
        , spookA = spookA
        , spookB = spookB
        , spookEps = spookEps
        , wA = Vec3.negate (Vec3.cross ri contact.ni)
        , vB = contact.ni
        , wB = Vec3.cross rj contact.ni
        }
        :: initSolverParams dt
            body1
            body2
            { kind = Friction { ri = ri, rj = rj, t = t1 }
            , minForce = -maxFrictionForce
            , maxForce = maxFrictionForce
            , solverBs = 0
            , solverInvCs = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.negate (Vec3.cross ri t1)
            , vB = t1
            , wB = Vec3.cross rj t1
            }
        :: initSolverParams dt
            body1
            body2
            { kind = Friction { ri = ri, rj = rj, t = t2 }
            , minForce = -maxFrictionForce
            , maxForce = maxFrictionForce
            , solverBs = 0
            , solverInvCs = 0
            , spookA = spookA
            , spookB = spookB
            , spookEps = spookEps
            , wA = Vec3.negate (Vec3.cross ri t2)
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


initSolverParams : Float -> Body data -> Body data -> Equation -> SolverEquation
initSolverParams dt bi bj solverEquation =
    { solverLambda = 0
    , equation =
        { kind = solverEquation.kind
        , minForce = solverEquation.minForce
        , maxForce = solverEquation.maxForce
        , solverBs = computeB dt bi bj solverEquation
        , solverInvCs = 1 / computeC bi bj solverEquation
        , spookA = solverEquation.spookA
        , spookB = solverEquation.spookB
        , spookEps = solverEquation.spookEps
        , wA = solverEquation.wA
        , vB = solverEquation.vB
        , wB = solverEquation.wB
        }
    }


{-| Computes the RHS of the SPOOK equation
-}
computeB : Float -> Body data -> Body data -> Equation -> Float
computeB dt bi bj solverEquation =
    case solverEquation.kind of
        Contact contactEquation ->
            computeContactB dt bi bj solverEquation contactEquation

        Rotational rotationalEquation ->
            computeRotationalB dt bi bj solverEquation rotationalEquation

        Friction frictionEquation ->
            computeFrictionB dt bi bj solverEquation frictionEquation


computeContactB : Float -> Body data -> Body data -> Equation -> ContactEquation -> Float
computeContactB dt bi bj ({ spookA, spookB } as solverEquation) { bounciness, ri, rj, ni } =
    let
        g =
            Transform3d.originPoint bj.transform3d
                |> Vec3.add rj
                |> Vec3.add (Vec3.negate (Transform3d.originPoint bi.transform3d))
                |> Vec3.add (Vec3.negate ri)
                |> Vec3.dot ni

        gW =
            (bounciness + 1)
                * (Vec3.dot bj.velocity ni - Vec3.dot bi.velocity ni)
                + Vec3.dot bj.angularVelocity (Vec3.cross rj ni)
                - Vec3.dot bi.angularVelocity (Vec3.cross ri ni)

        giMf =
            computeGiMf bi bj solverEquation
    in
    -g * spookA - gW * spookB - dt * giMf


computeRotationalB : Float -> Body data -> Body data -> Equation -> RotationalEquation -> Float
computeRotationalB dt bi bj ({ spookA, spookB } as solverEquation) { ni, nj, maxAngle } =
    let
        g =
            cos maxAngle - Vec3.dot ni nj

        gW =
            computeGW bi bj solverEquation

        giMf =
            computeGiMf bi bj solverEquation
    in
    -g * spookA - gW * spookB - dt * giMf


computeFrictionB : Float -> Body data -> Body data -> Equation -> FrictionEquation -> Float
computeFrictionB dt bi bj ({ spookB } as solverEquation) _ =
    let
        gW =
            computeGW bi bj solverEquation

        giMf =
            computeGiMf bi bj solverEquation
    in
    -gW * spookB - dt * giMf


{-| Computes G x inv(M) x f, where

  - M is the mass matrix with diagonal blocks for each body
  - f are the forces on the bodies

-}
computeGiMf : Body data -> Body data -> Equation -> Float
computeGiMf bi bj { wA, vB, wB } =
    let
        biV =
            Vec3.scale bi.invMass bi.force

        biW =
            Mat3.transform bi.invInertiaWorld bi.torque

        bjV =
            Vec3.scale bj.invMass bj.force

        bjW =
            Mat3.transform bj.invInertiaWorld bj.torque
    in
    -(vB.x * biV.x + vB.y * biV.y + vB.z * biV.z)
        + (wA.x * biW.x + wA.y * biW.y + wA.z * biW.z)
        + (vB.x * bjV.x + vB.y * bjV.y + vB.z * bjV.z)
        + (wB.x * bjW.x + wB.y * bjW.y + wB.z * bjW.z)


{-| Compute G x inv(M) x G' + eps, the denominator part of the SPOOK equation:
-}
computeC : Body data -> Body data -> Equation -> Float
computeC bi bj { wA, wB, spookEps } =
    bi.invMass
        + bj.invMass
        + Vec3.dot (Mat3.transform bi.invInertiaWorld wA) wA
        + Vec3.dot (Mat3.transform bj.invInertiaWorld wB) wB
        + spookEps


{-| Computes G x W, where W are the body velocities
-}
computeGW : Body data -> Body data -> Equation -> Float
computeGW bi bj { wA, vB, wB } =
    -(vB.x * bi.velocity.x + vB.y * bi.velocity.y + vB.z * bi.velocity.z)
        + (wA.x * bi.angularVelocity.x + wA.y * bi.angularVelocity.y + wA.z * bi.angularVelocity.z)
        + (vB.x * bj.velocity.x + vB.y * bj.velocity.y + vB.z * bj.velocity.z)
        + (wB.x * bj.angularVelocity.x + wB.y * bj.angularVelocity.y + wB.z * bj.angularVelocity.z)
