module Internal.Equation exposing
    ( Equation
    , EquationsGroup
    , SolverEquation
    , computeGWlambda
    , constraintEquationsGroup
    , contactEquationsGroup
    )

import Frame3d
import Internal.Body exposing (Body)
import Internal.Constraint exposing (Constraint(..))
import Internal.Contact exposing (Contact, ContactGroup)
import Internal.JacobianElement as JacobianElement exposing (JacobianElement)
import Internal.Material as Material
import Internal.Matrix3 as Mat3
import Internal.SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (CenterOfMassCoordinates)
import Point3d
import Vector3d


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
    , jacobianElementA : JacobianElement
    , jacobianElementB : JacobianElement
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


contactEquationsGroup : Float -> Vec3 -> ContactGroup data -> EquationsGroup
contactEquationsGroup dt gravity { body1, body2, contacts } =
    let
        mug =
            Material.contactFriction
                body1.material
                body2.material
                * Vec3.length gravity

        reducedMass =
            if (body1.invMass + body2.invMass) > 0 then
                1 / (body1.invMass + body2.invMass)

            else
                0

        maxFrictionForce =
            mug * reducedMass

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
        PointToPoint { pivot1, pivot2 } ->
            addPointToPointConstraintEquations dt body1 body2 pivot1 pivot2

        Hinge { pivot1, axis1, pivot2, axis2 } ->
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
            Vec3.direction (Point3d.toMeters (Frame3d.originPoint body2.frame3d)) (Point3d.toMeters (Frame3d.originPoint body1.frame3d))

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
            , jacobianElementA =
                { spatial = Vec3.negate ni
                , rotational = Vec3.negate (Vec3.cross ri ni)
                }
            , jacobianElementB =
                { spatial = ni
                , rotational = Vec3.cross rj ni
                }
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
            Vector3d.toMeters (Vector3d.placeIn body1.frame3d (Vector3d.fromMeters axis1))

        worldAxis2 =
            Vector3d.toMeters (Vector3d.placeIn body2.frame3d (Vector3d.fromMeters axis2))

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
        , jacobianElementA =
            { spatial = Vec3.zero
            , rotational = Vec3.cross nj1 ni1
            }
        , jacobianElementB =
            { spatial = Vec3.zero
            , rotational = Vec3.cross ni1 nj1
            }
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
            , jacobianElementA =
                { spatial = Vec3.zero
                , rotational = Vec3.cross nj2 ni2
                }
            , jacobianElementB =
                { spatial = Vec3.zero
                , rotational = Vec3.cross ni2 nj2
                }
            }
        :: equations


addPointToPointConstraintEquations : Float -> Body data -> Body data -> Vec3 -> Vec3 -> List SolverEquation -> List SolverEquation
addPointToPointConstraintEquations dt body1 body2 pivot1 pivot2 equations =
    let
        ri =
            Vector3d.toMeters (Vector3d.placeIn body1.frame3d (Vector3d.fromMeters pivot1))

        rj =
            Vector3d.toMeters (Vector3d.placeIn body2.frame3d (Vector3d.fromMeters pivot2))

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
                    , jacobianElementA =
                        { spatial = Vec3.negate ni
                        , rotational = Vec3.negate (Vec3.cross ri ni)
                        }
                    , jacobianElementB =
                        { spatial = ni
                        , rotational = Vec3.cross rj ni
                        }
                    }
                )
        )
        equations
        axes


addContactEquations : Float -> Float -> Float -> Body data -> Body data -> Contact -> List SolverEquation -> List SolverEquation
addContactEquations dt maxFrictionForce bounciness body1 body2 contact equations =
    let
        ri =
            Vec3.sub contact.pi (Point3d.toMeters (Frame3d.originPoint body1.frame3d))

        rj =
            Vec3.sub contact.pj (Point3d.toMeters (Frame3d.originPoint body2.frame3d))

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
        , jacobianElementA =
            { spatial = Vec3.negate contact.ni
            , rotational = Vec3.negate (Vec3.cross ri contact.ni)
            }
        , jacobianElementB =
            { spatial = contact.ni
            , rotational = Vec3.cross rj contact.ni
            }
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
            , jacobianElementA =
                { spatial = Vec3.negate t1
                , rotational = Vec3.negate (Vec3.cross ri t1)
                }
            , jacobianElementB =
                { spatial = t1
                , rotational = Vec3.cross rj t1
                }
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
            , jacobianElementA =
                { spatial = Vec3.negate t2
                , rotational = Vec3.negate (Vec3.cross ri t2)
                }
            , jacobianElementB =
                { spatial = t2
                , rotational = Vec3.cross rj t2
                }
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
        , jacobianElementA = solverEquation.jacobianElementA
        , jacobianElementB = solverEquation.jacobianElementB
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
            Point3d.toMeters (Frame3d.originPoint bj.frame3d)
                |> Vec3.add rj
                |> Vec3.add (Vec3.negate (Point3d.toMeters (Frame3d.originPoint bi.frame3d)))
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
computeGiMf bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec
        (Vec3.scale bi.invMass bi.force)
        (Mat3.transform bi.invInertiaWorld bi.torque)
        jacobianElementA
        + JacobianElement.mulVec
            (Vec3.scale bj.invMass bj.force)
            (Mat3.transform bj.invInertiaWorld bj.torque)
            jacobianElementB


{-| Compute G\_inv(M)\_G' + eps, the denominator part of the SPOOK equation:
-}
computeC : Body data -> Body data -> Equation -> Float
computeC bi bj { jacobianElementA, jacobianElementB, spookEps } =
    bi.invMass
        + bj.invMass
        + Vec3.dot (Mat3.transform bi.invInertiaWorld jacobianElementA.rotational) jacobianElementA.rotational
        + Vec3.dot (Mat3.transform bj.invInertiaWorld jacobianElementB.rotational) jacobianElementB.rotational
        + spookEps


{-| Computes G\*Wlambda, where W are the body velocities
-}
computeGWlambda : SolverBody data -> SolverBody data -> Equation -> Float
computeGWlambda bi bj { jacobianElementA, jacobianElementB } =
    {-
       JacobianElement.mulVec bi.vlambda bi.wlambda jacobianElementA
           + JacobianElement.mulVec bj.vlambda bj.wlambda jacobianElementB
    -}
    (jacobianElementA.spatial.x * bi.vlambda.x + jacobianElementA.spatial.y * bi.vlambda.y + jacobianElementA.spatial.z * bi.vlambda.z)
        + (jacobianElementA.rotational.x * bi.wlambda.x + jacobianElementA.rotational.y * bi.wlambda.y + jacobianElementA.rotational.z * bi.wlambda.z)
        + (jacobianElementB.spatial.x * bj.vlambda.x + jacobianElementB.spatial.y * bj.vlambda.y + jacobianElementB.spatial.z * bj.vlambda.z)
        + (jacobianElementB.rotational.x * bj.wlambda.x + jacobianElementB.rotational.y * bj.wlambda.y + jacobianElementB.rotational.z * bj.wlambda.z)


{-| Computes G\*W, where W are the body velocities
-}
computeGW : Body data -> Body data -> Equation -> Float
computeGW bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec bi.velocity bi.angularVelocity jacobianElementA
        + JacobianElement.mulVec bj.velocity bj.angularVelocity jacobianElementB
