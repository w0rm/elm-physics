module Internal.SolverEquation exposing
    ( ContactEquation
    , SolverEquation
    , addSolverEquations
    , computeGWlambda
    )

import Internal.Body exposing (Body, BodyId)
import Internal.JacobianElement as JacobianElement exposing (JacobianElement)
import Internal.Material as Material
import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.SolverBody as SolverBody exposing (SolverBody)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias ContactEquation data =
    { body1 : Body data
    , body2 : Body data
    , ri : Vec3 -- vector from the center of body1 to the contact point
    , rj : Vec3 -- vector from body2 position to the contact point
    , ni : Vec3 -- contact normal, pointing out of body1
    , bounciness : Float -- u1 = -e*u0
    }


type alias FrictionEquation =
    { t : Vec3 -- tangent
    , ri : Vec3
    , rj : Vec3
    }


type EquationKind data
    = Contact (ContactEquation data)
    | Friction FrictionEquation


type alias SolverEquation data =
    { kind : EquationKind data
    , bodyId1 : BodyId
    , bodyId2 : BodyId
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


addSolverEquations : Float -> Vec3 -> ContactEquation data -> List (SolverEquation data) -> List (SolverEquation data)
addSolverEquations dt gravity contactEquation =
    let
        μg =
            Material.contactFriction
                contactEquation.body1.material
                contactEquation.body2.material
                * Vec3.length gravity

        reducedMass =
            if (contactEquation.body1.invMass + contactEquation.body2.invMass) > 0 then
                1 / (contactEquation.body1.invMass + contactEquation.body2.invMass)

            else
                0

        ( t1, t2 ) =
            Vec3.tangents contactEquation.ni

        frictionEquation1 =
            { ri = contactEquation.ri
            , rj = contactEquation.rj
            , t = t1
            }

        frictionEquation2 =
            { ri = contactEquation.ri
            , rj = contactEquation.rj
            , t = t2
            }

        rixn =
            Vec3.cross contactEquation.ri contactEquation.ni

        rjxn =
            Vec3.cross contactEquation.rj contactEquation.ni

        rixt1 =
            Vec3.cross contactEquation.ri t1

        rjxt1 =
            Vec3.cross contactEquation.rj t1

        rixt2 =
            Vec3.cross contactEquation.ri t2

        rjxt2 =
            Vec3.cross contactEquation.rj t2

        spookA =
            4.0 / (dt * (1 + 4 * defaultRelaxation))

        spookB =
            (4.0 * defaultRelaxation) / (1 + 4 * defaultRelaxation)

        spookEps =
            4.0 / (dt * dt * defaultStiffness * (1 + 4 * defaultRelaxation))
    in
    (++)
        [ { bodyId1 = contactEquation.body1.id
          , bodyId2 = contactEquation.body2.id
          , kind = Contact contactEquation
          , minForce = 0
          , maxForce = 1000000
          , solverBs = 0
          , solverInvCs = 0
          , spookA = spookA
          , spookB = spookB
          , spookEps = spookEps
          , jacobianElementA =
                { spatial = Vec3.negate contactEquation.ni
                , rotational = Vec3.negate rixn
                }
          , jacobianElementB =
                { spatial = contactEquation.ni
                , rotational = rjxn
                }
          }
            |> initSolverParams dt contactEquation.body1 contactEquation.body2
        , { bodyId1 = contactEquation.body1.id
          , bodyId2 = contactEquation.body2.id
          , kind = Friction frictionEquation1
          , minForce = -μg * reducedMass
          , maxForce = μg * reducedMass
          , solverBs = 0
          , solverInvCs = 0
          , spookA = spookA
          , spookB = spookB
          , spookEps = spookEps
          , jacobianElementA =
                { spatial = Vec3.negate t1
                , rotational = Vec3.negate rixt1
                }
          , jacobianElementB =
                { spatial = t1
                , rotational = rjxt1
                }
          }
            |> initSolverParams dt contactEquation.body1 contactEquation.body2
        , { bodyId1 = contactEquation.body1.id
          , bodyId2 = contactEquation.body2.id
          , kind = Friction frictionEquation2
          , minForce = -μg * reducedMass
          , maxForce = μg * reducedMass
          , solverBs = 0
          , solverInvCs = 0
          , spookA = spookA
          , spookB = spookB
          , spookEps = spookEps
          , jacobianElementA =
                { spatial = Vec3.negate t2
                , rotational = Vec3.negate rixt2
                }
          , jacobianElementB =
                { spatial = t2
                , rotational = rjxt2
                }
          }
            |> initSolverParams dt contactEquation.body1 contactEquation.body2
        ]


defaultRelaxation : Float
defaultRelaxation =
    3


defaultStiffness : Float
defaultStiffness =
    10000000


initSolverParams : Float -> Body data -> Body data -> SolverEquation data -> SolverEquation data
initSolverParams dt bi bj solverEquation =
    { kind = solverEquation.kind
    , bodyId1 = solverEquation.bodyId1
    , bodyId2 = solverEquation.bodyId2
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


{-| Computes the RHS of the SPOOK equation
-}
computeB : Float -> Body data -> Body data -> SolverEquation data -> Float
computeB dt bi bj solverEquation =
    case solverEquation.kind of
        Contact contactEquation ->
            computeContactB dt bi bj solverEquation contactEquation

        Friction frictionEquation ->
            computeFrictionB dt bi bj solverEquation frictionEquation


computeContactB : Float -> Body data -> Body data -> SolverEquation data -> ContactEquation data -> Float
computeContactB dt bi bj ({ spookA, spookB } as solverEquation) { bounciness, ri, rj, ni } =
    let
        g =
            bj.position
                |> Vec3.add rj
                |> Vec3.add (Vec3.negate bi.position)
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


computeFrictionB : Float -> Body data -> Body data -> SolverEquation data -> FrictionEquation -> Float
computeFrictionB dt bi bj ({ spookB } as solverEquation) _ =
    let
        gW =
            computeGW bi bj solverEquation

        giMf =
            computeGiMf bi bj solverEquation
    in
    -gW * spookB - dt * giMf


{-| Computes G\_inv(M)\_f, where

  - M is the mass matrix with diagonal blocks for each body
  - f are the forces on the bodies

-}
computeGiMf : Body data -> Body data -> SolverEquation data -> Float
computeGiMf bi bj { jacobianElementA, jacobianElementB } =
    (+)
        (JacobianElement.mulVec
            (Vec3.scale bi.invMass bi.force)
            (Mat3.transform bi.invInertiaWorld bi.torque)
            jacobianElementA
        )
        (JacobianElement.mulVec
            (Vec3.scale bj.invMass bj.force)
            (Mat3.transform bj.invInertiaWorld bj.torque)
            jacobianElementB
        )


{-| Compute G\_inv(M)\_G' + eps, the denominator part of the SPOOK equation:
-}
computeC : Body data -> Body data -> SolverEquation data -> Float
computeC bi bj { jacobianElementA, jacobianElementB, spookEps } =
    bi.invMass
        + bj.invMass
        + Vec3.dot (Mat3.transform bi.invInertiaWorld jacobianElementA.rotational) jacobianElementA.rotational
        + Vec3.dot (Mat3.transform bj.invInertiaWorld jacobianElementB.rotational) jacobianElementB.rotational
        + spookEps


{-| Computes G\*Wlambda, where W are the body velocities
-}
computeGWlambda : SolverBody data -> SolverBody data -> SolverEquation data -> Float
computeGWlambda bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec bi.vlambda bi.wlambda jacobianElementA
        + JacobianElement.mulVec bj.vlambda bj.wlambda jacobianElementB


{-| Computes G\*W, where W are the body velocities
-}
computeGW : Body data -> Body data -> SolverEquation data -> Float
computeGW bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec bi.velocity bi.angularVelocity jacobianElementA
        + JacobianElement.mulVec bj.velocity bj.angularVelocity jacobianElementB
