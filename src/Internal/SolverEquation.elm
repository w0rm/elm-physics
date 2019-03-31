module Internal.SolverEquation exposing
    ( SolverEquation
    , addContactEquation
    , addFrictionEquations
    , computeGWlambda
    )

import Internal.Matrix4 as Mat4 exposing (Mat4)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.Body exposing (BodyId)
import Internal.ContactEquation as ContactEquation exposing (ContactEquation)
import Internal.JacobianElement as JacobianElement exposing (JacobianElement)
import Internal.SolverBody as SolverBody exposing (SolverBody)


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
    , solverLambda : Float
    , solverBs : Float
    , solverInvCs : Float
    , spookA : Float
    , spookB : Float
    , spookEps : Float
    , jacobianElementA : JacobianElement
    , jacobianElementB : JacobianElement
    }


addContactEquation : Float -> Vec3 -> SolverBody data -> SolverBody data -> ContactEquation data -> List (SolverEquation data) -> List (SolverEquation data)
addContactEquation dt gravity bi bj contactEquation =
    let
        rixn =
            Vec3.cross contactEquation.ri contactEquation.ni

        rjxn =
            Vec3.cross contactEquation.rj contactEquation.ni
    in
    { bodyId1 = contactEquation.body1.id
    , bodyId2 = contactEquation.body2.id
    , kind = Contact contactEquation
    , minForce = 0
    , maxForce = 1000000
    , solverLambda = 0
    , solverBs = 0
    , solverInvCs = 0
    , spookA = 0
    , spookB = 0
    , spookEps = 0
    , jacobianElementA =
        { spatial = Vec3.negate contactEquation.ni
        , rotational = Vec3.negate rixn
        }
    , jacobianElementB =
        { spatial = contactEquation.ni
        , rotational = rjxn
        }
    }
        |> setSpookParams 1.0e7 4 dt
        |> initSolverParams dt bi bj
        |> (::)


addFrictionEquations : Float -> Vec3 -> SolverBody data -> SolverBody data -> ContactEquation data -> List (SolverEquation data) -> List (SolverEquation data)
addFrictionEquations dt gravity bi bj contactEquation =
    let
        mug =
            defaultContactMaterial.friction * Vec3.length gravity

        reducedMass =
            if (bi.body.invMass + bj.body.invMass) > 0 then
                1 / (bi.body.invMass + bj.body.invMass)

            else
                0

        ( t1, t2 ) =
            tangents contactEquation.ni

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

        rixt1 =
            Vec3.cross contactEquation.ri t1

        rjxt1 =
            Vec3.cross contactEquation.rj t1

        rixt2 =
            Vec3.cross contactEquation.ri t2

        rjxt2 =
            Vec3.cross contactEquation.rj t2
    in
    (++)
        [ { bodyId1 = contactEquation.body1.id
          , bodyId2 = contactEquation.body2.id
          , kind = Friction frictionEquation1
          , minForce = -mug * reducedMass
          , maxForce = mug * reducedMass
          , solverLambda = 0
          , solverBs = 0
          , solverInvCs = 0
          , spookA = 0
          , spookB = 0
          , spookEps = 0
          , jacobianElementA =
                { spatial = Vec3.negate t1
                , rotational = Vec3.negate rixt1
                }
          , jacobianElementB =
                { spatial = t1
                , rotational = rjxt1
                }
          }
            |> setSpookParams
                defaultContactMaterial.frictionEquationStiffness
                defaultContactMaterial.frictionEquationRelaxation
                dt
            |> initSolverParams dt bi bj
        , { bodyId1 = contactEquation.body1.id
          , bodyId2 = contactEquation.body2.id
          , kind = Friction frictionEquation2
          , minForce = -mug * reducedMass
          , maxForce = mug * reducedMass
          , solverLambda = 0
          , solverBs = 0
          , solverInvCs = 0
          , spookA = 0
          , spookB = 0
          , spookEps = 0
          , jacobianElementA =
                { spatial = Vec3.negate t2
                , rotational = Vec3.negate rixt2
                }
          , jacobianElementB =
                { spatial = t2
                , rotational = rjxt2
                }
          }
            |> setSpookParams
                defaultContactMaterial.frictionEquationStiffness
                defaultContactMaterial.frictionEquationRelaxation
                dt
            |> initSolverParams dt bi bj
        ]


setSpookParams : Float -> Float -> Float -> SolverEquation data -> SolverEquation data
setSpookParams stiffness relaxation timeStep solverEquation =
    { solverEquation
        | spookA = 4.0 / (timeStep * (1 + 4 * relaxation))
        , spookB = (4.0 * relaxation) / (1 + 4 * relaxation)
        , spookEps = 4.0 / (timeStep * timeStep * stiffness * (1 + 4 * relaxation))
    }


initSolverParams : Float -> SolverBody data -> SolverBody data -> SolverEquation data -> SolverEquation data
initSolverParams dt bi bj solverEquation =
    { solverEquation
        | solverLambda = 0
        , solverBs = computeB dt bi bj solverEquation
        , solverInvCs = 1 / computeC bi bj solverEquation
    }


{-| Computes the RHS of the SPOOK equation
-}
computeB : Float -> SolverBody data -> SolverBody data -> SolverEquation data -> Float
computeB dt bi bj solverEquation =
    case solverEquation.kind of
        Contact contactEquation ->
            computeContactB dt bi bj solverEquation contactEquation

        Friction frictionEquation ->
            computeFrictionB dt bi bj solverEquation frictionEquation


computeContactB : Float -> SolverBody data -> SolverBody data -> SolverEquation data -> ContactEquation data -> Float
computeContactB dt bi bj ({ spookA, spookB } as solverEquation) { restitution, ri, rj, ni } =
    let
        g =
            bj.body.position
                |> Vec3.add rj
                |> Vec3.add (Vec3.negate bi.body.position)
                |> Vec3.add (Vec3.negate ri)
                |> Vec3.dot ni

        gW =
            (restitution + 1)
                * (Vec3.dot bj.body.velocity ni - Vec3.dot bi.body.velocity ni)
                + Vec3.dot bj.body.angularVelocity (Vec3.cross rj ni)
                - Vec3.dot bi.body.angularVelocity (Vec3.cross ri ni)

        giMf =
            computeGiMf bi bj solverEquation
    in
    -g * spookA - gW * spookB - dt * giMf


computeFrictionB : Float -> SolverBody data -> SolverBody data -> SolverEquation data -> FrictionEquation -> Float
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
computeGiMf : SolverBody data -> SolverBody data -> SolverEquation data -> Float
computeGiMf bi bj { jacobianElementA, jacobianElementB } =
    (+)
        (JacobianElement.mulVec
            (Vec3.scale bi.body.invMass bi.body.force)
            (Mat4.transform bi.body.invInertiaWorld bi.body.torque)
            jacobianElementA
        )
        (JacobianElement.mulVec
            (Vec3.scale bj.body.invMass bj.body.force)
            (Mat4.transform bj.body.invInertiaWorld bj.body.torque)
            jacobianElementB
        )


{-| Compute G\_inv(M)\_G' + eps, the denominator part of the SPOOK equation:
-}
computeC : SolverBody data -> SolverBody data -> SolverEquation data -> Float
computeC bi bj { jacobianElementA, jacobianElementB, spookEps } =
    bi.body.invMass
        + bj.body.invMass
        + Vec3.dot (Mat4.transform bi.body.invInertiaWorld jacobianElementA.rotational) jacobianElementA.rotational
        + Vec3.dot (Mat4.transform bj.body.invInertiaWorld jacobianElementB.rotational) jacobianElementB.rotational
        + spookEps


{-| Computes G\*Wlambda, where W are the body velocities
-}
computeGWlambda : SolverBody data -> SolverBody data -> SolverEquation data -> Float
computeGWlambda bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec bi.vlambda bi.wlambda jacobianElementA
        + JacobianElement.mulVec bj.vlambda bj.wlambda jacobianElementB


{-| Computes G\*W, where W are the body velocities
-}
computeGW : SolverBody data -> SolverBody data -> SolverEquation data -> Float
computeGW bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec bi.body.velocity bi.body.angularVelocity jacobianElementA
        + JacobianElement.mulVec bj.body.velocity bj.body.angularVelocity jacobianElementB


type alias ContactMaterial =
    { contactEquationRelaxation : Float
    , contactEquationStiffness : Float
    , friction : Float
    , frictionEquationRelaxation : Float
    , frictionEquationStiffness : Float
    , restitution : Float

    -- materials - todo?
    }


defaultContactMaterial : ContactMaterial
defaultContactMaterial =
    { contactEquationRelaxation = 5
    , contactEquationStiffness = 10000000
    , friction = 0.3
    , frictionEquationRelaxation = 3
    , frictionEquationStiffness = 10000000
    , restitution = 0
    }


tangents : Vec3 -> ( Vec3, Vec3 )
tangents vec =
    if Vec3.length vec > 0 then
        let
            normalized =
                Vec3.normalize vec

            v =
                if abs normalized.x < 0.9 then
                    Vec3.cross normalized Vec3.i

                else
                    Vec3.cross normalized Vec3.j
        in
        ( v, Vec3.cross normalized v )

    else
        ( Vec3.i, Vec3.j )
