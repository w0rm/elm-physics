module Physics.SolverEquation exposing (..)

import Physics.JacobianElement as JacobianElement exposing (JacobianElement)
import Physics.ContactEquation as ContactEquation exposing (ContactEquation)
import Physics.Body exposing (BodyId)
import Physics.SolverBody as SolverBody exposing (SolverBody)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Mat3 as Mat3 exposing (Mat3)
import Time exposing (Time)
import Dict exposing (Dict)


type alias SolverEquation =
    { bodyId1 : BodyId
    , bodyId2 : BodyId
    , ri : Vec3 -- vector from the center of body1 to the contact point
    , rj : Vec3 -- vector from body2 position to the contact point
    , ni : Vec3 -- contact normal, pointing out of body1
    , restitution : Float -- bounciness
    , solverLambda : Float
    , solverBs : Float
    , solverInvCs : Float
    , spookA : Float
    , spookB : Float
    , spookEps : Float
    , jacobianElementA : JacobianElement
    , jacobianElementB : JacobianElement
    }


fromJust : Maybe a -> a
fromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            Debug.crash ""


fromContactEquation : Time -> Dict BodyId SolverBody -> ContactEquation -> SolverEquation
fromContactEquation dt solverBodies contactEquation =
    let
        bi =
            Dict.get contactEquation.bodyId1 solverBodies
                |> fromJust

        bj =
            Dict.get contactEquation.bodyId2 solverBodies
                |> fromJust

        rixn =
            Vec3.cross contactEquation.ri contactEquation.ni

        rjxn =
            Vec3.cross contactEquation.rj contactEquation.ni
    in
        { bodyId1 = contactEquation.bodyId1
        , bodyId2 = contactEquation.bodyId2
        , ri = contactEquation.ri
        , rj = contactEquation.rj
        , ni = contactEquation.ni
        , restitution = 0
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


setSpookParams : Float -> Float -> Float -> SolverEquation -> SolverEquation
setSpookParams stiffness relaxation timeStep solverEquation =
    { solverEquation
        | spookA = 4.0 / (timeStep * (1 + 4 * relaxation))
        , spookB = (4.0 * relaxation) / (1 + 4 * relaxation)
        , spookEps = 4.0 / (timeStep * timeStep * stiffness * (1 + 4 * relaxation))
    }


initSolverParams : Time -> SolverBody -> SolverBody -> SolverEquation -> SolverEquation
initSolverParams dt bi bj solverEquation =
    { solverEquation
        | solverLambda = 0
        , solverBs = computeB dt bi bj solverEquation
        , solverInvCs = 1 / computeC bi bj solverEquation
    }


computeB : Time -> SolverBody -> SolverBody -> SolverEquation -> Float
computeB dt bi bj ({ restitution, ri, rj, ni, spookA, spookB } as solverEquation) =
    let
        g =
            bj.position
                |> Vec3.add rj
                |> Vec3.add (Vec3.negate bi.position)
                |> Vec3.add (Vec3.negate ri)
                |> Vec3.dot ni

        gW =
            (restitution + 1)
                * (Vec3.dot bj.velocity ni - Vec3.dot bi.velocity ni)
                + Vec3.dot bj.angularVelocity (Vec3.cross rj ni)
                - Vec3.dot bi.angularVelocity (Vec3.cross ri ni)

        giMf =
            computeGiMf bi bj solverEquation
    in
        -g * spookA - gW * spookB - dt * giMf


computeGiMf : SolverBody -> SolverBody -> SolverEquation -> Float
computeGiMf bi bj { jacobianElementA, jacobianElementB } =
    let
        iMfi =
            Vec3.scale bi.invMassSolve bi.force

        iMfj =
            Vec3.scale bj.invMassSolve bj.force

        invIi_vmult_taui =
            Mat3.mul bi.invInertiaWorldSolve bi.torque

        invIj_vmult_tauj =
            Mat3.mul bj.invInertiaWorldSolve bj.torque
    in
        JacobianElement.mulVec iMfi invIi_vmult_taui jacobianElementA
            + JacobianElement.mulVec iMfj invIj_vmult_tauj jacobianElementB


computeC : SolverBody -> SolverBody -> SolverEquation -> Float
computeC bi bj solverEquation =
    computeGiMGt bi bj solverEquation
        + solverEquation.spookEps


computeGiMGt : SolverBody -> SolverBody -> SolverEquation -> Float
computeGiMGt bi bj { jacobianElementA, jacobianElementB } =
    bi.invMassSolve
        + bj.invMassSolve
        + (Vec3.dot (Mat3.mul bi.invInertiaWorldSolve jacobianElementA.rotational) jacobianElementA.rotational)
        + (Vec3.dot (Mat3.mul bj.invInertiaWorldSolve jacobianElementB.rotational) jacobianElementB.rotational)


computeGWlambda : SolverBody -> SolverBody -> SolverEquation -> Float
computeGWlambda bi bj { jacobianElementA, jacobianElementB } =
    JacobianElement.mulVec bi.vlambda bi.wlambda jacobianElementA
        + JacobianElement.mulVec bj.vlambda bj.wlambda jacobianElementB
