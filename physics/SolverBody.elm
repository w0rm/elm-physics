module Physics.SolverBody exposing (..)

import Physics.Body as Body exposing (Body)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Physics.Mat3 as Mat3 exposing (Mat3)
import Physics.JacobianElement as JacobianElement exposing (JacobianElement)


type alias SolverBody =
    { position : Vec3
    , velocity : Vec3
    , angularVelocity : Vec3
    , quaternion : Vec4
    , force : Vec3
    , torque : Vec3
    , vlambda : Vec3
    , wlambda : Vec3
    , invMassSolve : Float
    , invInertiaWorldSolve : Mat3
    }


fromBody : Body -> SolverBody
fromBody { mass, position, velocity, angularVelocity, quaternion, force, torque } =
    { position = position
    , velocity = velocity
    , angularVelocity = angularVelocity
    , quaternion = quaternion
    , force = force
    , torque = torque

    -- more attributes for the solver
    , vlambda = Body.zero3
    , wlambda = Body.zero3
    , invMassSolve =
        if mass == 0 then
            0
        else
            1 / mass
    , invInertiaWorldSolve =
        Mat3.zero
    }


addToWlambda : Float -> JacobianElement -> SolverBody -> SolverBody
addToWlambda deltalambda { spatial, rotational } body =
    { body
        | vlambda =
            spatial
                |> Vec3.scale (deltalambda * body.invMassSolve)
                |> Vec3.add body.vlambda
        , wlambda =
            rotational
                |> Mat3.mul body.invInertiaWorldSolve
                |> Vec3.scale deltalambda
                |> Vec3.add body.wlambda
    }
