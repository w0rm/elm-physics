module Internal.SolverBody exposing
    ( SolverBody
    , addToWlambda
    , fromBody
    )

import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.JacobianElement as JacobianElement exposing (JacobianElement)
import Internal.Matrix3 as Mat3
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias SolverBody data =
    { body : Body data
    , vlambda : Vec3
    , wlambda : Vec3
    }


fromBody : Body data -> SolverBody data
fromBody body =
    { body = body
    , vlambda = Const.zero3
    , wlambda = Const.zero3
    }


addToWlambda : Float -> JacobianElement -> SolverBody data -> SolverBody data
addToWlambda deltalambda { spatial, rotational } { body, vlambda, wlambda } =
    { body = body
    , vlambda =
        {- spatial
           |> Vec3.scale (deltalambda * body.invMass)
           |> Vec3.add vlambda
        -}
        { x = spatial.x * deltalambda * body.invMass + vlambda.x
        , y = spatial.y * deltalambda * body.invMass + vlambda.y
        , z = spatial.z * deltalambda * body.invMass + vlambda.z
        }
    , wlambda =
        {- rotational
           |> Mat3.transform body.invInertiaWorld
           |> Vec3.scale deltalambda
           |> Vec3.add wlambda
        -}
        { x = (body.invInertiaWorld.m11 * rotational.x + body.invInertiaWorld.m12 * rotational.y + body.invInertiaWorld.m13 * rotational.z) * deltalambda + wlambda.x
        , y = (body.invInertiaWorld.m21 * rotational.x + body.invInertiaWorld.m22 * rotational.y + body.invInertiaWorld.m23 * rotational.z) * deltalambda + wlambda.y
        , z = (body.invInertiaWorld.m31 * rotational.x + body.invInertiaWorld.m32 * rotational.y + body.invInertiaWorld.m33 * rotational.z) * deltalambda + wlambda.z
        }
    }
