module Internal.SolverBody exposing
    ( SolverBody
    , addToWlambda
    , fromBody
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.JacobianElement as JacobianElement exposing (JacobianElement)


type alias SolverBody =
    { body : Body
    , vlambda : Vec3
    , wlambda : Vec3
    }


fromBody : Body -> SolverBody
fromBody body =
    { body = body
    , vlambda = Const.zero3
    , wlambda = Const.zero3
    }


addToWlambda : Float -> JacobianElement -> SolverBody -> SolverBody
addToWlambda deltalambda { spatial, rotational } { body, vlambda, wlambda } =
    { body = body
    , vlambda =
        spatial
            |> Vec3.scale (deltalambda * body.invMass)
            |> Vec3.add vlambda
    , wlambda =
        rotational
            |> Mat4.transform body.invInertiaWorld
            |> Vec3.scale deltalambda
            |> Vec3.add wlambda
    }
