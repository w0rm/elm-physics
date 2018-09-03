module AltPhysics.JacobianElement exposing
    ( JacobianElement
    , jacobianElement
    , mulVec
    )

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltPhysics.Const as Const


type alias JacobianElement =
    { spatial : Vec3
    , rotational : Vec3
    }


jacobianElement : JacobianElement
jacobianElement =
    { spatial = Const.zero3
    , rotational = Const.zero3
    }


mulVec : Vec3 -> Vec3 -> JacobianElement -> Float
mulVec spatial rotational jacobianElement_ =
    Vec3.dot jacobianElement_.spatial spatial
        + Vec3.dot jacobianElement_.rotational rotational
