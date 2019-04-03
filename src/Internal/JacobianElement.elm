module Internal.JacobianElement exposing
    ( JacobianElement
    , mulVec
    )

import Internal.Vector3 as Vec3 exposing (Vec3)


type alias JacobianElement =
    { spatial : Vec3
    , rotational : Vec3
    }


mulVec : Vec3 -> Vec3 -> JacobianElement -> Float
mulVec spatial rotational jacobianElement =
    Vec3.dot jacobianElement.spatial spatial
        + Vec3.dot jacobianElement.rotational rotational
