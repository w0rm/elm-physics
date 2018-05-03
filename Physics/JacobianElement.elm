module Physics.JacobianElement
    exposing
        ( JacobianElement
        , jacobianElement
        , mulVec
        )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias JacobianElement =
    { spatial : Vec3
    , rotational : Vec3
    }


jacobianElement : JacobianElement
jacobianElement =
    { spatial = vec3 0 0 0
    , rotational = vec3 0 0 0
    }


mulVec : Vec3 -> Vec3 -> JacobianElement -> Float
mulVec spatial rotational jacobianElement =
    Vec3.dot jacobianElement.spatial spatial
        + Vec3.dot jacobianElement.rotational rotational
