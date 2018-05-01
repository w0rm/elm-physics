module Physics.JacobianElement exposing (..)

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


mul : JacobianElement -> JacobianElement -> Float
mul jacobianElement1 jacobianElement2 =
    Vec3.dot jacobianElement2.spatial jacobianElement1.spatial
        + Vec3.dot jacobianElement2.rotational jacobianElement1.rotational


mulVec : Vec3 -> Vec3 -> JacobianElement -> Float
mulVec spatial rotational jacobianElement =
    Vec3.dot jacobianElement.spatial spatial
        + Vec3.dot jacobianElement.rotational rotational
