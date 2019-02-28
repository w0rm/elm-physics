module Physics.Transform exposing
    ( Transform
    , identity
    , pointToLocalFrame
    , pointToWorldFrame
    , vectorToLocalFrame
    )

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4)
import Physics.Quaternion as Quaternion


type alias Transform =
    { quaternion : Vec4
    , position : Vec3
    }


identity : Transform
identity =
    { position = vec3 0 0 0
    , quaternion = Quaternion.identity
    }


pointToWorldFrame : Transform -> Vec3 -> Vec3
pointToWorldFrame { position, quaternion } localPoint =
    localPoint
        |> Quaternion.rotate quaternion
        |> Vec3.add position


vectorToLocalFrame : Transform -> Vec3 -> Vec3
vectorToLocalFrame { quaternion } worldVector =
    Quaternion.rotate
        (Vec4.setW (-1 * Vec4.getW quaternion) quaternion)
        worldVector


pointToLocalFrame : Transform -> Vec3 -> Vec3
pointToLocalFrame { position, quaternion } worldPoint =
    position
        |> Vec3.sub worldPoint
        |> Quaternion.rotate (Quaternion.conjugate quaternion)
