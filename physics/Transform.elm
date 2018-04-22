module Physics.Transform
    exposing
        ( Transform
        , vectorToLocalFrame
        , pointToLocalFrame
        , pointToWorldFrame
        )

import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Physics.Quaternion as Quaternion


type alias Transform =
    { quaternion : Vec4
    , position : Vec3
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
    worldPoint
        |> Vec3.sub position
        |> Quaternion.rotate (Quaternion.conjugate quaternion)
