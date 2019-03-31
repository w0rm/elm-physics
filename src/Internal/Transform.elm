module Internal.Transform exposing
    ( Transform
    , identity
    , pointToLocalFrame
    , pointToWorldFrame
    , vectorToLocalFrame
    )

import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Internal.Quaternion as Quaternion exposing (Quaternion)


type alias Transform =
    { quaternion : Quaternion
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
        { quaternion | w = -1 * quaternion.w }
        worldVector


pointToLocalFrame : Transform -> Vec3 -> Vec3
pointToLocalFrame { position, quaternion } worldPoint =
    Quaternion.rotate
        { quaternion | w = -1 * quaternion.w }
        (Vec3.sub worldPoint position)
