module Internal.Transform exposing
    ( Transform
    , identity
    , pointToLocalFrame
    , pointToWorldFrame
    , vectorToLocalFrame
    )

import Internal.Quaternion as Quaternion exposing (Quaternion)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Transform =
    { orientation : Quaternion
    , position : Vec3
    }


identity : Transform
identity =
    { position = vec3 0 0 0
    , orientation = Quaternion.identity
    }


pointToWorldFrame : Transform -> Vec3 -> Vec3
pointToWorldFrame { position, orientation } localPoint =
    Vec3.add position (Quaternion.rotate orientation localPoint)


vectorToLocalFrame : Transform -> Vec3 -> Vec3
vectorToLocalFrame { orientation } worldVector =
    Quaternion.derotate orientation worldVector


pointToLocalFrame : Transform -> Vec3 -> Vec3
pointToLocalFrame { position, orientation } worldPoint =
    Quaternion.derotate orientation (Vec3.sub worldPoint position)
