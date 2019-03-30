module Internal.ContactEquation exposing (ContactEquation)

import AltMath.Vector3 as Vec3 exposing (Vec3)
import Internal.Body as Body exposing (Body)


type alias ContactEquation data =
    { body1 : Body data
    , body2 : Body data
    , ri : Vec3 -- vector from the center of body1 to the contact point
    , rj : Vec3 -- vector from body2 position to the contact point
    , ni : Vec3 -- contact normal, pointing out of body1
    , restitution : Float -- "bounciness": u1 = -e*u0
    }
