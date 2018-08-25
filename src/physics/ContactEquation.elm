module Physics.ContactEquation exposing (ContactEquation)

import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Body as Body exposing (BodyId)


type alias ContactEquation =
    { bodyId1 : BodyId
    , bodyId2 : BodyId
    , ri : Vec3 -- vector from the center of body1 to the contact point
    , rj : Vec3 -- vector from body2 position to the contact point
    , ni : Vec3 -- contact normal, pointing out of body1
    , restitution : Float -- "bounciness": u1 = -e*u0
    }
