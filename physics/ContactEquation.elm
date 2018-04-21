module Physics.ContactEquation exposing (..)

import Physics.Body as Body exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3)
import Time exposing (Time)
import Dict exposing (Dict)


minForce : Float
minForce =
    0


maxForce : Float
maxForce =
    1.0e6


restitution : Float
restitution =
    0


type alias ContactEquation =
    { bodyId1 : BodyId
    , bodyId2 : BodyId
    , ri : Vec3 -- vector from the center of body1 to the contact point
    , rj : Vec3 -- vector from body2 position to the contact point
    , ni : Vec3 -- contact normal, pointing out of body1
    }
