module Internal.Contact exposing (Contact, PairGroup, SolverContact, flip)

import Internal.Body exposing (Body)
import Internal.Constraint exposing (Constraint)
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias PairGroup =
    { body1 : Body
    , body2 : Body
    , contacts : List SolverContact
    , constraints : List (Constraint CenterOfMassCoordinates)
    }


type alias SolverContact =
    { friction : Float
    , bounciness : Float
    , contact : Contact
    }


type alias Contact =
    { shapeKey : Int
    , featureKey : Int
    , ni : Vec3 -- contact normal, pointing out of body1
    , pi : Vec3 -- contact point on body1
    , pj : Vec3 -- contact point on body2
    }


{-| Flip the body order in a contact — lets one collision function
handle both sphere-convex and convex-sphere into the same group.
-}
flip : Contact -> Contact
flip contact =
    { shapeKey = contact.shapeKey
    , featureKey = contact.featureKey
    , ni = Vec3.negate contact.ni
    , pi = contact.pj
    , pj = contact.pi
    }
