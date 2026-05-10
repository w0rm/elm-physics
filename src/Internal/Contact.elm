module Internal.Contact exposing (Contact, ContactGroup, SolverContact, flip)

import Internal.Body exposing (Body)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias ContactGroup =
    { body1 : Body
    , body2 : Body
    , contacts : List SolverContact
    }


type alias SolverContact =
    { friction : Float
    , bounciness : Float
    , contact : Contact
    }


type alias Contact =
    { id : String
    , ni : Vec3 -- contact normal, pointing out of body1
    , pi : Vec3 -- contact point on body1
    , pj : Vec3 -- contact point on body2
    }


{-| Flip the body order in a contact — lets one collision function
handle both sphere-convex and convex-sphere into the same group.
-}
flip : Contact -> Contact
flip contact =
    { id = contact.id
    , ni = Vec3.negate contact.ni
    , pi = contact.pj
    , pj = contact.pi
    }
