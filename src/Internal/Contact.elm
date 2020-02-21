module Internal.Contact exposing (Contact, ContactGroup, Protected(..), flip)

import Direction3d exposing (Direction3d)
import Internal.Body exposing (Body)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Length exposing (Meters)
import Physics.Coordinates exposing (WorldCoordinates)
import Point3d exposing (Point3d)


type Protected data
    = Protected
        { body1 : Body data
        , body2 : Body data
        , points :
            List
                { point : Point3d Meters WorldCoordinates
                , normal : Direction3d WorldCoordinates
                }
        }


type alias ContactGroup data =
    { body1 : Body data
    , body2 : Body data
    , contacts : List Contact
    }


type alias Contact =
    { ni : Vec3 -- contact normal, pointing out of body1
    , pi : Vec3 -- contact point on body1
    , pj : Vec3 -- contact point on body2
    }


{-| Flips the order of two bodies in the contact,
this is useful to e.g. use the same collision function
for adding sphere-convex and convex-sphere contacts
into the same contact group
-}
flip : Contact -> Contact
flip contact =
    { ni = Vec3.negate contact.ni
    , pi = contact.pj
    , pj = contact.pi
    }
