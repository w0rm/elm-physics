module Internal.NarrowPhase exposing
    ( ContactGroup
    , getContacts
    )

import Collision.ConvexConvex
import Collision.PlaneConvex
import Collision.PlaneSphere
import Collision.SphereConvex
import Collision.SphereSphere
import Internal.Body as Body exposing (Body)
import Internal.Contact as Contact exposing (Contact)
import Internal.Shape exposing (Kind(..), Shape)
import Internal.Transform exposing (Transform)
import Internal.World as World exposing (World)


type alias ContactGroup data =
    { body1 : Body data
    , body2 : Body data
    , contacts : List Contact
    }


getContacts : World data -> List (ContactGroup data)
getContacts world =
    List.foldl
        (\( body1, body2 ) ->
            (::)
                { body1 = body1
                , body2 = body2
                , contacts = getBodyContacts body1 body2
                }
        )
        []
        (World.getPairs world)


getBodyContacts : Body data -> Body data -> List Contact
getBodyContacts body1 body2 =
    List.foldl
        (\shape1 currentContactEquations1 ->
            List.foldl
                (\shape2 ->
                    addShapeContacts
                        (Body.shapeWorldTransform shape1 body1)
                        shape1
                        (Body.shapeWorldTransform shape2 body2)
                        shape2
                )
                currentContactEquations1
                body2.shapes
        )
        []
        body1.shapes


addShapeContacts : Transform -> Shape -> Transform -> Shape -> List Contact -> List Contact
addShapeContacts shapeTransform1 shape1 shapeTransform2 shape2 contacts =
    case shape1.kind of
        Convex convexPolyhedron ->
            case shape2.kind of
                Convex convexPolyhedron2 ->
                    Collision.ConvexConvex.addContacts
                        shapeTransform1
                        convexPolyhedron
                        shapeTransform2
                        convexPolyhedron2
                        contacts

                Plane ->
                    Collision.PlaneConvex.addContacts
                        Contact.flip
                        shapeTransform2
                        shapeTransform1
                        convexPolyhedron
                        contacts

                Sphere radius ->
                    Collision.SphereConvex.addContacts
                        Contact.flip
                        shapeTransform2
                        radius
                        shapeTransform1
                        convexPolyhedron
                        contacts

        Plane ->
            case shape2.kind of
                Plane ->
                    -- don't collide two planes
                    contacts

                Convex convexPolyhedron ->
                    Collision.PlaneConvex.addContacts
                        identity
                        shapeTransform1
                        shapeTransform2
                        convexPolyhedron
                        contacts

                Sphere radius ->
                    Collision.PlaneSphere.addContacts
                        identity
                        shapeTransform1
                        shapeTransform2
                        radius
                        contacts

        Sphere radius ->
            case shape2.kind of
                Plane ->
                    Collision.PlaneSphere.addContacts
                        Contact.flip
                        shapeTransform2
                        shapeTransform1
                        radius
                        contacts

                Convex convexPolyhedron ->
                    Collision.SphereConvex.addContacts
                        identity
                        shapeTransform1
                        radius
                        shapeTransform2
                        convexPolyhedron
                        contacts

                Sphere radius2 ->
                    Collision.SphereSphere.addContacts
                        shapeTransform1
                        radius
                        shapeTransform2
                        radius2
                        contacts
