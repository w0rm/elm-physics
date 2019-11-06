module Internal.NarrowPhase exposing (getContacts)

import Collision.ConvexConvex
import Collision.ParticleConvex
import Collision.PlaneConvex
import Collision.PlaneParticle
import Collision.PlaneSphere
import Collision.SphereConvex
import Collision.SphereParticle
import Collision.SphereSphere
import Internal.Body exposing (Body)
import Internal.Contact as Contact exposing (Contact)
import Internal.Coordinates exposing (CenterOfMassCoordinates, ShapeWorldTransform3d)
import Internal.Shape exposing (Kind(..), Shape)
import Internal.Transform3d as Transform3d


getContacts : Body data -> Body data -> List Contact
getContacts body1 body2 =
    List.foldl
        (\shape1 currentContactEquations1 ->
            List.foldl
                (\shape2 ->
                    addShapeContacts
                        (Transform3d.placeIn body1.transform3d shape1.transform3d)
                        shape1
                        (Transform3d.placeIn body2.transform3d shape2.transform3d)
                        shape2
                )
                currentContactEquations1
                body2.shapes
        )
        []
        body1.shapes


addShapeContacts : ShapeWorldTransform3d -> Shape CenterOfMassCoordinates -> ShapeWorldTransform3d -> Shape CenterOfMassCoordinates -> List Contact -> List Contact
addShapeContacts transform3d1 shape1 transform3d2 shape2 contacts =
    case shape1.kind of
        Convex convex1 ->
            case shape2.kind of
                Convex convex2 ->
                    Collision.ConvexConvex.addContacts
                        transform3d1
                        convex1
                        transform3d2
                        convex2
                        contacts

                Plane ->
                    Collision.PlaneConvex.addContacts
                        Contact.flip
                        transform3d2
                        transform3d1
                        convex1
                        contacts

                Sphere radius2 ->
                    Collision.SphereConvex.addContacts
                        Contact.flip
                        transform3d2
                        radius2
                        transform3d1
                        convex1
                        contacts

                Particle ->
                    Collision.ParticleConvex.addContacts
                        Contact.flip
                        transform3d2
                        transform3d1
                        convex1
                        contacts

        Plane ->
            case shape2.kind of
                Plane ->
                    -- don't collide two planes
                    contacts

                Convex convex2 ->
                    Collision.PlaneConvex.addContacts
                        identity
                        transform3d1
                        transform3d2
                        convex2
                        contacts

                Sphere radius2 ->
                    Collision.PlaneSphere.addContacts
                        identity
                        transform3d1
                        transform3d2
                        radius2
                        contacts

                Particle ->
                    Collision.PlaneParticle.addContacts
                        identity
                        transform3d1
                        transform3d2
                        contacts

        Sphere radius1 ->
            case shape2.kind of
                Plane ->
                    Collision.PlaneSphere.addContacts
                        Contact.flip
                        transform3d2
                        transform3d1
                        radius1
                        contacts

                Convex convex2 ->
                    Collision.SphereConvex.addContacts
                        identity
                        transform3d1
                        radius1
                        transform3d2
                        convex2
                        contacts

                Sphere radius2 ->
                    Collision.SphereSphere.addContacts
                        transform3d1
                        radius1
                        transform3d2
                        radius2
                        contacts

                Particle ->
                    Collision.SphereParticle.addContacts
                        identity
                        transform3d1
                        radius1
                        transform3d2
                        contacts

        Particle ->
            case shape2.kind of
                Plane ->
                    Collision.PlaneParticle.addContacts
                        Contact.flip
                        transform3d2
                        transform3d1
                        contacts

                Convex convex2 ->
                    Collision.ParticleConvex.addContacts
                        identity
                        transform3d1
                        transform3d2
                        convex2
                        contacts

                Sphere radius2 ->
                    Collision.SphereParticle.addContacts
                        Contact.flip
                        transform3d2
                        radius2
                        transform3d1
                        contacts

                Particle ->
                    -- don't collide two particles
                    contacts
