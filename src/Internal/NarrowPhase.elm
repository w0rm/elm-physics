module Internal.NarrowPhase exposing (getContacts)

import Collision.ConvexConvex
import Collision.ParticleConvex
import Collision.PlaneConvex
import Collision.PlaneParticle
import Collision.PlaneSphere
import Collision.SphereConvex
import Collision.SphereParticle
import Collision.SphereSphere
import Frame3d
import Internal.Body exposing (Body)
import Internal.Contact as Contact exposing (Contact)
import Internal.Coordinates exposing (ShapeWorldFrame3d)
import Internal.Shape exposing (Kind(..), Shape)


getContacts : Body data -> Body data -> List Contact
getContacts body1 body2 =
    List.foldl
        (\shape1 currentContactEquations1 ->
            List.foldl
                (\shape2 ->
                    addShapeContacts
                        (Frame3d.placeIn body1.frame3d shape1.frame3d)
                        shape1
                        (Frame3d.placeIn body2.frame3d shape2.frame3d)
                        shape2
                )
                currentContactEquations1
                body2.shapes
        )
        []
        body1.shapes


addShapeContacts : ShapeWorldFrame3d -> Shape -> ShapeWorldFrame3d -> Shape -> List Contact -> List Contact
addShapeContacts frame3d1 shape1 frame3d2 shape2 contacts =
    case shape1.kind of
        Convex convex1 ->
            case shape2.kind of
                Convex convex2 ->
                    Collision.ConvexConvex.addContacts
                        frame3d1
                        convex1
                        frame3d2
                        convex2
                        contacts

                Plane ->
                    Collision.PlaneConvex.addContacts
                        Contact.flip
                        frame3d2
                        frame3d1
                        convex1
                        contacts

                Sphere radius2 ->
                    Collision.SphereConvex.addContacts
                        Contact.flip
                        frame3d2
                        radius2
                        frame3d1
                        convex1
                        contacts

                Particle ->
                    Collision.ParticleConvex.addContacts
                        Contact.flip
                        frame3d2
                        frame3d1
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
                        frame3d1
                        frame3d2
                        convex2
                        contacts

                Sphere radius2 ->
                    Collision.PlaneSphere.addContacts
                        identity
                        frame3d1
                        frame3d2
                        radius2
                        contacts

                Particle ->
                    Collision.PlaneParticle.addContacts
                        identity
                        frame3d1
                        frame3d2
                        contacts

        Sphere radius1 ->
            case shape2.kind of
                Plane ->
                    Collision.PlaneSphere.addContacts
                        Contact.flip
                        frame3d2
                        frame3d1
                        radius1
                        contacts

                Convex convex2 ->
                    Collision.SphereConvex.addContacts
                        identity
                        frame3d1
                        radius1
                        frame3d2
                        convex2
                        contacts

                Sphere radius2 ->
                    Collision.SphereSphere.addContacts
                        frame3d1
                        radius1
                        frame3d2
                        radius2
                        contacts

                Particle ->
                    Collision.SphereParticle.addContacts
                        identity
                        frame3d1
                        radius1
                        frame3d2
                        contacts

        Particle ->
            case shape2.kind of
                Plane ->
                    Collision.PlaneParticle.addContacts
                        Contact.flip
                        frame3d2
                        frame3d1
                        contacts

                Convex convex2 ->
                    Collision.ParticleConvex.addContacts
                        identity
                        frame3d1
                        frame3d2
                        convex2
                        contacts

                Sphere radius2 ->
                    Collision.SphereParticle.addContacts
                        Contact.flip
                        frame3d2
                        radius2
                        frame3d1
                        contacts

                Particle ->
                    -- don't collide two particles
                    contacts
