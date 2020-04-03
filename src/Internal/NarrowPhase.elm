module Internal.NarrowPhase exposing (getContacts)

import Collision.ConvexConvex
import Collision.ParticleConvex
import Collision.PlaneConvex
import Collision.PlaneParticle
import Collision.PlaneSphere
import Collision.SphereConvex
import Collision.SphereParticle
import Collision.SphereSphere
import Internal.Contact as Contact exposing (Contact)
import Internal.Shape exposing (Shape(..))
import Physics.Coordinates exposing (WorldCoordinates)


getContacts : List (Shape WorldCoordinates) -> List (Shape WorldCoordinates) -> List Contact
getContacts shapes1 shapes2 =
    case shapes1 of
        shape1 :: remainingShapes1 ->
            getContactsHelp shape1 remainingShapes1 shapes2 shapes2 []

        [] ->
            []


getContactsHelp : Shape WorldCoordinates -> List (Shape WorldCoordinates) -> List (Shape WorldCoordinates) -> List (Shape WorldCoordinates) -> List Contact -> List Contact
getContactsHelp shape1 currentShapes1 currentShapes2 shapes2 result =
    case currentShapes2 of
        shape2 :: remainingShapes2 ->
            getContactsHelp shape1
                currentShapes1
                remainingShapes2
                shapes2
                (addShapeContacts shape1 shape2 result)

        [] ->
            case currentShapes1 of
                newShape1 :: remainingShapes1 ->
                    getContactsHelp newShape1
                        remainingShapes1
                        shapes2
                        shapes2
                        result

                [] ->
                    result


addShapeContacts : Shape WorldCoordinates -> Shape WorldCoordinates -> List Contact -> List Contact
addShapeContacts shape1 shape2 contacts =
    case shape1 of
        Convex convex1 ->
            case shape2 of
                Convex convex2 ->
                    Collision.ConvexConvex.addContacts
                        convex1
                        convex2
                        contacts

                Plane plane2 ->
                    Collision.PlaneConvex.addContacts
                        Contact.flip
                        plane2
                        convex1
                        contacts

                Sphere sphere2 ->
                    Collision.SphereConvex.addContacts
                        Contact.flip
                        sphere2
                        convex1
                        contacts

                Particle particle2 ->
                    Collision.ParticleConvex.addContacts
                        Contact.flip
                        particle2
                        convex1
                        contacts

        Plane plane1 ->
            case shape2 of
                Plane _ ->
                    -- don't collide two planes
                    contacts

                Convex convex2 ->
                    Collision.PlaneConvex.addContacts
                        identity
                        plane1
                        convex2
                        contacts

                Sphere sphere2 ->
                    Collision.PlaneSphere.addContacts
                        identity
                        plane1
                        sphere2
                        contacts

                Particle particle2 ->
                    Collision.PlaneParticle.addContacts
                        identity
                        plane1
                        particle2
                        contacts

        Sphere sphere1 ->
            case shape2 of
                Plane plane2 ->
                    Collision.PlaneSphere.addContacts
                        Contact.flip
                        plane2
                        sphere1
                        contacts

                Convex convex2 ->
                    Collision.SphereConvex.addContacts
                        identity
                        sphere1
                        convex2
                        contacts

                Sphere sphere2 ->
                    Collision.SphereSphere.addContacts
                        sphere1
                        sphere2
                        contacts

                Particle particle2 ->
                    Collision.SphereParticle.addContacts
                        identity
                        sphere1
                        particle2
                        contacts

        Particle particle1 ->
            case shape2 of
                Plane plane2 ->
                    Collision.PlaneParticle.addContacts
                        Contact.flip
                        plane2
                        particle1
                        contacts

                Convex convex2 ->
                    Collision.ParticleConvex.addContacts
                        identity
                        particle1
                        convex2
                        contacts

                Sphere sphere2 ->
                    Collision.SphereParticle.addContacts
                        Contact.flip
                        sphere2
                        particle1
                        contacts

                Particle _ ->
                    -- don't collide two particles
                    contacts
