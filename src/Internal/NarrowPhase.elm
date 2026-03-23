module Internal.NarrowPhase exposing (getContacts)

import Collision.ConvexConvex
import Collision.ParticleConvex
import Collision.PlaneConvex
import Collision.PlaneParticle
import Collision.PlaneSphere
import Collision.SphereConvex
import Collision.SphereParticle
import Collision.SphereSphere
import Internal.Contact as Contact exposing (Contact, SolverContact)
import Internal.Material as Material exposing (Material)
import Internal.Shape exposing (Shape(..))
import Physics.Coordinates exposing (WorldCoordinates)


getContacts : List ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> List SolverContact
getContacts pairs1 pairs2 =
    case pairs1 of
        pair1 :: remainingPairs1 ->
            getContactsHelp pair1 remainingPairs1 pairs2 pairs2 []

        [] ->
            []


getContactsHelp : ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> List SolverContact -> List SolverContact
getContactsHelp pair1 currentPairs1 currentPairs2 pairs2 result =
    case currentPairs2 of
        pair2 :: remainingPairs2 ->
            getContactsHelp pair1
                currentPairs1
                remainingPairs2
                pairs2
                (addShapeContacts pair1 pair2 result)

        [] ->
            case currentPairs1 of
                newPair1 :: remainingPairs1 ->
                    getContactsHelp newPair1
                        remainingPairs1
                        pairs2
                        pairs2
                        result

                [] ->
                    result


addShapeContacts : ( Shape WorldCoordinates, Material ) -> ( Shape WorldCoordinates, Material ) -> List SolverContact -> List SolverContact
addShapeContacts ( shape1, mat1 ) ( shape2, mat2 ) contacts =
    let
        bounciness =
            Material.combine mat1.bounciness mat2.bounciness

        friction =
            Material.combine mat1.friction mat2.friction

        rawContacts =
            addRawShapeContacts shape1 shape2 []
    in
    List.foldl
        (\contact acc ->
            { bounciness = bounciness
            , friction = friction
            , contact = contact
            }
                :: acc
        )
        contacts
        rawContacts


addRawShapeContacts : Shape WorldCoordinates -> Shape WorldCoordinates -> List Contact -> List Contact
addRawShapeContacts shape1 shape2 contacts =
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
