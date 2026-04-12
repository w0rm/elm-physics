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
import Internal.Coordinates exposing (WorldCoordinates)
import Internal.Material as Material exposing (Material)
import Internal.Shape exposing (Shape(..))


getContacts : String -> List ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> List SolverContact
getContacts idPrefix pairs1 pairs2 =
    case pairs1 of
        pair1 :: remainingPairs1 ->
            getContactsHelp idPrefix 1 pair1 remainingPairs1 1 pairs2 pairs2 []

        [] ->
            []


getContactsHelp : String -> Int -> ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> Int -> List ( Shape WorldCoordinates, Material ) -> List ( Shape WorldCoordinates, Material ) -> List SolverContact -> List SolverContact
getContactsHelp idPrefix shapeId1 pair1 currentPairs1 shapeId2 currentPairs2 pairs2 result =
    case currentPairs2 of
        pair2 :: remainingPairs2 ->
            getContactsHelp idPrefix
                shapeId1
                pair1
                currentPairs1
                (shapeId2 + 1)
                remainingPairs2
                pairs2
                (addShapeContacts (idPrefix ++ "-" ++ String.fromInt shapeId1 ++ "-" ++ String.fromInt shapeId2) pair1 pair2 result)

        [] ->
            case currentPairs1 of
                newPair1 :: remainingPairs1 ->
                    getContactsHelp idPrefix
                        (shapeId1 + 1)
                        newPair1
                        remainingPairs1
                        1
                        pairs2
                        pairs2
                        result

                [] ->
                    result


addShapeContacts : String -> ( Shape WorldCoordinates, Material ) -> ( Shape WorldCoordinates, Material ) -> List SolverContact -> List SolverContact
addShapeContacts idPrefix ( shape1, mat1 ) ( shape2, mat2 ) contacts =
    let
        bounciness =
            Material.combine mat1.bounciness mat2.bounciness

        friction =
            Material.combine mat1.friction mat2.friction

        rawContacts =
            addRawShapeContacts idPrefix shape1 shape2 []
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


addRawShapeContacts : String -> Shape WorldCoordinates -> Shape WorldCoordinates -> List Contact -> List Contact
addRawShapeContacts idPrefix shape1 shape2 contacts =
    case shape1 of
        Convex convex1 ->
            case shape2 of
                Convex convex2 ->
                    Collision.ConvexConvex.addContacts
                        idPrefix
                        convex1
                        convex2
                        contacts

                Plane plane2 ->
                    Collision.PlaneConvex.addContacts
                        idPrefix
                        Contact.flip
                        plane2
                        convex1
                        contacts

                Sphere sphere2 ->
                    Collision.SphereConvex.addContacts
                        idPrefix
                        Contact.flip
                        sphere2
                        convex1
                        contacts

                Particle particle2 ->
                    Collision.ParticleConvex.addContacts
                        idPrefix
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
                        idPrefix
                        identity
                        plane1
                        convex2
                        contacts

                Sphere sphere2 ->
                    Collision.PlaneSphere.addContacts
                        idPrefix
                        identity
                        plane1
                        sphere2
                        contacts

                Particle particle2 ->
                    Collision.PlaneParticle.addContacts
                        idPrefix
                        identity
                        plane1
                        particle2
                        contacts

        Sphere sphere1 ->
            case shape2 of
                Plane plane2 ->
                    Collision.PlaneSphere.addContacts
                        idPrefix
                        Contact.flip
                        plane2
                        sphere1
                        contacts

                Convex convex2 ->
                    Collision.SphereConvex.addContacts
                        idPrefix
                        identity
                        sphere1
                        convex2
                        contacts

                Sphere sphere2 ->
                    Collision.SphereSphere.addContacts
                        idPrefix
                        sphere1
                        sphere2
                        contacts

                Particle particle2 ->
                    Collision.SphereParticle.addContacts
                        idPrefix
                        identity
                        sphere1
                        particle2
                        contacts

        Particle particle1 ->
            case shape2 of
                Plane plane2 ->
                    Collision.PlaneParticle.addContacts
                        idPrefix
                        Contact.flip
                        plane2
                        particle1
                        contacts

                Convex convex2 ->
                    Collision.ParticleConvex.addContacts
                        idPrefix
                        identity
                        particle1
                        convex2
                        contacts

                Sphere sphere2 ->
                    Collision.SphereParticle.addContacts
                        idPrefix
                        Contact.flip
                        sphere2
                        particle1
                        contacts

                Particle _ ->
                    -- don't collide two particles
                    contacts
