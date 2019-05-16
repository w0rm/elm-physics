module Internal.NarrowPhase exposing
    ( Contact
    , ContactGroup
    , Order(..)
    , addSphereConvexContacts
    , getContacts
    )

import Internal.Body as Body exposing (Body)
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Internal.Quaternion as Quaternion
import Internal.Shape as Shape exposing (Kind(..), Shape)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.World as World exposing (World)


type alias Contact =
    { ni : Vec3 -- contact normal, pointing out of body1
    , pi : Vec3 -- contact point on body1
    , pj : Vec3 -- contact point on body2
    }


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


type Order
    = ASC
    | DESC


orderContact : Order -> Contact -> Contact
orderContact order contact =
    case order of
        ASC ->
            contact

        DESC ->
            { ni = Vec3.negate contact.ni
            , pi = contact.pj
            , pj = contact.pi
            }


addShapeContacts : Transform -> Shape -> Transform -> Shape -> List Contact -> List Contact
addShapeContacts shapeTransform1 shape1 shapeTransform2 shape2 contacts =
    case ( shape1.kind, shape2.kind ) of
        ( Plane, Plane ) ->
            -- don't collide two planes
            contacts

        ( Plane, Convex convexPolyhedron ) ->
            addPlaneConvexContacts
                ASC
                shapeTransform1
                shapeTransform2
                convexPolyhedron
                contacts

        ( Plane, Sphere radius ) ->
            addPlaneSphereContacts
                ASC
                shapeTransform1
                shapeTransform2
                radius
                contacts

        ( Convex convexPolyhedron, Plane ) ->
            addPlaneConvexContacts
                DESC
                shapeTransform2
                shapeTransform1
                convexPolyhedron
                contacts

        ( Convex convexPolyhedron1, Convex convexPolyhedron2 ) ->
            addConvexConvexContacts
                shapeTransform1
                convexPolyhedron1
                shapeTransform2
                convexPolyhedron2
                contacts

        ( Convex convexPolyhedron, Sphere radius ) ->
            addSphereConvexContacts
                DESC
                shapeTransform2
                radius
                shapeTransform1
                convexPolyhedron
                contacts

        ( Sphere radius, Plane ) ->
            addPlaneSphereContacts
                DESC
                shapeTransform2
                shapeTransform1
                radius
                contacts

        ( Sphere radius, Convex convexPolyhedron ) ->
            addSphereConvexContacts
                ASC
                shapeTransform1
                radius
                shapeTransform2
                convexPolyhedron
                contacts

        ( Sphere radius1, Sphere radius2 ) ->
            addSphereSphereContacts
                shapeTransform1
                radius1
                shapeTransform2
                radius2
                contacts


addPlaneConvexContacts : Order -> Transform -> Transform -> ConvexPolyhedron -> List Contact -> List Contact
addPlaneConvexContacts order planeTransform convexTransform convexPolyhedron contacts =
    let
        worldNormal =
            Quaternion.rotate planeTransform.orientation Vec3.k
    in
    List.foldl
        (\vertex currentContacts ->
            let
                worldVertex =
                    Transform.pointToWorldFrame convexTransform vertex

                dot =
                    planeTransform.position
                        |> Vec3.sub worldVertex
                        |> Vec3.dot worldNormal
            in
            if dot <= 0 then
                orderContact order
                    { ni = worldNormal
                    , pi =
                        worldNormal
                            |> Vec3.scale dot
                            |> Vec3.sub worldVertex
                    , pj = worldVertex
                    }
                    :: currentContacts

            else
                currentContacts
        )
        contacts
        convexPolyhedron.vertices


addConvexConvexContacts : Transform -> ConvexPolyhedron -> Transform -> ConvexPolyhedron -> List Contact -> List Contact
addConvexConvexContacts shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 contacts =
    case ConvexPolyhedron.findSeparatingAxis shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 of
        Just sepAxis ->
            ConvexPolyhedron.clipAgainstHull shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 sepAxis -100 100
                |> List.foldl
                    (\{ point, normal, depth } currentContacts ->
                        { ni = Vec3.negate sepAxis
                        , pi = Vec3.sub point (Vec3.scale depth normal)
                        , pj = point
                        }
                            :: currentContacts
                    )
                    contacts

        Nothing ->
            contacts


addPlaneSphereContacts : Order -> Transform -> Transform -> Float -> List Contact -> List Contact
addPlaneSphereContacts order planeTransform t2 radius contacts =
    let
        worldPlaneNormal =
            Quaternion.rotate planeTransform.orientation Vec3.k

        worldVertex =
            worldPlaneNormal
                |> Vec3.scale radius
                |> Vec3.sub t2.position

        dot =
            planeTransform.position
                |> Vec3.sub worldVertex
                |> Vec3.dot worldPlaneNormal
    in
    if dot <= 0 then
        orderContact order
            { ni = worldPlaneNormal
            , pi = Vec3.sub worldVertex (Vec3.scale dot worldPlaneNormal)
            , pj = worldVertex
            }
            :: contacts

    else
        contacts


addSphereConvexContacts : Order -> Transform -> Float -> Transform -> ConvexPolyhedron -> List Contact -> List Contact
addSphereConvexContacts order { position } radius t2 hull2 contacts =
    let
        ( maybeWorldContact, penetration ) =
            ConvexPolyhedron.sphereContact position radius t2 hull2
    in
    case maybeWorldContact of
        Just worldContact2 ->
            let
                worldNormal =
                    Vec3.direction worldContact2 position
            in
            orderContact order
                { ni = worldNormal
                , pi = Vec3.add worldContact2 (Vec3.scale penetration worldNormal)
                , pj = worldContact2
                }
                :: contacts

        Nothing ->
            contacts


addSphereSphereContacts : Transform -> Float -> Transform -> Float -> List Contact -> List Contact
addSphereSphereContacts t1 radius1 t2 radius2 contacts =
    let
        center1 =
            Transform.pointToWorldFrame t1 Vec3.zero

        center2 =
            Transform.pointToWorldFrame t2 Vec3.zero

        distance =
            Vec3.distance center2 center1
                - radius1
                - radius2

        normal =
            Vec3.direction center2 center1
    in
    if distance > 0 then
        contacts

    else
        { ni = normal
        , pi = Vec3.add center1 (Vec3.scale (radius1 - distance) normal)
        , pj = Vec3.add center2 (Vec3.scale -radius2 normal)
        }
            :: contacts
