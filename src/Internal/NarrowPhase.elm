module Internal.NarrowPhase exposing
    ( addSphereConvexContacts
    , getContacts
    )

import Array exposing (Array)
import Internal.Body as Body exposing (Body, BodyId)
import Internal.Const as Const
import Internal.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Internal.Material as Material
import Internal.Quaternion as Quaternion
import Internal.Shape as Shape exposing (Kind(..), Shape)
import Internal.SolverEquation exposing (ContactEquation)
import Internal.Transform as Transform exposing (Transform)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Internal.World as World exposing (World)


getContacts : World data -> List (ContactEquation data)
getContacts world =
    List.foldl
        (\( body1, body2 ) -> getBodyContacts world body1 body2)
        []
        (World.getPairs world)


getBodyContacts : World data -> Body data -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
getBodyContacts world body1 body2 contactEquations =
    List.foldl
        (\shape1 currentContactEquations1 ->
            List.foldl
                (\shape2 currentContactEquations2 ->
                    getShapeContacts
                        (Body.shapeWorldTransform shape1 body1)
                        shape1
                        body1
                        (Body.shapeWorldTransform shape2 body2)
                        shape2
                        body2
                        currentContactEquations2
                )
                currentContactEquations1
                body2.shapes
        )
        contactEquations
        body1.shapes


getShapeContacts : Transform -> Shape -> Body data -> Transform -> Shape -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
getShapeContacts shapeTransform1 shape1 body1 shapeTransform2 shape2 body2 contactEquations =
    case ( shape1.kind, shape2.kind ) of
        ( Plane, Plane ) ->
            -- don't collide two planes
            contactEquations

        ( Plane, Convex convexPolyhedron ) ->
            addPlaneConvexContacts
                shapeTransform1
                body1
                shapeTransform2
                convexPolyhedron
                body2
                contactEquations

        ( Plane, Sphere radius ) ->
            addPlaneSphereContacts
                shapeTransform1
                body1
                shapeTransform2
                radius
                body2
                contactEquations

        ( Convex convexPolyhedron, Plane ) ->
            addPlaneConvexContacts
                shapeTransform2
                body2
                shapeTransform1
                convexPolyhedron
                body1
                contactEquations

        ( Convex convexPolyhedron1, Convex convexPolyhedron2 ) ->
            addConvexConvexContacts
                shapeTransform1
                convexPolyhedron1
                body1
                shapeTransform2
                convexPolyhedron2
                body2
                contactEquations

        ( Convex convexPolyhedron, Sphere radius ) ->
            addSphereConvexContacts
                shapeTransform2
                radius
                body2
                shapeTransform1
                convexPolyhedron
                body1
                contactEquations

        ( Sphere radius, Plane ) ->
            addPlaneSphereContacts
                shapeTransform2
                body2
                shapeTransform1
                radius
                body1
                contactEquations

        ( Sphere radius, Convex convexPolyhedron ) ->
            addSphereConvexContacts
                shapeTransform1
                radius
                body1
                shapeTransform2
                convexPolyhedron
                body2
                contactEquations

        ( Sphere radius1, Sphere radius2 ) ->
            addSphereSphereContacts
                shapeTransform1
                radius1
                body1
                shapeTransform2
                radius2
                body2
                contactEquations


addPlaneConvexContacts : Transform -> Body data -> Transform -> ConvexPolyhedron -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
addPlaneConvexContacts planeTransform planeBody convexTransform convexPolyhedron convexBody contactEquations =
    let
        worldNormal =
            Quaternion.rotate planeTransform.orientation Vec3.k
    in
    List.foldl
        (\vertex currentContactEquations ->
            let
                worldVertex =
                    Transform.pointToWorldFrame convexTransform vertex

                dot =
                    planeTransform.position
                        |> Vec3.sub worldVertex
                        |> Vec3.dot worldNormal
            in
            if dot <= 0 then
                { body1 = planeBody
                , body2 = convexBody
                , ni = worldNormal
                , ri =
                    Vec3.sub
                        (worldNormal
                            |> Vec3.scale dot
                            |> Vec3.sub worldVertex
                        )
                        planeBody.position
                , rj = Vec3.sub worldVertex convexBody.position
                , bounciness = Material.contactBounciness planeBody.material convexBody.material
                }
                    :: currentContactEquations

            else
                currentContactEquations
        )
        contactEquations
        convexPolyhedron.vertices


addConvexConvexContacts : Transform -> ConvexPolyhedron -> Body data -> Transform -> ConvexPolyhedron -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
addConvexConvexContacts shapeTransform1 convexPolyhedron1 body1 shapeTransform2 convexPolyhedron2 body2 contactEquations =
    case ConvexPolyhedron.findSeparatingAxis shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 of
        Just sepAxis ->
            ConvexPolyhedron.clipAgainstHull shapeTransform1 convexPolyhedron1 shapeTransform2 convexPolyhedron2 sepAxis -100 100
                |> List.foldl
                    (\{ point, normal, depth } currentContactEquations ->
                        let
                            q =
                                normal
                                    |> Vec3.negate
                                    |> Vec3.scale depth

                            ri =
                                Vec3.add point q
                                    |> Vec3.add (Vec3.negate body1.position)

                            rj =
                                point
                                    |> Vec3.add (Vec3.negate body2.position)
                        in
                        { body1 = body1
                        , body2 = body2
                        , ni = Vec3.negate sepAxis
                        , ri = ri
                        , rj = rj
                        , bounciness = Material.contactBounciness body1.material body2.material
                        }
                            :: currentContactEquations
                    )
                    contactEquations

        Nothing ->
            contactEquations


addPlaneSphereContacts : Transform -> Body data -> Transform -> Float -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
addPlaneSphereContacts planeTransform body1 t2 radius body2 contactEquations =
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
        { body1 = body1
        , body2 = body2
        , ni = worldPlaneNormal
        , ri =
            Vec3.sub
                (worldPlaneNormal
                    |> Vec3.scale dot
                    |> Vec3.sub worldVertex
                )
                body1.position
        , rj = Vec3.sub worldVertex body2.position
        , bounciness = Material.contactBounciness body1.material body2.material
        }
            :: contactEquations

    else
        contactEquations


addSphereConvexContacts : Transform -> Float -> Body data -> Transform -> ConvexPolyhedron -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
addSphereConvexContacts { position } radius body1 t2 hull2 body2 contactEquations =
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
            { body1 = body1
            , body2 = body2
            , ni = worldNormal
            , ri = Vec3.scale radius worldNormal
            , rj = Vec3.sub worldContact2 t2.position
            , bounciness = Material.contactBounciness body1.material body2.material
            }
                :: contactEquations

        Nothing ->
            contactEquations


addSphereSphereContacts : Transform -> Float -> Body data -> Transform -> Float -> Body data -> List (ContactEquation data) -> List (ContactEquation data)
addSphereSphereContacts t1 radius1 body1 t2 radius2 body2 contactEquations =
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
        contactEquations

    else
        { body1 = body1
        , body2 = body2
        , ni = normal
        , ri = Vec3.scale radius1 normal
        , rj = Vec3.scale -radius2 normal
        , bounciness = Material.contactBounciness body1.material body2.material
        }
            :: contactEquations
