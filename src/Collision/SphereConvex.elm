module Collision.SphereConvex exposing (addContacts)

import Internal.Contact exposing (Contact)
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Convex as Convex exposing (Convex)
import Shapes.Sphere exposing (Sphere)


addContacts : (Contact -> Contact) -> Sphere -> Convex -> List Contact -> List Contact
addContacts orderContact { radius, position } hull2 contacts =
    let
        ( maybeContact, penetration ) =
            sphereContact position radius hull2
    in
    case maybeContact of
        Just contact2 ->
            let
                normal =
                    Vec3.direction contact2 position
            in
            orderContact
                { ni = normal
                , pi =
                    { x = contact2.x + penetration * normal.x
                    , y = contact2.y + penetration * normal.y
                    , z = contact2.z + penetration * normal.z
                    }
                , pj = contact2
                }
                :: contacts

        Nothing ->
            contacts


{-| Encapsulated result of sphereTestFace
-}
type TestFaceResult
    = QualifiedEdges (List (List ( Vec3, Vec3 )))
    | FaceContact Vec3 Float


isAFaceContact : TestFaceResult -> Bool
isAFaceContact testFaceResult =
    case testFaceResult of
        FaceContact _ _ ->
            True

        _ ->
            False


type TestBoundaryResult
    = PossibleVertexContact ( Maybe Vec3, Float )
    | EdgeContact ( Vec3, Float )


isAnEdgeContact : TestBoundaryResult -> Bool
isAnEdgeContact testEdgeResult =
    case testEdgeResult of
        EdgeContact _ ->
            True

        _ ->
            False


{-| The contact point, if any, of a Convex with a sphere, and
the sphere's penetration into the Convex beyond that contact.
-}
sphereContact : Vec3 -> Float -> Convex -> ( Maybe Vec3, Float )
sphereContact center radius { faces } =
    let
        sphereFaceContact : Vec3 -> Float -> ( Maybe Vec3, Float )
        sphereFaceContact normal distance =
            -- The contact is located distance away from
            -- the sphere center in the OPPOSITE direction of
            -- the normal.
            ( Just (Vec3.sub center (Vec3.scale distance normal))
            , radius - distance
            )

        sphereBoundaryContact : Vec3 -> Float -> ( Maybe Vec3, Float )
        sphereBoundaryContact localContact distanceSq =
            ( Just (Vec3.add localContact center)
            , radius - sqrt distanceSq
            )

        spherePossibleBoundaryContact : List (List ( Vec3, Vec3 )) -> ( Maybe Vec3, Float )
        spherePossibleBoundaryContact faceEdgeList =
            case sphereTestBoundaries radius faceEdgeList of
                PossibleVertexContact ( Just localContact, distanceSq ) ->
                    sphereBoundaryContact localContact distanceSq

                PossibleVertexContact noContact ->
                    noContact

                EdgeContact ( localContact, distanceSq ) ->
                    sphereBoundaryContact localContact distanceSq

        reframedVertices faceVertices =
            List.foldl
                (\vertex acc ->
                    Vec3.sub vertex center :: acc
                )
                []
                faceVertices

        -- Find the details of the closest faces.
        testFaceResult =
            listRecurseUntil
                isAFaceContact
                (\face statusQuo ->
                    case statusQuo of
                        QualifiedEdges acc ->
                            sphereTestFace
                                radius
                                face.normal
                                (reframedVertices face.vertices)
                                acc

                        FaceContact _ _ ->
                            -- Since a FaceContact short circuits the
                            -- recursion, this case is not expected.
                            statusQuo
                )
                (QualifiedEdges [])
                faces
    in
    case testFaceResult of
        QualifiedEdges faceEdgeList ->
            -- Check the candidate faces' edges and vertices.
            spherePossibleBoundaryContact faceEdgeList

        FaceContact faceNormal faceDistance ->
            sphereFaceContact faceNormal faceDistance


{-| The contact point and distance, if any, of a Convex's face
with a sphere, or otherwise a list of the face's edges that may contain an
edge or vertex contact.
-}
sphereTestFace : Float -> Vec3 -> List Vec3 -> List (List ( Vec3, Vec3 )) -> TestFaceResult
sphereTestFace radius normal vertices acc =
    let
        -- Use an arbitrary vertex from the face to measure the distance to
        -- the origin (sphere center) along the face normal.
        faceDistance =
            case vertices of
                point :: _ ->
                    -(Vec3.dot normal point)

                [] ->
                    -- a negative value prevents a face or edge contact match
                    -1
    in
    if faceDistance - radius < 0 && faceDistance > 0.0 then
        -- Sphere intersects the face plane.
        -- Assume 3 or more valid vertices to proceed
        -- Check if the sphere center projects onto the face plane INSIDE the face polygon.
        case originProjection vertices normal of
            [] ->
                -- The projection falls within all the face's edges.
                FaceContact normal faceDistance

            separatingEdges ->
                -- These origin-excluding edges are candidates for
                -- having an edge or vertex contact.
                QualifiedEdges (separatingEdges :: acc)

    else
        QualifiedEdges acc


{-| The edge or vertex contact point and its distance (squared), if any,
of a Convex's edges with a sphere, limited to a pre-qualified
list of edges per face.
-}
sphereTestBoundaries : Float -> List (List ( Vec3, Vec3 )) -> TestBoundaryResult
sphereTestBoundaries radius faceEdgeList =
    List.foldl
        sphereTestBoundary
        (PossibleVertexContact ( Nothing, radius * radius ))
        faceEdgeList


{-| The edge or possible vertex contact point and its distance (squared),
if any, of a Convex face's pre-qualified edges with a sphere.
-}
sphereTestBoundary : List ( Vec3, Vec3 ) -> TestBoundaryResult -> TestBoundaryResult
sphereTestBoundary faceEdges statusQuo =
    listRecurseUntil
        isAnEdgeContact
        (\( prevVertex, vertex ) statusQuo1 ->
            case statusQuo1 of
                PossibleVertexContact soFar ->
                    sphereTestEdge prevVertex vertex soFar

                EdgeContact _ ->
                    -- Since an EdgeContact stops the recursion,
                    -- this case is not expected.
                    statusQuo1
        )
        statusQuo
        faceEdges


{-| The edge or possible vertex contact point and its distance (squared),
if any, of a Convex face's pre-qualified edge with a sphere.
-}
sphereTestEdge : Vec3 -> Vec3 -> ( Maybe Vec3, Float ) -> TestBoundaryResult
sphereTestEdge prevVertex vertex (( _, minDistanceSq ) as statusQuo) =
    let
        betterVertexContact : Vec3 -> ( Maybe Vec3, Float )
        betterVertexContact candidate =
            let
                -- Note: the vector length of a sphere-framed vertex
                -- is its distance from the sphere center
                vertexLengthSq =
                    Vec3.lengthSquared candidate
            in
            if vertexLengthSq - minDistanceSq < 0 then
                ( Just candidate, vertexLengthSq )

            else
                statusQuo

        edge =
            Vec3.sub vertex prevVertex

        edgeUnit =
            Vec3.normalize edge

        -- The potential contact is where the sphere center
        -- projects onto the edge.
        -- offset is the directed distance between the edge's
        -- starting vertex and that projection. If it is not
        -- between 0 and the edge's length, there is no edge contact.
        -- Yet there may be a contact with whichever vertex is closest
        -- to the projection.
        offset =
            -(Vec3.dot prevVertex edgeUnit)
    in
    if offset < 0 then
        -- prevVertex is closest in this edge,
        -- but there may be a closer edge or
        -- no contact.
        PossibleVertexContact (betterVertexContact prevVertex)

    else if offset * offset - Vec3.lengthSquared edge > 0 then
        -- vertex is closest in this edge,
        -- but there may be a closer edge or
        -- no contact.
        PossibleVertexContact (betterVertexContact vertex)

    else
        let
            edgeContact =
                Vec3.add prevVertex (Vec3.scale offset edgeUnit)

            edgeDistanceSq =
                Vec3.lengthSquared edgeContact
        in
        if edgeDistanceSq - minDistanceSq < 0 then
            EdgeContact ( edgeContact, edgeDistanceSq )

        else
            PossibleVertexContact statusQuo


{-| A 2D point-in-polygon check for the projection of the origin
(e.g. the center of a sphere within its own frame of reference) within a
polygon (e.g. a Convex face). To simplify post-processing,
return a relatively short but complete list of qualified edges (adjacent
vertex pairs) whose lines separate the projection from the polygon.
If the list is empty, the projection is within the polygon.
-}
originProjection : List Vec3 -> Vec3 -> List ( Vec3, Vec3 )
originProjection vertices normal =
    Convex.foldFaceEdges
        (\prevVertex vertex acc ->
            let
                edge_x_normal =
                    Vec3.cross normal (Vec3.sub vertex prevVertex)
            in
            -- The sign of this dot product determines on which
            -- side of the directed edge the projected point lies,
            -- left or right, within the face plane.
            -- For the projection to be within the face, the sign
            -- must always be non-negative when circling from vertex
            -- to vertex in the listed (counter-clockwise) direction.
            -- Retain any edge that tests negative as a candidate
            -- for an edge or vertex contact.
            if Vec3.dot edge_x_normal prevVertex < 0 then
                ( prevVertex, vertex ) :: acc

            else
                acc
        )
        []
        vertices


{-| Recursively "foldl" the function over the elements of the list,
until the result passes a test. Using recursion in the place of a true
fold allows a short-circuit return as soon as the test passes.
Note: If the short-circuit condition is unlikely, especially towards
the beginning of the list, it MAY be more efficient to use foldl,
integrating the short-circuit test into the folding function as an up-front
pass-through condition.
-}
listRecurseUntil : (b -> Bool) -> (a -> b -> b) -> b -> List a -> b
listRecurseUntil test fn resultSoFar list =
    if test resultSoFar then
        resultSoFar

    else
        case list of
            head :: tail ->
                let
                    acc =
                        fn head resultSoFar
                in
                listRecurseUntil test fn acc tail

            _ ->
                resultSoFar
