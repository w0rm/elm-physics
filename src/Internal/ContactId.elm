module Internal.ContactId exposing
    ( bodyKey
    , capsuleCapEnd1
    , capsuleCapEnd2
    , capsuleCylinder
    , capsuleCylinder1
    , capsuleCylinder2
    , convexConvexEdge
    , convexConvexFace
    , featureString
    , onConvexEdge
    , onConvexFace
    , onConvexNone
    , onConvexVertex
    , planeCapEnd
    , planeVertex
    , shapeKey
    , simple
    , sphereOnConvex
    , toString
    )

{-| The warm-start cache key for a contact: two packed integers stored unboxed
on `Contact`/`Equation`.

  - `shapeKey` — the colliding shape pair (`shapeKey s1 s2`). The body pair is
    not part of the id; it is the cache's tree key (`bodyKey`), so the id only
    disambiguates contacts within one body pair.
  - `featureKey` — which geometric feature touched, built by the constructors
    below. `featureString` / `toString` decode it for the sandbox and tests.

User constraints, which are not warm-started, use the `0, 0` sentinel.

Field budgets (under the 2^53 limit): body id < 2^18 → `bodyKey` < 2^36; shape
index < 2^8 → `shapeKey` < 2^16; feature tag < 16 with four sub-ids each < 2^11.

-}

-- KEY 1: SHAPE PAIR IDENTITY


bodyRange : Int
bodyRange =
    262144


shapeRange : Int
shapeRange =
    256


{-| The warm-start cache's tree key for a body pair, canonicalized so the
smaller id is always first. That keeps the key identical across frames even
when the gravity sort swaps which body is body1/body2 — what makes the
warm-start lookup stable. A pair's contacts are grouped under this key; their
shape pair lives in `shapeKey` (`shapeKey`).

(`a - b <= 0` rather than `a <= b`: comparing two bindings directly compiles to
the polymorphic `_Utils_cmp`; the subtraction form is a direct JS comparison.)

-}
bodyKey : Int -> Int -> Int
bodyKey a b =
    if a - b <= 0 then
        a * bodyRange + b

    else
        b * bodyRange + a


{-| Pack a colliding pair's two 1-based shape indices into the contact's first
key (`shapeKey`). The body pair is not included — it is the cache tree key
(`bodyKey`), so `shapeKey` only has to disambiguate contacts within one body pair.
-}
shapeKey : Int -> Int -> Int
shapeKey shapeId1 shapeId2 =
    shapeId1 * shapeRange + shapeId2



-- KEY 2: FEATURE IDENTITY
--
-- A featureKey is a tag plus up to four sub-ids, packed little-endian by
-- `feature`. The tag, capsule-site, and convex-feature codes below are private:
-- collision modules compose the exposed constructors, and `featureString`
-- decodes against these same constants.


featRange : Int
featRange =
    2048


feature : Int -> Int -> Int -> Int -> Int -> Int
feature tag a b c d =
    (((tag * featRange + a) * featRange + b) * featRange + c) * featRange + d


tagSimple : Int
tagSimple =
    0


tagConvexFace : Int
tagConvexFace =
    1


tagConvexEdge : Int
tagConvexEdge =
    2


tagPlaneVertex : Int
tagPlaneVertex =
    3


tagPlaneCap : Int
tagPlaneCap =
    4


tagSphereConvex : Int
tagSphereConvex =
    5


tagCapsuleConvex : Int
tagCapsuleConvex =
    6


{-| Which part of a capsule made a contact (stored in a capsule-convex
featureKey's first sub-id; see `siteString`).
-}
siteCapEnd1 : Int
siteCapEnd1 =
    1


siteCapEnd2 : Int
siteCapEnd2 =
    2


siteCapCyl1 : Int
siteCapCyl1 =
    3


siteCapCyl2 : Int
siteCapCyl2 =
    4


siteCapCyl : Int
siteCapCyl =
    5


{-| A convex feature (which face/edge/vertex of a convex hull a contact sits
on) packed into one sub-id: `0` none, `1` edge, `2` vertex, and `faceId + 2`
for a specific face (face ids are 1-based, so faces never collide with the
edge/vertex/none sentinels).
-}
cfNone : Int
cfNone =
    0


cfEdge : Int
cfEdge =
    1


cfVertex : Int
cfVertex =
    2


cfFaceOffset : Int
cfFaceOffset =
    2



-- ENCODE


{-| Single-contact collisions (sphere/plane/particle/capsule primitives).
The shape pair already disambiguates, so the feature carries no sub-ids.
-}
simple : Int
simple =
    tagSimple


{-| Convex-convex face clip: faces `f1`/`f2` (1-based) and the incident vertex `v`
the clipped point keys to (a kept vertex is itself; a crossing takes its nearer
endpoint). Keyed to the feature, not clip position, so warm-start survives the
manifold cull.
-}
convexConvexFace : Int -> Int -> Int -> Int
convexConvexFace f1 f2 v =
    feature tagConvexFace f1 f2 v 0


{-| Convex-convex edge-edge: direction/edge indices on each side.
-}
convexConvexEdge : Int -> Int -> Int -> Int -> Int
convexConvexEdge dir1 edge1 dir2 edge2 =
    feature tagConvexEdge dir1 edge1 dir2 edge2


{-| Plane-convex: the convex vertex index dipped below the plane.
-}
planeVertex : Int -> Int
planeVertex v =
    feature tagPlaneVertex v 0 0 0


{-| Plane-capsule: which capsule endpoint (1 or 2) made the contact.
-}
planeCapEnd : Int -> Int
planeCapEnd endpoint =
    feature tagPlaneCap endpoint 0 0 0


{-| The four convex features, shared by `sphereOnConvex` and the `capsule*`
constructors below. Compose them, e.g. `sphereOnConvex (onConvexFace 3)` or
`capsuleCylinder onConvexEdge`.
-}
onConvexFace : Int -> Int
onConvexFace faceId =
    faceId + cfFaceOffset


onConvexEdge : Int
onConvexEdge =
    cfEdge


onConvexVertex : Int
onConvexVertex =
    cfVertex


onConvexNone : Int
onConvexNone =
    cfNone


{-| Sphere resting on a convex feature (`onConvex*`).
-}
sphereOnConvex : Int -> Int
sphereOnConvex convexFeature =
    feature tagSphereConvex convexFeature 0 0 0


{-| Capsule-convex contacts: which part of the capsule (cap end 1/2, or the
cylinder body — two clipped points, or a single one) touched which convex
feature (`onConvex*`).
-}
capsuleCapEnd1 : Int -> Int
capsuleCapEnd1 convexFeature =
    feature tagCapsuleConvex siteCapEnd1 convexFeature 0 0


capsuleCapEnd2 : Int -> Int
capsuleCapEnd2 convexFeature =
    feature tagCapsuleConvex siteCapEnd2 convexFeature 0 0


capsuleCylinder1 : Int -> Int
capsuleCylinder1 convexFeature =
    feature tagCapsuleConvex siteCapCyl1 convexFeature 0 0


capsuleCylinder2 : Int -> Int
capsuleCylinder2 convexFeature =
    feature tagCapsuleConvex siteCapCyl2 convexFeature 0 0


capsuleCylinder : Int -> Int
capsuleCylinder convexFeature =
    feature tagCapsuleConvex siteCapCyl convexFeature 0 0



-- DECODE


{-| Reproduce the contact's shape-pair + feature string, e.g. `"1-2-f0-f3-v1"`,
from its two ids. The body pair is not part of the id (it's the cache tree key),
so it does not appear here; callers that want it prepend the body ids.
-}
toString : Int -> Int -> String
toString shapes featureKey =
    let
        s2 =
            modBy shapeRange shapes

        s1 =
            shapes // shapeRange
    in
    String.fromInt s1
        ++ "-"
        ++ String.fromInt s2
        ++ featureString featureKey


{-| Reproduce only the feature suffix, e.g. `"-f0-f3-v1"` or `"-e1-f5"`, from a
contact's feature key (its `featureKey`).
-}
featureString : Int -> String
featureString featureKey =
    let
        d =
            modBy featRange featureKey

        c =
            modBy featRange (featureKey // featRange)

        b =
            modBy featRange (featureKey // (featRange * featRange))

        a =
            modBy featRange (featureKey // (featRange * featRange * featRange))

        tag =
            featureKey // (featRange * featRange * featRange * featRange)
    in
    if tag == tagSimple then
        ""

    else if tag == tagConvexFace then
        "-f" ++ String.fromInt a ++ "-f" ++ String.fromInt b ++ "-v" ++ String.fromInt c

    else if tag == tagConvexEdge then
        "-e" ++ String.fromInt a ++ "." ++ String.fromInt b ++ "-e" ++ String.fromInt c ++ "." ++ String.fromInt d

    else if tag == tagPlaneVertex then
        "-v" ++ String.fromInt a

    else if tag == tagPlaneCap then
        "-e" ++ String.fromInt a

    else if tag == tagSphereConvex then
        convexFeatureString a

    else if tag == tagCapsuleConvex then
        siteString a ++ convexFeatureString b

    else
        ""


convexFeatureString : Int -> String
convexFeatureString convexFeature =
    if convexFeature == cfNone then
        ""

    else if convexFeature == cfEdge then
        "-e"

    else if convexFeature == cfVertex then
        "-v"

    else
        "-f" ++ String.fromInt (convexFeature - cfFaceOffset)


siteString : Int -> String
siteString site =
    if site == siteCapEnd1 then
        "-e1"

    else if site == siteCapEnd2 then
        "-e2"

    else if site == siteCapCyl1 then
        "-c1"

    else if site == siteCapCyl2 then
        "-c2"

    else
        "-c"
