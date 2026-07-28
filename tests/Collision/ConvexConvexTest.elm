module Collision.ConvexConvexTest exposing
    ( addContacts
    , edgeOracle
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Collision.ConvexConvex
import Expect
import Fuzz exposing (Fuzzer)
import Internal.Const as Const
import Internal.ContactId as ContactId
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Internal.VertexBuffer as VertexBuffer
import Shapes.Convex as Convex
import Test exposing (Test, describe, fuzz, test)


addContacts : Test
addContacts =
    let
        -- Captured from the Collisions sandbox: a 1.5-cube whose local
        -- frame is ~47° off identity plus a ~0.3° tilt. Only the
        -- origin differs between the two scenarios below.
        tiltedBox origin =
            Convex.fromBlock 1.5 1.5 1.5
                |> Convex.placeIn
                    (Transform3d.fromOriginAndBasis
                        origin
                        { x = -0.001580161067349219, y = 0.9999882020975505, z = -0.004593338296621986 }
                        { x = -0.6852663896092609, y = -0.004428115539111141, z = -0.7282790447793068 }
                        { x = -0.7282907924468673, y = 0.0019968621580540736, z = 0.6852653020390244 }
                    )

        targetBox =
            Convex.fromBlock 2 2 2
                |> Convex.placeIn Transform3d.atOrigin
    in
    describe "Collision.ConvexConvex.addContacts"
        [ test "should return 4 results" <|
            \_ ->
                let
                    convex =
                        Convex.fromBlock 2 2 2

                    t1 =
                        -- going slightly into another box
                        Transform3d.atPoint { x = 0, y = 0, z = 2.1 }
                            |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)

                    t2 =
                        Transform3d.atPoint { x = 0, y = 0, z = 4 }
                            |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 2)
                in
                Collision.ConvexConvex.addContacts 0 (Convex.placeIn t1 convex) (Convex.placeIn t2 convex) []
                    |> List.length
                    |> Expect.equal 4
        , test "should return 2 results" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1.2 1.2 1.2

                    convex2 =
                        Convex.fromBlock 1 1 1

                    transform3d1 =
                        Transform3d.atPoint { x = -0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 2)

                    transform3d2 =
                        Transform3d.atPoint { x = 0.5, y = 0, z = 0 }
                            |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                in
                Collision.ConvexConvex.addContacts 0
                    (Convex.placeIn transform3d1 convex1)
                    (Convex.placeIn transform3d2 convex2)
                    []
                    |> List.length
                    |> Expect.equal 2
        , test "produces a single edge-edge contact when neither body's face normal is the SAT min" <|
            \_ ->
                let
                    box1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.atOrigin
                                    |> Transform3d.rotateAroundOwn Vec3.yAxis (pi / 6)
                                )

                    box2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.atPoint { x = 0.6, y = 0.6, z = 0.6 }
                                    |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 6)
                                )
                in
                Collision.ConvexConvex.addContacts 0 box1 box2 []
                    |> List.length
                    |> Expect.equal 1
        , test "axis-aligned corner-on-corner stays on the face-clip path" <|
            \_ ->
                let
                    box1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn Transform3d.atOrigin

                    box2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.7, y = 0.7, z = 0.7 })
                in
                Collision.ConvexConvex.addContacts 0 box1 box2 []
                    |> List.all (\c -> not (String.startsWith "-e" (ContactId.featureString c.featureKey)))
                    |> Expect.equal True
        , test "settled near-coplanar stack interface stays at four contacts (no degenerate over-count)" <|
            -- Real settling poses where the clip emitted 6 points (4 corners + 2 on
            -- the shared edges); ~1e-4 drift triggers it, exact coplanarity doesn't.
            \_ ->
                let
                    box2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.fromOriginAndBasis
                                    { x = 0.0002598859790008731, y = 0.00013646414197537694, z = 1.4996999282620576 }
                                    { x = 0.999999999331571, y = 0.0000032595970446131582, z = -0.00003641748174313937 }
                                    { x = -0.0000032582908802255252, y = 0.999999999351492, z = 0.00003586641330410744 }
                                    { x = 0.00003641759862957714, y = -0.000035866294621384654, z = 0.9999999986936836 }
                                )

                    box3 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.fromOriginAndBasis
                                    { x = 0.0001967863351834402, y = 0.000008252933652183521, z = 2.499616662498541 }
                                    { x = 0.9999999861179005, y = 0.000002032073717667724, z = 0.00016661353315350634 }
                                    { x = -0.0000020688652840958605, y = 0.9999999756171408, z = 0.00022081992116338665 }
                                    { x = -0.00016661308036863387, y = -0.00022082026279889716, z = 0.9999999617392458 }
                                )
                in
                Collision.ConvexConvex.addContacts 0 box2 box3 []
                    |> List.length
                    |> Expect.equal 4
        , test "tilted box with edge crossing target top face emits two face-face contacts" <|
            \_ ->
                Collision.ConvexConvex.addContacts 0
                    (tiltedBox { x = -0.11608751888227789, y = 0.5683352706207844, z = 1.9053833288923054 })
                    targetBox
                    []
                    |> List.map (\c -> ContactId.featureString c.featureKey)
                    |> List.sort
                    |> Expect.equal [ "-f3-f5-v5", "-f3-f5-v6" ]
        , test "same tilt at shifted y-offset emits two face-face contacts" <|
            \_ ->
                Collision.ConvexConvex.addContacts 0
                    (tiltedBox { x = -0.06790182997043828, y = -0.6276944695546394, z = 1.9053833288923054 })
                    targetBox
                    []
                    |> List.map (\c -> ContactId.featureString c.featureKey)
                    |> List.sort
                    |> Expect.equal [ "-f3-f5-v5", "-f3-f5-v6" ]
        ]


testSeparatingAxis : Test
testSeparatingAxis =
    describe "separationAlong"
        [ test "returns Just depth" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (separationAlong convex1 convex2 Vec3.xAxis)
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -5.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (separationAlong convex1 convex2 Vec3.xAxis)
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    let
                        convex1 =
                            Convex.fromBlock 1 1 1
                                |> Convex.placeIn (Transform3d.atPoint { x = 1, y = 0, z = 0 })

                        convex2 =
                            Convex.fromBlock 1 1 1
                                |> Convex.placeIn
                                    (Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                                        |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                                    )
                    in
                    separationAlong convex1 convex2 Vec3.xAxis
                of
                    Nothing ->
                        Expect.fail "expected depth"

                    Just value ->
                        Expect.within (Expect.Absolute 0.00001) 0.4071067 value
        ]


findSeparatingAxis : Test
findSeparatingAxis =
    describe "Collision.ConvexConvex.findSeparatingAxis"
        [ test "works for offset" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0.2, y = 0, z = 0 })
                in
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis convex1 convex2)
                    (Just { x = -1, y = 0, z = 0 })
        , test "works for rotation" <|
            \_ ->
                let
                    convex1 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = -0.2, y = 0, z = 0 })

                    convex2 =
                        Convex.fromBlock 1 1 1
                            |> Convex.placeIn
                                (Transform3d.atPoint { x = 0.2, y = 0, z = 0 }
                                    |> Transform3d.rotateAroundOwn Vec3.zAxis (pi / 4)
                                )
                in
                Expect.equal
                    (Collision.ConvexConvex.findSeparatingAxis convex1 convex2)
                    (Just { x = -1, y = 0, z = 0 })
        ]


project : Test
project =
    describe "Collision.ConvexConvex.project"
        [ test "works for the positive x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.xAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.convexVertices (Convex.fromBlock 1 1 1))
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        { x = -1, y = 0, z = 0 }
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.convexVertices (Convex.fromBlock 1 1 1))
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.convexVertices (Convex.fromBlock 1 1 1))
                    )
                    { min = -0.5, max = 0.5 }
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (Collision.ConvexConvex.project
                        Vec3.yAxis
                        Const.maxNumber
                        -Const.maxNumber
                        (Convex.fromBlock 1 1 1
                            |> Convex.placeIn (Transform3d.atPoint { x = 0, y = 1, z = 0 })
                            |> Convex.convexVertices
                        )
                    )
                    { min = 0.5, max = 1.5 }
        , test "works for the rotation and offset" <|
            \_ ->
                Collision.ConvexConvex.project
                    Vec3.yAxis
                    Const.maxNumber
                    -Const.maxNumber
                    (Convex.fromBlock 1 1 1
                        |> Convex.placeIn
                            (Transform3d.atPoint { x = 0, y = 1, z = 0 }
                                |> Transform3d.rotateAroundOwn Vec3.xAxis (pi / 2)
                            )
                        |> Convex.convexVertices
                    )
                    |> Expect.all
                        [ .min >> Expect.within (Expect.Absolute 0.00001) 0.5
                        , .max >> Expect.within (Expect.Absolute 0.00001) 1.5
                        ]
        ]


{-| Projection-based separation depth along `axis`: `Just depth` when the two
hulls' [min,max] intervals overlap, `Nothing` when they clear the contact
margin. An independent reference for the arc SAT — projects both hulls (via the
production `projectConvex`) and checks interval overlap, sharing no logic with
the Gauss-arc path it is used to cross-check.
-}
separationAlong : Convex.Convex -> Convex.Convex -> Vec3.Vec3 -> Maybe Float
separationAlong convex1 convex2 axis =
    let
        o =
            overlapAlong convex1 convex2 axis
    in
    if o + Const.contactBreakingThreshold < 0 then
        Nothing

    else
        Just o


{-| Interval overlap of the two hulls' projections along `axis` (negative =
gap), via the production `projectConvex` — no logic shared with the arc path.
-}
overlapAlong : Convex.Convex -> Convex.Convex -> Vec3.Vec3 -> Float
overlapAlong convex1 convex2 axis =
    let
        p1 =
            Collision.ConvexConvex.projectConvex axis convex1

        p2 =
            Collision.ConvexConvex.projectConvex axis convex2
    in
    min (p1.max - p2.min) (p2.max - p1.min)


{-| Independent reimplementation of the certified axis set: face normals
plus old-style fan containment on the normalized cross, in world frame.
`strictMin` covers the axes the arc SAT tests, `looseMin` also the
near-parallel pairs it prunes; a pose whose verdict hangs on a pruned pair
or sits within `oracleBand` of the contact threshold is legitimately
either way — skipped.
-}
edgeOracle : Test
edgeOracle =
    describe "Collision.ConvexConvex.findSeparatingAxis oracle"
        [ fuzz poseFuzzer "agrees with the containment oracle" <|
            \pose ->
                let
                    convex1 =
                        Convex.placeIn
                            (Transform3d.atOrigin
                                |> Transform3d.rotateAroundOwn pose.axis1 pose.angle1
                            )
                            pose.shape1

                    convex2 =
                        Convex.placeIn
                            (Transform3d.atPoint (Vec3.scale pose.offset pose.offsetDir)
                                |> Transform3d.rotateAroundOwn pose.axis2 pose.angle2
                            )
                            pose.shape2

                    satSeparated =
                        Collision.ConvexConvex.findSeparatingAxis convex1 convex2 == Nothing

                    faceMin =
                        List.foldl (\n o -> min o (overlapAlong convex1 convex2 n))
                            Const.maxNumber
                            (List.map Convex.faceGroupNormal convex1.faces
                                ++ List.map Convex.faceGroupNormal convex2.faces
                            )

                    ( strictMin, looseMin ) =
                        edgeMinOverlaps convex1 convex2 faceMin
                in
                if strictMin + Const.contactBreakingThreshold + oracleBand < 0 then
                    satSeparated
                        |> Expect.equal True
                        |> Expect.onFail "oracle separated, arc SAT colliding"

                else if looseMin + Const.contactBreakingThreshold - oracleBand > 0 then
                    satSeparated
                        |> Expect.equal False
                        |> Expect.onFail "oracle colliding, arc SAT separated"

                else
                    Expect.pass
        ]


{-| FP slack between the oracle's arithmetic and the arc SAT's, including
its scan noise floors.
-}
oracleBand : Float
oracleBand =
    1.0e-5


type alias Pose =
    { shape1 : Convex.Convex
    , shape2 : Convex.Convex
    , axis1 : Vec3.Vec3
    , angle1 : Float
    , axis2 : Vec3.Vec3
    , angle2 : Float
    , offsetDir : Vec3.Vec3
    , offset : Float
    }


poseFuzzer : Fuzzer Pose
poseFuzzer =
    Fuzz.constant Pose
        |> Fuzz.andMap shapeFuzzer
        |> Fuzz.andMap shapeFuzzer
        |> Fuzz.andMap directionFuzzer
        |> Fuzz.andMap (Fuzz.floatRange 0 (2 * pi))
        |> Fuzz.andMap directionFuzzer
        |> Fuzz.andMap (Fuzz.floatRange 0 (2 * pi))
        |> Fuzz.andMap directionFuzzer
        |> Fuzz.andMap (Fuzz.floatRange 0.8 3)


shapeFuzzer : Fuzzer Convex.Convex
shapeFuzzer =
    Fuzz.oneOfValues
        [ Convex.fromBlock 1.5 1 2
        , Convex.fromCylinder 5 0.8 1.6
        , Convex.fromCone 6 0.9 1.4
        ]


directionFuzzer : Fuzzer Vec3.Vec3
directionFuzzer =
    Fuzz.map3
        (\x y z ->
            let
                v =
                    { x = x, y = y, z = z }
            in
            if Vec3.lengthSquared v < 1.0e-4 then
                Vec3.zAxis

            else
                Vec3.normalize v
        )
        (Fuzz.floatRange -1 1)
        (Fuzz.floatRange -1 1)
        (Fuzz.floatRange -1 1)


{-| Minimum overlap over the certified edge axes: fan containment of the
normalized cross in both groups, measured from one endpoint of each support
edge. First component covers the pairs the arc SAT tests, second also the
near-parallel pairs it prunes.
-}
edgeMinOverlaps : Convex.Convex -> Convex.Convex -> Float -> ( Float, Float )
edgeMinOverlaps convex1 convex2 start =
    List.foldl
        (\group1 acc1 ->
            List.foldl
                (\group2 ( s, l ) ->
                    let
                        d1 =
                            Transform3d.rotate convex1.orientation group1.dir

                        d2 =
                            Transform3d.rotate convex2.orientation group2.dir

                        c =
                            Vec3.cross d1 d2
                    in
                    if Vec3.lengthSquared c < 1.0e-12 then
                        ( s, l )

                    else
                        case pairMinOverlap convex1 convex2 d1 d2 (Vec3.normalize c) group1 group2 of
                            Nothing ->
                                ( s, l )

                            Just o ->
                                if Vec3.lengthSquared c < Const.parallelTolerance then
                                    ( s, min l o )

                                else
                                    ( min s o, min l o )
                )
                acc1
                convex2.uniqueEdges
        )
        ( start, start )
        convex1.uniqueEdges


{-| Support-consistent candidates for one direction pair; `Just` the
smallest overlap, `Nothing` when no orientation is support-consistent.
-}
pairMinOverlap : Convex.Convex -> Convex.Convex -> Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Convex.EdgeGroup -> Convex.EdgeGroup -> Maybe Float
pairMinOverlap convex1 convex2 d1 d2 u group1 group2 =
    let
        hit1 =
            List.filterMap (fanHit convex1 d1 u) group1.edges

        hit2 =
            List.filterMap (fanHit convex2 d2 u) group2.edges
    in
    List.minimum
        (List.concatMap
            (\( s1, w1 ) ->
                List.filterMap
                    (\( s2, w2 ) ->
                        if s1 > 0 && s2 < 0 then
                            Just (Vec3.dot u (Vec3.sub w1 w2))

                        else if s1 < 0 && s2 > 0 then
                            Just (Vec3.dot u (Vec3.sub w2 w1))

                        else
                            Nothing
                    )
                    hit2
            )
            hit1
        )


{-| Fan containment of `±u` for one edge, in world frame: returns the sign
of the contained orientation and one edge endpoint.
-}
fanHit : Convex.Convex -> Vec3.Vec3 -> Vec3.Vec3 -> Convex.Edge -> Maybe ( Float, Vec3.Vec3 )
fanHit convex d u edge =
    let
        nA =
            Transform3d.rotate convex.orientation edge.nA

        nB =
            Transform3d.rotate convex.orientation edge.nB

        pA =
            Vec3.dot u (Vec3.cross d nA)

        pB =
            Vec3.dot u (Vec3.cross nB d)
    in
    if pA > 0 && pB > 0 then
        Just ( 1, VertexBuffer.get edge.i1 convex.vertexBuffer )

    else if pA < 0 && pB < 0 then
        Just ( -1, VertexBuffer.get edge.i1 convex.vertexBuffer )

    else
        Nothing
