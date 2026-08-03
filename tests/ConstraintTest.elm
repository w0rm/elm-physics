module ConstraintTest exposing (suite)

{-| End-to-end accuracy tests for the constraint (joint) solver: pivots must
not drift apart, hinge axes must stay aligned under sustained gravity load,
and statically loaded joints must settle to rest.
-}

import Angle
import Axis3d
import Block3d
import Direction3d
import Expect
import Frame3d
import Length
import Physics exposing (Body, onEarth)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape
import Point3d exposing (Point3d)
import Speed
import Test exposing (Test, describe, test)
import Vector3d


{-| A small static block to anchor joints to.
-}
anchor : Body
anchor =
    Physics.static
        [ ( Shape.block (Block3d.centeredOn Frame3d.atOrigin ( Length.meters 0.2, Length.meters 0.2, Length.meters 0.2 )), Material.wood )
        ]


unitBox : Body
unitBox =
    Physics.block
        (Block3d.centeredOn Frame3d.atOrigin
            ( Length.meters 1, Length.meters 1, Length.meters 1 )
        )
        Material.wood


{-| Thread contacts through `steps` simulation calls, waking all bodies each
step so sleeping can't freeze the scene mid-measurement.
-}
run : Int -> (Int -> Maybe (Int -> List Constraint)) -> List ( Int, Body ) -> List ( Int, Body )
run steps constrain bodies =
    loop steps constrain Physics.emptyContacts bodies


loop : Int -> (Int -> Maybe (Int -> List Constraint)) -> Physics.Contacts Int -> List ( Int, Body ) -> List ( Int, Body )
loop steps constrain contacts bodies =
    if steps <= 0 then
        bodies

    else
        let
            ( nextBodies, nextContacts ) =
                Physics.simulate { onEarth | contacts = contacts, constrain = constrain }
                    (List.map (Tuple.mapSecond Physics.wake) bodies)
        in
        loop (steps - 1) constrain nextContacts nextBodies


lookup : Int -> List ( Int, Body ) -> Maybe Body
lookup wanted bodies =
    bodies
        |> List.filter (\( id, _ ) -> id == wanted)
        |> List.head
        |> Maybe.map Tuple.second


{-| World-space distance between the two bodies' pivot points, in meters.
-}
pivotGap : Point3d Length.Meters Physics.BodyCoordinates -> Point3d Length.Meters Physics.BodyCoordinates -> Body -> Body -> Float
pivotGap pivot1 pivot2 body1 body2 =
    Length.inMeters
        (Point3d.distanceFrom
            (Point3d.placeIn (Physics.frame body1) pivot1)
            (Point3d.placeIn (Physics.frame body2) pivot2)
        )


speed : Body -> Float
speed body =
    Speed.inMetersPerSecond (Vector3d.length (Physics.velocity body))


constrainPair : List Constraint -> Int -> Maybe (Int -> List Constraint)
constrainPair constraints id1 =
    if id1 == 0 then
        Just
            (\id2 ->
                if id2 == 1 then
                    constraints

                else
                    []
            )

    else
        Nothing


suite : Test
suite =
    describe "constraints"
        [ test "pointToPoint: swinging pendulum keeps the pivots together" <|
            \_ ->
                let
                    -- box hanging 1 m below the anchor by its top-center point,
                    -- swung 30° to the side around the pivot so it keeps moving
                    pivotOnBox =
                        Point3d.meters 0 0 1

                    scene =
                        [ ( 0, anchor )
                        , ( 1
                          , unitBox
                                |> Physics.moveTo (Point3d.meters 0 0 -1)
                                |> Physics.rotateAround Axis3d.y (Angle.degrees 30)
                          )
                        ]

                    result =
                        run 300
                            (constrainPair [ Constraint.pointToPoint Point3d.origin pivotOnBox ])
                            scene
                in
                case ( lookup 0 result, lookup 1 result ) of
                    ( Just anchorBody, Just box ) ->
                        pivotGap Point3d.origin pivotOnBox anchorBody box
                            |> Expect.atMost 0.002

                    _ ->
                        Expect.fail "bodies are missing"
        , test "hinge: sideways-loaded door keeps its axis aligned and settles" <|
            \_ ->
                let
                    -- door hangs off the side of a vertical hinge, so gravity
                    -- loads the rotational rows with a constant torque
                    doorAxis =
                        Axis3d.through (Point3d.meters 1 0 0) Direction3d.z

                    scene =
                        [ ( 0, anchor )
                        , ( 1
                          , Physics.block
                                (Block3d.centeredOn Frame3d.atOrigin
                                    ( Length.meters 1, Length.meters 0.2, Length.meters 0.2 )
                                )
                                Material.wood
                                |> Physics.moveTo (Point3d.meters -1 0 0)
                          )
                        ]

                    result =
                        run 300
                            (constrainPair [ Constraint.hinge Axis3d.z doorAxis ])
                            scene
                in
                case ( lookup 0 result, lookup 1 result ) of
                    ( Just anchorBody, Just door ) ->
                        let
                            axisError =
                                1 - Direction3d.zComponent (Direction3d.placeIn (Physics.frame door) Direction3d.z)
                        in
                        Expect.all
                            [ \_ -> axisError |> Expect.atMost 0.00001
                            , \_ -> pivotGap Point3d.origin (Point3d.meters 1 0 0) anchorBody door |> Expect.atMost 0.002
                            , \_ -> speed door |> Expect.atMost 0.000001
                            ]
                            ()

                    _ ->
                        Expect.fail "bodies are missing"
        , test "pointToPoint: swinging 5-box chain stays taut" <|
            \_ ->
                let
                    -- boxes chained face-to-face off the anchor, started
                    -- horizontally so the chain whips down before settling
                    chainLink id1 =
                        Just
                            (\id2 ->
                                if id2 - id1 == 1 then
                                    [ Constraint.pointToPoint
                                        (Point3d.meters 0.5 0 0)
                                        (Point3d.meters -0.5 0 0)
                                    ]

                                else
                                    []
                            )

                    scene =
                        ( 0, anchor )
                            :: List.map
                                (\i -> ( i, unitBox |> Physics.moveTo (Point3d.meters (toFloat i) 0 0) ))
                                (List.range 1 5)

                    result =
                        run 600 chainLink scene

                    gaps =
                        List.map2
                            (\( _, upper ) ( _, lower ) ->
                                pivotGap (Point3d.meters 0.5 0 0) (Point3d.meters -0.5 0 0) upper lower
                            )
                            result
                            (List.drop 1 result)

                    speeds =
                        List.map (\( _, body ) -> speed body) result
                in
                Expect.all
                    [ \_ -> List.maximum gaps |> Maybe.withDefault (1 / 0) |> Expect.atMost 0.005
                    , -- an energy bound: the chain may still swing, but must not blow up
                      \_ -> List.maximum speeds |> Maybe.withDefault (1 / 0) |> Expect.atMost 10
                    ]
                    ()
        , test "lock: cantilevered box stays rigidly attached and settles" <|
            \_ ->
                let
                    -- box locked 1 m off the side of the anchor: gravity loads
                    -- both the translational and the rotational rows
                    frameOnBox =
                        Frame3d.atPoint (Point3d.meters 1 0 0)

                    scene =
                        [ ( 0, anchor )
                        , ( 1, unitBox |> Physics.moveTo (Point3d.meters -1 0 0) )
                        ]

                    result =
                        run 300
                            (constrainPair [ Constraint.lock Frame3d.atOrigin frameOnBox ])
                            scene
                in
                case ( lookup 0 result, lookup 1 result ) of
                    ( Just anchorBody, Just box ) ->
                        let
                            axisError =
                                1 - Direction3d.zComponent (Direction3d.placeIn (Physics.frame box) Direction3d.z)
                        in
                        Expect.all
                            [ \_ -> axisError |> Expect.atMost 0.00001
                            , \_ -> pivotGap Point3d.origin (Point3d.meters 1 0 0) anchorBody box |> Expect.atMost 0.001
                            , \_ -> speed box |> Expect.atMost 0.000001
                            ]
                            ()

                    _ ->
                        Expect.fail "bodies are missing"
        ]
