module CompoundVsLock exposing (main)

{-| This demo shows two possible ways to create complex objects.
One way is through a compound body out of multiple shapes.
The second way is by using the lock constraint.
-}

import Block3d exposing (Block3d)
import Common.Demo as Demo
import Common.Meshes as Meshes
import Dict exposing (Dict)
import Frame3d
import Length exposing (Meters)
import Mass
import Physics exposing (Body, BodyCoordinates)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d exposing (Point3d)


main : Program () (Demo.Model String ()) (Demo.Msg msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = initialBodies
                , lookupMesh = \_ id -> Dict.get id initialMeshes
                , camera =
                    { from = { x = 0, y = 30, z = 20 }
                    , to = { x = 0, y = 0, z = 0 }
                    }
                , initialState = ()
                }
    in
    Demo.program
        { base
            | constrain = \_ bodies -> constrain (Dict.fromList bodies)
            , collide = \_ a b -> not (isPiece a && isPiece b)
        }


isPiece : String -> Bool
isPiece id =
    id == "first" || id == "second" || id == "third"


constrain : Dict String Body -> String -> Maybe (String -> List Constraint)
constrain bodyDict id1 =
    case id1 of
        "first" ->
            Just
                (\id2 ->
                    case id2 of
                        "second" ->
                            case ( Dict.get "first" bodyDict, Dict.get "second" bodyDict ) of
                                ( Just b1, Just b2 ) ->
                                    [ lockTwoBodies b1 b2 ]

                                _ ->
                                    []

                        _ ->
                            []
                )

        "second" ->
            Just
                (\id2 ->
                    case id2 of
                        "third" ->
                            case ( Dict.get "second" bodyDict, Dict.get "third" bodyDict ) of
                                ( Just b1, Just b2 ) ->
                                    [ lockTwoBodies b1 b2 ]

                                _ ->
                                    []

                        _ ->
                            []
                )

        _ ->
            Nothing


lockTwoBodies : Body -> Body -> Constraint
lockTwoBodies b1 b2 =
    let
        center1 =
            Physics.originPoint b1

        center2 =
            Physics.originPoint b2

        middle =
            Point3d.midpoint center1 center2

        frame1 =
            Frame3d.atPoint (Point3d.relativeTo (Physics.frame b1) middle)

        frame2 =
            Frame3d.atPoint (Point3d.relativeTo (Physics.frame b2) middle)
    in
    Constraint.lock frame1 frame2


block3d : Block3d Meters BodyCoordinates
block3d =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 1
        , Length.meters 1
        , Length.meters 1
        )


pos1 : Point3d Meters BodyCoordinates
pos1 =
    Point3d.meters -0.5 0 -0.5


pos2 : Point3d Meters BodyCoordinates
pos2 =
    Point3d.meters -0.5 0 0.5


pos3 : Point3d Meters BodyCoordinates
pos3 =
    Point3d.meters 0.5 0 0.5


initialBodies : List ( String, Body )
initialBodies =
    let
        lockedPosition =
            Frame3d.atPoint (Point3d.meters -2 0 5)

        compoundPosition =
            Point3d.meters 2 0 5

        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        blocks =
            List.map
                (\center ->
                    Block3d.placeIn (Frame3d.atPoint center) block3d
                )
                [ pos1, pos2, pos3 ]

        compoundBody =
            Physics.dynamic
                (List.map (\b -> ( Shape.block b, Material.wood )) blocks)
                |> Physics.scaleMassTo (Mass.kilograms 5)
                |> Physics.moveTo compoundPosition

        boxBody =
            Physics.block block3d Material.wood
                |> Physics.scaleMassTo (Mass.kilograms 5)
    in
    [ ( "floor", floorBody )
    , ( "compound", compoundBody )
    , ( "first", boxBody |> Physics.moveTo (Point3d.placeIn lockedPosition pos1) )
    , ( "second", boxBody |> Physics.moveTo (Point3d.placeIn lockedPosition pos2) )
    , ( "third", boxBody |> Physics.moveTo (Point3d.placeIn lockedPosition pos3) )
    ]


initialMeshes : Dict String Meshes.Meshes
initialMeshes =
    let
        blocks =
            List.map
                (\center ->
                    Block3d.placeIn (Frame3d.atPoint center) block3d
                )
                [ pos1, pos2, pos3 ]

        boxMesh =
            Meshes.fromTriangles (Meshes.block block3d)
    in
    Dict.fromList
        [ ( "floor", Meshes.fromTriangles [] )
        , ( "compound", Meshes.fromTriangleGroups (List.map Meshes.block blocks) )
        , ( "first", boxMesh )
        , ( "second", boxMesh )
        , ( "third", boxMesh )
        ]
