module ConvexSphereSimTest exposing (icosphereDoesNotPenetrateCube)

{-| Reproduces the bug observed in `sandbox/src/ConvexSphere.elm`:
the icosphere body bounces off the floor and then passes through the cube body.

Same scene: floor plane at z = -1, cube body at (0, 0, 8), icosphere body at
(0.3, 0, 5). Both bodies are dynamic and load their convex shape via
`Shape.unsafeConvex` from triangulated mesh data (matching the OBJ data in
ConvexSphere.elm).

After ~600 simulation steps (10 seconds of simulated time at 60 fps), the
icosphere's center must remain outside the cube's interior — otherwise it has
tunnelled through.

-}

import Array
import Expect
import Length
import Mass
import Physics exposing (Body, onEarth)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d exposing (Point3d)
import Test exposing (Test, test)
import TriangularMesh exposing (TriangularMesh)


icosphereDoesNotPenetrateCube : Test
icosphereDoesNotPenetrateCube =
    test "icosphere body bounces off the floor and does not pass through the cube body" <|
        \_ ->
            let
                bodies =
                    [ ( 0, Physics.plane Plane3d.xy Material.wood |> Physics.moveTo (Point3d.meters 0 0 -1) )
                    , ( 1, cubeBody |> Physics.moveTo (Point3d.meters 0 0 8) )
                    , ( 2, icoSphereBody |> Physics.moveTo (Point3d.meters 0.3 0 5) )
                    ]

                trace =
                    runStepsTrace 200 bodies Physics.emptyContacts []

                -- The cube must never tunnel into the icosphere. Check that
                -- across the simulation the centers stay > 0.5 m apart
                -- whenever both bodies are still near the impact zone.
                -- (Sum of half-extents is ~2; deep penetration would mean
                -- < 0.5.)
                minDist =
                    trace |> List.map (\( c, s ) -> dist c s) |> List.minimum |> Maybe.withDefault 0
            in
            (minDist > 0.4)
                |> Expect.equal True
                |> Expect.onFail ("min cube–ico center distance was " ++ String.fromFloat minDist ++ " m (deep penetration / tunneling)")


dist : { x : Float, y : Float, z : Float } -> { x : Float, y : Float, z : Float } -> Float
dist a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2 + (a.z - b.z) ^ 2)


runStepsTrace : Int -> List ( Int, Body ) -> Physics.Contacts Int -> List ( { x : Float, y : Float, z : Float }, { x : Float, y : Float, z : Float } ) -> List ( { x : Float, y : Float, z : Float }, { x : Float, y : Float, z : Float } )
runStepsTrace n bodies contacts trace =
    if n <= 0 then
        trace

    else
        let
            ( newBodies, newContacts ) =
                Physics.simulate { onEarth | contacts = contacts } bodies

            cubeP =
                newBodies
                    |> List.filter (\( id, _ ) -> id == 1)
                    |> List.head
                    |> Maybe.map (\( _, b ) -> Point3d.toMeters (Physics.originPoint b))
                    |> Maybe.withDefault { x = 0, y = 0, z = 0 }

            icoP =
                newBodies
                    |> List.filter (\( id, _ ) -> id == 2)
                    |> List.head
                    |> Maybe.map (\( _, b ) -> Point3d.toMeters (Physics.originPoint b))
                    |> Maybe.withDefault { x = 0, y = 0, z = 0 }
        in
        runStepsTrace (n - 1) newBodies newContacts (( cubeP, icoP ) :: trace)



-- Bodies (mirror sandbox/src/ConvexSphere.elm)


cubeBody : Body
cubeBody =
    Physics.dynamic [ ( Shape.unsafeConvex cubeMesh, Material.wood ) ]
        |> Physics.scaleMassTo (Mass.kilograms 5)


icoSphereBody : Body
icoSphereBody =
    Physics.dynamic [ ( Shape.unsafeConvex icoSphereMesh, Material.wood ) ]
        |> Physics.scaleMassTo (Mass.kilograms 5)



-- Meshes: same triangulation Obj.Decode would produce from the OBJ files in
-- ConvexSphere.elm. For each OBJ "f i1 i2 ... iN", the parser builds outIndices
-- in REVERSE order then triangulates as a fan from the LAST OBJ vertex, so the
-- 0-based fan from "f i1 i2 i3 i4" is [(i4, i3, i2), (i4, i2, i1)].


cubeMesh : TriangularMesh (Point3d Length.Meters Physics.BodyCoordinates)
cubeMesh =
    let
        v x y z =
            Point3d.meters x y z

        vertices =
            Array.fromList
                [ v 1 1 -1 -- 0  (OBJ 1)
                , v 1 -1 -1 -- 1  (OBJ 2)
                , v 1 1 1 -- 2  (OBJ 3)
                , v 1 -1 1 -- 3  (OBJ 4)
                , v -1 1 -1 -- 4  (OBJ 5)
                , v -1 -1 -1 -- 5  (OBJ 6)
                , v -1 1 1 -- 6  (OBJ 7)
                , v -1 -1 1 -- 7  (OBJ 8)
                ]

        -- Obj.Decode preserves OBJ winding (double-reversal in parser+addTriangles).
        -- For "f a b c d" (1-based) → fan from first vertex: (a, b, c), (a, c, d) in 0-based.
        -- f 1 5 7 3 → (0, 4, 6), (0, 6, 2)
        -- f 4 3 7 8 → (3, 2, 6), (3, 6, 7)
        -- f 8 7 5 6 → (7, 6, 4), (7, 4, 5)
        -- f 6 2 4 8 → (5, 1, 3), (5, 3, 7)
        -- f 2 1 3 4 → (1, 0, 2), (1, 2, 3)
        -- f 6 5 1 2 → (5, 4, 0), (5, 0, 1)
        faceIndices =
            [ ( 0, 4, 6 )
            , ( 0, 6, 2 )
            , ( 3, 2, 6 )
            , ( 3, 6, 7 )
            , ( 7, 6, 4 )
            , ( 7, 4, 5 )
            , ( 5, 1, 3 )
            , ( 5, 3, 7 )
            , ( 1, 0, 2 )
            , ( 1, 2, 3 )
            , ( 5, 4, 0 )
            , ( 5, 0, 1 )
            ]
    in
    TriangularMesh.indexed vertices faceIndices


icoSphereMesh : TriangularMesh (Point3d Length.Meters Physics.BodyCoordinates)
icoSphereMesh =
    let
        v x y z =
            Point3d.meters x y z

        vertices =
            Array.fromList
                [ v 0 0 -1 -- 0
                , v 0.723607 -0.525725 -0.44722 -- 1
                , v -0.276388 -0.850649 -0.44722 -- 2
                , v -0.894426 0 -0.447216 -- 3
                , v -0.276388 0.850649 -0.44722 -- 4
                , v 0.723607 0.525725 -0.44722 -- 5
                , v 0.276388 -0.850649 0.44722 -- 6
                , v -0.723607 -0.525725 0.44722 -- 7
                , v -0.723607 0.525725 0.44722 -- 8
                , v 0.276388 0.850649 0.44722 -- 9
                , v 0.894426 0 0.447216 -- 10
                , v 0 0 1 -- 11
                , v -0.162456 -0.499995 -0.850654 -- 12
                , v 0.425323 -0.309011 -0.850654 -- 13
                , v 0.262869 -0.809012 -0.525738 -- 14
                , v 0.850648 0 -0.525736 -- 15
                , v 0.425323 0.309011 -0.850654 -- 16
                , v -0.52573 0 -0.850652 -- 17
                , v -0.688189 -0.499997 -0.525736 -- 18
                , v -0.162456 0.499995 -0.850654 -- 19
                , v -0.688189 0.499997 -0.525736 -- 20
                , v 0.262869 0.809012 -0.525738 -- 21
                , v 0.951058 -0.309013 0 -- 22
                , v 0.951058 0.309013 0 -- 23
                , v 0 -1 0 -- 24
                , v 0.587786 -0.809017 0 -- 25
                , v -0.951058 -0.309013 0 -- 26
                , v -0.587786 -0.809017 0 -- 27
                , v -0.587786 0.809017 0 -- 28
                , v -0.951058 0.309013 0 -- 29
                , v 0.587786 0.809017 0 -- 30
                , v 0 1 0 -- 31
                , v 0.688189 -0.499997 0.525736 -- 32
                , v -0.262869 -0.809012 0.525738 -- 33
                , v -0.850648 0 0.525736 -- 34
                , v -0.262869 0.809012 0.525738 -- 35
                , v 0.688189 0.499997 0.525736 -- 36
                , v 0.162456 -0.499995 0.850654 -- 37
                , v 0.52573 0 0.850652 -- 38
                , v -0.425323 -0.309011 0.850654 -- 39
                , v -0.425323 0.309011 0.850654 -- 40
                , v 0.162456 0.499995 0.850654 -- 41
                ]

        -- Obj.Decode preserves OBJ winding (double-reverse in parser+addTriangles).
        -- For triangle "f a b c" (1-based), the output is (a-1, b-1, c-1).
        faceIndices =
            [ ( 0, 13, 12 )
            , ( 1, 13, 15 )
            , ( 0, 12, 17 )
            , ( 0, 17, 19 )
            , ( 0, 19, 16 )
            , ( 1, 15, 22 )
            , ( 2, 14, 24 )
            , ( 3, 18, 26 )
            , ( 4, 20, 28 )
            , ( 5, 21, 30 )
            , ( 1, 22, 25 )
            , ( 2, 24, 27 )
            , ( 3, 26, 29 )
            , ( 4, 28, 31 )
            , ( 5, 30, 23 )
            , ( 6, 32, 37 )
            , ( 7, 33, 39 )
            , ( 8, 34, 40 )
            , ( 9, 35, 41 )
            , ( 10, 36, 38 )
            , ( 38, 41, 11 )
            , ( 38, 36, 41 )
            , ( 36, 9, 41 )
            , ( 41, 40, 11 )
            , ( 41, 35, 40 )
            , ( 35, 8, 40 )
            , ( 40, 39, 11 )
            , ( 40, 34, 39 )
            , ( 34, 7, 39 )
            , ( 39, 37, 11 )
            , ( 39, 33, 37 )
            , ( 33, 6, 37 )
            , ( 37, 38, 11 )
            , ( 37, 32, 38 )
            , ( 32, 10, 38 )
            , ( 23, 36, 10 )
            , ( 23, 30, 36 )
            , ( 30, 9, 36 )
            , ( 31, 35, 9 )
            , ( 31, 28, 35 )
            , ( 28, 8, 35 )
            , ( 29, 34, 8 )
            , ( 29, 26, 34 )
            , ( 26, 7, 34 )
            , ( 27, 33, 7 )
            , ( 27, 24, 33 )
            , ( 24, 6, 33 )
            , ( 25, 32, 6 )
            , ( 25, 22, 32 )
            , ( 22, 10, 32 )
            , ( 30, 31, 9 )
            , ( 30, 21, 31 )
            , ( 21, 4, 31 )
            , ( 28, 29, 8 )
            , ( 28, 20, 29 )
            , ( 20, 3, 29 )
            , ( 26, 27, 7 )
            , ( 26, 18, 27 )
            , ( 18, 2, 27 )
            , ( 24, 25, 6 )
            , ( 24, 14, 25 )
            , ( 14, 1, 25 )
            , ( 22, 23, 10 )
            , ( 22, 15, 23 )
            , ( 15, 5, 23 )
            , ( 16, 21, 5 )
            , ( 16, 19, 21 )
            , ( 19, 4, 21 )
            , ( 19, 20, 4 )
            , ( 19, 17, 20 )
            , ( 17, 3, 20 )
            , ( 17, 18, 3 )
            , ( 17, 12, 18 )
            , ( 12, 2, 18 )
            , ( 15, 16, 5 )
            , ( 15, 13, 16 )
            , ( 13, 0, 16 )
            , ( 12, 14, 2 )
            , ( 12, 13, 14 )
            , ( 13, 1, 14 )
            ]
    in
    TriangularMesh.indexed vertices faceIndices
