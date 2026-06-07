module UnsafeConvex exposing (main)

{-| This is used to demonstrate loading `unsafeConvex` shape from the OBJ file!
-}

import Array exposing (Array)
import Common.Demo as Demo
import Common.Meshes as Meshes
import Frame3d
import Length
import Mass
import Obj.Decode
import Physics exposing (Body)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d


main : Program () (Demo.Model Int ()) (Demo.Msg msg)
main =
    Demo.program
        (Demo.defaults
            { initialBodies = initialBodies
            , lookupMesh = \_ id -> Array.get id initialMeshes
            , camera =
                { from = { x = 0, y = 30, z = 20 }
                , to = { x = 0, y = 0, z = 0 }
                }
            , initialState = ()
            }
        )


icoSphereBody : Body
icoSphereBody =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) icoSphereObj of
        Ok triangularMesh ->
            Physics.dynamic [ ( Shape.unsafeConvex triangularMesh, Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 5)

        Err _ ->
            Physics.dynamic []


cubeBody : Body
cubeBody =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) cubeObj of
        Ok triangularMesh ->
            Physics.dynamic [ ( Shape.unsafeConvex triangularMesh, Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 5)

        Err _ ->
            Physics.dynamic []


wedgeBody : Body
wedgeBody =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) wedgeObj of
        Ok triangularMesh ->
            Physics.dynamic [ ( Shape.unsafeConvex triangularMesh, Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 5)

        Err _ ->
            Physics.dynamic []


tetraBody : Body
tetraBody =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) tetraObj of
        Ok triangularMesh ->
            Physics.dynamic [ ( Shape.unsafeConvex triangularMesh, Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 5)

        Err _ ->
            Physics.dynamic []


parallelepipedBody : Body
parallelepipedBody =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) parallelepipedObj of
        Ok triangularMesh ->
            Physics.dynamic [ ( Shape.unsafeConvex triangularMesh, Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 5)

        Err _ ->
            Physics.dynamic []


icoSphereMesh : Meshes.Meshes
icoSphereMesh =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) icoSphereObj of
        Ok triangularMesh ->
            Meshes.fromTriangles (Meshes.triangularMesh triangularMesh)

        Err _ ->
            Meshes.fromTriangles []


cubeMesh : Meshes.Meshes
cubeMesh =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) cubeObj of
        Ok triangularMesh ->
            Meshes.fromTriangles (Meshes.triangularMesh triangularMesh)

        Err _ ->
            Meshes.fromTriangles []


wedgeMesh : Meshes.Meshes
wedgeMesh =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) wedgeObj of
        Ok triangularMesh ->
            Meshes.fromTriangles (Meshes.triangularMesh triangularMesh)

        Err _ ->
            Meshes.fromTriangles []


tetraMesh : Meshes.Meshes
tetraMesh =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) tetraObj of
        Ok triangularMesh ->
            Meshes.fromTriangles (Meshes.triangularMesh triangularMesh)

        Err _ ->
            Meshes.fromTriangles []


parallelepipedMesh : Meshes.Meshes
parallelepipedMesh =
    case Obj.Decode.decodeString Length.meters (Obj.Decode.trianglesIn Frame3d.atOrigin) parallelepipedObj of
        Ok triangularMesh ->
            Meshes.fromTriangles (Meshes.triangularMesh triangularMesh)

        Err _ ->
            Meshes.fromTriangles []


initialBodies : List ( Int, Body )
initialBodies =
    [ ( 0, Physics.plane Plane3d.xy Material.wood |> Physics.moveTo (Point3d.fromMeters Demo.floorZ) )
    , ( 1, cubeBody |> Physics.moveTo (Point3d.meters 0 0 8) )
    , ( 2, icoSphereBody |> Physics.moveTo (Point3d.meters 0.3 0 5) )
    , ( 3, wedgeBody |> Physics.moveTo (Point3d.meters 0 -3 12) )
    , ( 4, tetraBody |> Physics.moveTo (Point3d.meters -3 3 15) )
    , ( 5, parallelepipedBody |> Physics.moveTo (Point3d.meters 4 0 18) )
    ]


initialMeshes : Array Meshes.Meshes
initialMeshes =
    Array.fromList
        [ Meshes.fromTriangles []
        , cubeMesh
        , icoSphereMesh
        , wedgeMesh
        , tetraMesh
        , parallelepipedMesh
        ]


cubeObj : String
cubeObj =
    """v 1.000000 1.000000 -1.000000
v 1.000000 -1.000000 -1.000000
v 1.000000 1.000000 1.000000
v 1.000000 -1.000000 1.000000
v -1.000000 1.000000 -1.000000
v -1.000000 -1.000000 -1.000000
v -1.000000 1.000000 1.000000
v -1.000000 -1.000000 1.000000
f 1 5 7 3
f 4 3 7 8
f 8 7 5 6
f 6 2 4 8
f 2 1 3 4
f 6 5 1 2
"""


{-| Right triangular prism (doorstop wedge): 3 long in x, 2 in y, 1.5 tall in z.
The asymmetric triangular cross-section in the xz plane gives non-axis-aligned
principal axes — two eigenvectors tilt within xz, one points along y.
-}
wedgeObj : String
wedgeObj =
    """v 0 -1 0
v 3 -1 0
v 3 -1 1.5
v 0 1 0
v 3 1 0
v 3 1 1.5
f 1 2 3
f 4 6 5
f 1 4 5 2
f 2 5 6 3
f 1 3 6 4
"""


{-| Irregular tetrahedron with no symmetry plane — all three eigenvectors
end up tilted relative to body xyz.
-}
tetraObj : String
tetraObj =
    """v -1 -1 -1
v 3 -1 -1
v -1 2 -1
v -1 -1 2.5
f 1 3 2
f 1 2 4
f 1 4 3
f 2 3 4
"""


{-| Sheared parallelepiped — a box whose top is shifted +2 in x relative to
its bottom. Symmetric in y, so one eigenvector stays along body-y; the other
two tilt within the xz plane along the parallelepiped's diagonal directions.
-}
parallelepipedObj : String
parallelepipedObj =
    """v -1 -1 -1
v 1 -1 -1
v 1 1 -1
v -1 1 -1
v 1 -1 1
v 3 -1 1
v 3 1 1
v 1 1 1
f 1 4 3 2
f 5 6 7 8
f 1 2 6 5
f 4 8 7 3
f 1 5 8 4
f 2 3 7 6
"""


icoSphereObj : String
icoSphereObj =
    """v 0.000000 0.000000 -1.000000
v 0.723607 -0.525725 -0.447220
v -0.276388 -0.850649 -0.447220
v -0.894426 0.000000 -0.447216
v -0.276388 0.850649 -0.447220
v 0.723607 0.525725 -0.447220
v 0.276388 -0.850649 0.447220
v -0.723607 -0.525725 0.447220
v -0.723607 0.525725 0.447220
v 0.276388 0.850649 0.447220
v 0.894426 0.000000 0.447216
v 0.000000 0.000000 1.000000
v -0.162456 -0.499995 -0.850654
v 0.425323 -0.309011 -0.850654
v 0.262869 -0.809012 -0.525738
v 0.850648 0.000000 -0.525736
v 0.425323 0.309011 -0.850654
v -0.525730 0.000000 -0.850652
v -0.688189 -0.499997 -0.525736
v -0.162456 0.499995 -0.850654
v -0.688189 0.499997 -0.525736
v 0.262869 0.809012 -0.525738
v 0.951058 -0.309013 0.000000
v 0.951058 0.309013 0.000000
v 0.000000 -1.000000 0.000000
v 0.587786 -0.809017 0.000000
v -0.951058 -0.309013 0.000000
v -0.587786 -0.809017 0.000000
v -0.587786 0.809017 0.000000
v -0.951058 0.309013 0.000000
v 0.587786 0.809017 0.000000
v 0.000000 1.000000 0.000000
v 0.688189 -0.499997 0.525736
v -0.262869 -0.809012 0.525738
v -0.850648 0.000000 0.525736
v -0.262869 0.809012 0.525738
v 0.688189 0.499997 0.525736
v 0.162456 -0.499995 0.850654
v 0.525730 0.000000 0.850652
v -0.425323 -0.309011 0.850654
v -0.425323 0.309011 0.850654
v 0.162456 0.499995 0.850654
s off
f 1 14 13
f 2 14 16
f 1 13 18
f 1 18 20
f 1 20 17
f 2 16 23
f 3 15 25
f 4 19 27
f 5 21 29
f 6 22 31
f 2 23 26
f 3 25 28
f 4 27 30
f 5 29 32
f 6 31 24
f 7 33 38
f 8 34 40
f 9 35 41
f 10 36 42
f 11 37 39
f 39 42 12
f 39 37 42
f 37 10 42
f 42 41 12
f 42 36 41
f 36 9 41
f 41 40 12
f 41 35 40
f 35 8 40
f 40 38 12
f 40 34 38
f 34 7 38
f 38 39 12
f 38 33 39
f 33 11 39
f 24 37 11
f 24 31 37
f 31 10 37
f 32 36 10
f 32 29 36
f 29 9 36
f 30 35 9
f 30 27 35
f 27 8 35
f 28 34 8
f 28 25 34
f 25 7 34
f 26 33 7
f 26 23 33
f 23 11 33
f 31 32 10
f 31 22 32
f 22 5 32
f 29 30 9
f 29 21 30
f 21 4 30
f 27 28 8
f 27 19 28
f 19 3 28
f 25 26 7
f 25 15 26
f 15 2 26
f 23 24 11
f 23 16 24
f 16 6 24
f 17 22 6
f 17 20 22
f 20 5 22
f 20 21 5
f 20 18 21
f 18 4 21
f 18 19 4
f 18 13 19
f 13 3 19
f 16 17 6
f 16 14 17
f 14 1 17
f 13 15 3
f 13 14 15
f 14 2 15
"""
