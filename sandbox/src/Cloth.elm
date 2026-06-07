module Cloth exposing (main)

{-| Cloth simulation built using many particle bodies,
connected with distance constraints.
-}

import Array exposing (Array)
import Common.Demo as Demo
import Common.Meshes as Meshes
import Length
import Mass
import Physics exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Plane3d
import Point3d
import Sphere3d


particlesPerDimension : Int
particlesPerDimension =
    10


distanceBetweenParticles : Float
distanceBetweenParticles =
    0.5


{-| IDs:

  - 0 = floor
  - 1 = sphere
  - 2 + x \* particlesPerDimension + y = particle at (x, y)

-}
particleId : Int -> Int -> Int
particleId x y =
    2 + x * particlesPerDimension + y


main : Program () (Demo.Model Int ()) (Demo.Msg msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = initialBodies
                , lookupMesh = \_ id -> Array.get id initialMeshes
                , camera =
                    { from = { x = 0, y = 30, z = 20 }
                    , to = { x = 0, y = 0, z = 0 }
                    }
                , initialState = ()
                }
    in
    Demo.program { base | constrain = \_ _ -> constrainCloth }


{-| Constraints between adjacent particles using id arithmetic.
-}
constrainCloth : Int -> Maybe (Int -> List Constraint)
constrainCloth id1 =
    let
        n =
            particlesPerDimension

        firstParticleId =
            2

        lastParticleId =
            2 + n * n - 1
    in
    if id1 - firstParticleId < 0 || id1 - lastParticleId > 0 then
        Nothing

    else
        Just
            (\id2 ->
                if id2 - firstParticleId < 0 || id2 - lastParticleId > 0 then
                    []

                else
                    let
                        idx1 =
                            id1 - firstParticleId

                        idx2 =
                            id2 - firstParticleId

                        x1 =
                            idx1 // n

                        y1 =
                            modBy n idx1

                        x2 =
                            idx2 // n

                        y2 =
                            modBy n idx2
                    in
                    if x1 == x2 && y2 - y1 == 1 || y1 == y2 && x2 - x1 == 1 then
                        [ Constraint.distance (Length.meters distanceBetweenParticles) ]

                    else
                        []
            )


initialBodies : List ( Int, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        sphere3d =
            Sphere3d.atOrigin (Length.meters 2)

        sphereBody =
            Physics.sphere sphere3d Material.wood
                |> Physics.scaleMassTo (Mass.kilograms 5)
                |> Physics.moveTo (Point3d.meters 0 0 1)

        dimensions =
            List.range 0 (particlesPerDimension - 1)

        particleMaterial =
            Material.surface { friction = 0.3, bounciness = 0 }

        particles =
            List.concatMap
                (\x ->
                    List.map
                        (\y ->
                            ( particleId x y
                            , Physics.pointMass
                                (Point3d.meters
                                    ((toFloat x - (toFloat particlesPerDimension - 1) / 2) * distanceBetweenParticles)
                                    ((toFloat y - (toFloat particlesPerDimension - 1) / 2) * distanceBetweenParticles)
                                    8
                                )
                                (Mass.kilograms 5)
                                particleMaterial
                            )
                        )
                        dimensions
                )
                dimensions
    in
    ( 0, floorBody ) :: ( 1, sphereBody ) :: particles


initialMeshes : Array Meshes.Meshes
initialMeshes =
    let
        sphere3d =
            Sphere3d.atOrigin (Length.meters 2)

        particleSphere3d =
            Sphere3d.atOrigin (Length.meters 0.1)

        particleCount =
            particlesPerDimension * particlesPerDimension

        particleMesh =
            Meshes.fromTriangles (Meshes.sphere 1 particleSphere3d)
    in
    Array.fromList
        (Meshes.fromTriangles []
            :: Meshes.fromTriangles (Meshes.sphere 3 sphere3d)
            :: List.repeat particleCount particleMesh
        )
