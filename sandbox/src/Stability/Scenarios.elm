module Stability.Scenarios exposing
    ( Scenario
    , stackOf5
    , unitBlock
    )

{-| Repeatable, deterministic test scenarios for stability benchmarking.

Each scenario is a named initial body configuration. Body IDs follow the sandbox
convention: 0 = ground/floor, 1..n = dynamic bodies (array index into meshes).

The exposed shape constant (`unitBlock`) lets browser scenes build a
corresponding mesh.

Ground plane: z = 0, normal pointing +z.
Boxes: 1 m × 1 m × 1 m wood, centered at origin in body coordinates.

-}

import Block3d exposing (Block3d)
import Frame3d
import Length exposing (Meters)
import Physics exposing (BodyCoordinates)
import Physics.Material as Material
import Plane3d
import Point3d


type alias Scenario =
    { name : String
    , bodies : List ( Int, Physics.Body )
    }


unitBlock : Block3d Meters BodyCoordinates
unitBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 1, Length.meters 1, Length.meters 1 )


ground : ( Int, Physics.Body )
ground =
    ( 0, Physics.plane Plane3d.xy Material.wood )


{-| Five boxes placed at their exact resting positions, already touching, no drop.

Use with `consecutiveStableFrames` — the score starts near zero and rises as
solver drift accumulates, so the frame count until maxSpeed ≥ 0.05 m/s is a
clean single-number stability metric.

-}
stackOf5 : Scenario
stackOf5 =
    { name = "stack of 5 boxes"
    , bodies =
        let
            n =
                5
        in
        List.indexedMap
            (\i _ ->
                ( n - i
                , Physics.block unitBlock Material.wood
                    |> Physics.moveTo (Point3d.meters 0 0 (toFloat (n - i) - 0.5))
                )
            )
            (List.repeat n ())
            ++ [ ground ]
    }
