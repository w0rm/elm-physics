module Stability.Scenarios exposing
    ( Scenario
    , restingOnSlope
    , slopeRamp
    , stackOf5
    , stackOf5Dropped
    , stackOfCylinders
    , stackOfCylindersDropped
    , unitBlock
    , unitCylinder
    )

{-| Repeatable, deterministic test scenarios for stability benchmarking.

Each scenario is a named initial body configuration. Body IDs follow the sandbox
convention: 0 = ground/floor, 1..n = dynamic bodies (array index into meshes).

The exposed shape constant (`unitBlock`) lets browser scenes build a
corresponding mesh.

Ground plane: z = 0, normal pointing +z.
Boxes: 1 m × 1 m × 1 m wood, centered at origin in body coordinates.

-}

import Angle
import Axis3d
import Block3d exposing (Block3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Frame3d
import Length exposing (Meters)
import Physics exposing (BodyCoordinates)
import Physics.Material as Material
import Physics.Shape as Shape
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


{-| A unit-height wood cylinder, vertical axis, caps at z = ±0.5 so it stacks like
`unitBlock`. Exposed so browser scenes build a matching mesh.
-}
unitCylinder : Cylinder3d Meters BodyCoordinates
unitCylinder =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = Length.meters 0.5, length = Length.meters 1 }


ground : ( Int, Physics.Body )
ground =
    ( 0, Physics.plane Plane3d.xy Material.wood )


{-| Incline angle for `restingOnSlope`, in degrees. Chosen comfortably below
the wood-on-wood friction limit: combined μ = √(0.4·0.4) = 0.4, and
tan 15° ≈ 0.27 < 0.4, so static friction must hold the box in place. A box
that slides downhill means friction is failing to grip.
-}
slopeAngleDegrees : Float
slopeAngleDegrees =
    15


{-| The static ramp the slope box rests on: a wide, thin wood slab. Exposed
so browser scenes can build a matching mesh. Centered at origin in body
coordinates; `restingOnSlope` tilts and positions it.
-}
slopeRamp : Block3d Meters BodyCoordinates
slopeRamp =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 10, Length.meters 6, Length.meters 0.5 )


{-| Vertical lift applied to the whole slope assembly so the tilted ramp
floats clear above the rendered floor grid at z = 0. The 10 m-wide slab
tilted 15° reaches ~1.8 m below its own top-face origin, so 2 m of lift
leaves its lowest corner a little above the floor. Purely cosmetic — a
vertical translation doesn't change the friction physics or the drift the
test measures.
-}
slopeLift : Float
slopeLift =
    2


{-| A single wood box resting flat on a wood ramp tilted `slopeAngleDegrees`
about the Y axis. The ramp is a static slab; both it and the box are rotated
by the same angle so the box's bottom face lies flush on the ramp's top face,
contacting near `(0, 0, slopeLift)`. Gravity is vertical; the down-slope
component must be cancelled by friction for the box to stay put. The contact
is box-on-box (convex-convex), so this exercises convex friction on an
incline rather than the plane-convex case.
-}
restingOnSlope : Scenario
restingOnSlope =
    let
        theta =
            degrees slopeAngleDegrees

        angle =
            Angle.degrees slopeAngleDegrees

        -- Slope normal = (sin θ, 0, cos θ). The box center sits half a unit
        -- up that normal so its bottom face rests at the contact point; the
        -- ramp center sits a quarter unit (half its thickness) below the
        -- contact along the normal so its top face meets the box there. The
        -- whole assembly is lifted by `slopeLift` in z.
        box =
            Physics.block unitBlock Material.wood
                |> Physics.rotateAround Axis3d.y angle
                |> Physics.moveTo (Point3d.meters (0.5 * sin theta) 0 (slopeLift + 0.5 * cos theta))

        ramp =
            Physics.static [ ( Shape.block slopeRamp, Material.wood ) ]
                |> Physics.rotateAround Axis3d.y angle
                |> Physics.moveTo (Point3d.meters (-0.25 * sin theta) 0 (slopeLift - 0.25 * cos theta))
    in
    { name = "box resting on a slope"
    , bodies = [ ( 1, box ), ( 0, ramp ) ]
    }


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


{-| Vertical gap left between consecutive boxes in `stackOf5Dropped`, on top
of the 1 m box height. Each box starts this much higher than tight-packed, so
they free-fall and clack down into a settled stack — a visible drop rather
than the imperceptible 1 cm nudge this scenario used to apply.
-}
dropGap : Float
dropGap =
    0.4


{-| Five boxes released above their resting positions with `dropGap` of air
between them, so they fall and stack up. The bottom box drops ~0.4 m, the top
~2 m, giving a clear settling animation. Demo-only; the stability tests use
the perfect-rest `stackOf5` instead.
-}
stackOf5Dropped : Scenario
stackOf5Dropped =
    { name = "stack of 5 boxes (dropped)"
    , bodies =
        let
            n =
                5
        in
        List.indexedMap
            (\i _ ->
                let
                    k =
                        n - i
                in
                ( k
                , Physics.block unitBlock Material.wood
                    |> Physics.moveTo
                        (Point3d.meters 0 0 (toFloat k - 0.5 + toFloat k * dropGap))
                )
            )
            (List.repeat n ())
            ++ [ ground ]
    }


{-| Five wood cylinders placed at their exact resting positions, already touching
cap-on-cap, no drop. The cylinder counterpart to `stackOf5` — score starts near
zero and rises as solver drift accumulates. Each 12-gon cap over-counts before
the cull, so this also exercises that path at rest.
-}
stackOfCylinders : Scenario
stackOfCylinders =
    { name = "stack of 5 cylinders"
    , bodies =
        let
            n =
                5
        in
        List.indexedMap
            (\i _ ->
                ( n - i
                , Physics.cylinder unitCylinder Material.wood
                    |> Physics.moveTo (Point3d.meters 0 0 (toFloat (n - i) - 0.5))
                )
            )
            (List.repeat n ())
            ++ [ ground ]
    }


{-| Five wood cylinders dropped with `dropGap` of air, settling cap-on-cap. Each
12-gon cap over-counts before the cull — a visible exercise of it.
-}
stackOfCylindersDropped : Scenario
stackOfCylindersDropped =
    { name = "stack of 5 cylinders (dropped)"
    , bodies =
        let
            n =
                5
        in
        List.indexedMap
            (\i _ ->
                let
                    k =
                        n - i
                in
                ( k
                , Physics.cylinder unitCylinder Material.wood
                    |> Physics.moveTo
                        (Point3d.meters 0 0 (toFloat k - 0.5 + toFloat k * dropGap))
                )
            )
            (List.repeat n ())
            ++ [ ground ]
    }
