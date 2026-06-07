module Internal.Const exposing (contactBreakingThreshold, maxNumber, parallelTolerance, precision, solverTolerance)


maxNumber : Float
maxNumber =
    3.40282347e38


{-| Geometric length/zero tolerance: a vector, cross product, or distance below
this is treated as degenerate.
-}
precision : Float
precision =
    1.0e-6


{-| Velocity-solver early-out: once a sweep's total impulse change drops below
this, further iterations won't move bodies meaningfully, so stop.
-}
solverTolerance : Float
solverTolerance =
    1.0e-6


{-| Contact slop: a point this far past the geometric boundary still emits, so
resting bodies don't blink contacts (and flush warm-start) under position drift.
-}
contactBreakingThreshold : Float
contactBreakingThreshold =
    1.0e-3


{-| Two unit directions count as parallel when `sin²θ` between them is below this,
i.e. θ ≲ 0.057° — above 6-decimal rounding noise, below any genuine angle.
-}
parallelTolerance : Float
parallelTolerance =
    1.0e-6
