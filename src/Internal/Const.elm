module Internal.Const exposing (contactBreakingThreshold, maxNumber, precision)


maxNumber : Float
maxNumber =
    3.40282347e38


precision : Float
precision =
    1.0e-6


{-| Contact slop: a point this far past the geometric boundary still emits, so
resting bodies don't blink contacts (and flush warm-start) under position drift.
-}
contactBreakingThreshold : Float
contactBreakingThreshold =
    1.0e-3
