module Internal.Const exposing (contactBreakingThreshold, maxNumber, precision)


maxNumber : Float
maxNumber =
    3.40282347e38


precision : Float
precision =
    1.0e-6


{-| Distance slop for contact generation. A vertex/penetration above the
geometric boundary by less than this still emits a contact, so position-
integration noise around resting bodies doesn't make contacts blink in and
out of existence between frames. Without this the warm-start cache flushes
every time a contact is missing for one frame.

Bullet's analogous global (`gContactBreakingThreshold`) defaults to 0.02 m
which is far too generous for mm-scale bodies; 1 mm is enough to absorb
typical PGS residual position drift while being well under typical body
dimensions, so it doesn't conjure phantom contacts.
-}
contactBreakingThreshold : Float
contactBreakingThreshold =
    1.0e-3
