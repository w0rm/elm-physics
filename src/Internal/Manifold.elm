module Internal.Manifold exposing (reduce)

{-| Cull candidate contact points within the plane margin down to a bounded
4-point manifold.

@docs reduce

-}

import Internal.Const as Const
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Farthest-pair tie tolerance, for cylinder caps only: a regular polygon's equal
diameters would otherwise flip the pair under jitter. Other faces are unaffected.
-}
tieEpsilon : Float
tieEpsilon =
    1.0e-8


{-| Drop points outside the plane's contact margin, then keep the 4 most-spread
(farthest pair, then the two farthest from it); 4 or fewer pass through.
-}
reduce : Vec3 -> Float -> List ( tag, Vec3 ) -> List ( tag, Vec3 )
reduce normal planeConstant points =
    case withinMargin normal (planeConstant - Const.contactBreakingThreshold) points [] of
        (first :: _ :: _ :: _ :: _ :: _) as candidates ->
            farthestPair candidates candidates first first -1

        candidates ->
            candidates


{-| Points within the plane margin. Tail-recursive; output order reversed, fine.
-}
withinMargin : Vec3 -> Float -> List ( tag, Vec3 ) -> List ( tag, Vec3 ) -> List ( tag, Vec3 )
withinMargin normal marginConstant points acc =
    case points of
        (( _, p ) as v) :: rest ->
            if Vec3.dot normal p + marginConstant < 0 then
                withinMargin normal marginConstant rest (v :: acc)

            else
                withinMargin normal marginConstant rest acc

        [] ->
            acc


{-| Scan for the farthest-apart pair (ties broken by `tieEpsilon`), then add the
two farthest from it.
-}
farthestPair : List ( tag, Vec3 ) -> List ( tag, Vec3 ) -> ( tag, Vec3 ) -> ( tag, Vec3 ) -> Float -> List ( tag, Vec3 )
farthestPair outer all bestA bestB bestD =
    case outer of
        (( _, pp ) as p) :: rest ->
            let
                (( _, pq ) as q) =
                    farthestFrom p all

                d =
                    Vec3.distanceSquared pp pq
            in
            if d - bestD - tieEpsilon > 0 then
                farthestPair rest all p q d

            else
                farthestPair rest all bestA bestB bestD

        [] ->
            let
                c =
                    farthestFrom2 bestA bestB all

                e =
                    farthestFrom3 bestA bestB c all
            in
            [ bestA, bestB, c, e ]


{-| The candidate farthest from point `a`.
-}
farthestFrom : ( tag, Vec3 ) -> List ( tag, Vec3 ) -> ( tag, Vec3 )
farthestFrom (( _, pa ) as a) candidates =
    case candidates of
        (( _, ph ) as head) :: tail ->
            farthestFromHelp pa tail head (Vec3.distanceSquared pa ph)

        [] ->
            a


farthestFromHelp : Vec3 -> List ( tag, Vec3 ) -> ( tag, Vec3 ) -> Float -> ( tag, Vec3 )
farthestFromHelp pa remaining best bestScore =
    case remaining of
        (( _, pq ) as q) :: rest ->
            let
                s =
                    Vec3.distanceSquared pa pq
            in
            if s - bestScore > 0 then
                farthestFromHelp pa rest q s

            else
                farthestFromHelp pa rest best bestScore

        [] ->
            best


{-| The candidate farthest from the pair `{a, b}`.
-}
farthestFrom2 : ( tag, Vec3 ) -> ( tag, Vec3 ) -> List ( tag, Vec3 ) -> ( tag, Vec3 )
farthestFrom2 (( _, pa ) as a) ( _, pb ) candidates =
    case candidates of
        (( _, ph ) as head) :: tail ->
            farthestFrom2Help pa pb tail head (min (Vec3.distanceSquared pa ph) (Vec3.distanceSquared pb ph))

        [] ->
            a


farthestFrom2Help : Vec3 -> Vec3 -> List ( tag, Vec3 ) -> ( tag, Vec3 ) -> Float -> ( tag, Vec3 )
farthestFrom2Help pa pb remaining best bestScore =
    case remaining of
        (( _, pq ) as q) :: rest ->
            let
                s =
                    min (Vec3.distanceSquared pa pq) (Vec3.distanceSquared pb pq)
            in
            if s - bestScore > 0 then
                farthestFrom2Help pa pb rest q s

            else
                farthestFrom2Help pa pb rest best bestScore

        [] ->
            best


{-| The candidate farthest from the set `{a, b, c}`.
-}
farthestFrom3 : ( tag, Vec3 ) -> ( tag, Vec3 ) -> ( tag, Vec3 ) -> List ( tag, Vec3 ) -> ( tag, Vec3 )
farthestFrom3 (( _, pa ) as a) ( _, pb ) ( _, pc ) candidates =
    case candidates of
        (( _, ph ) as head) :: tail ->
            farthestFrom3Help pa pb pc tail head (min (Vec3.distanceSquared pa ph) (min (Vec3.distanceSquared pb ph) (Vec3.distanceSquared pc ph)))

        [] ->
            a


farthestFrom3Help : Vec3 -> Vec3 -> Vec3 -> List ( tag, Vec3 ) -> ( tag, Vec3 ) -> Float -> ( tag, Vec3 )
farthestFrom3Help pa pb pc remaining best bestScore =
    case remaining of
        (( _, pq ) as q) :: rest ->
            let
                s =
                    min (Vec3.distanceSquared pa pq) (min (Vec3.distanceSquared pb pq) (Vec3.distanceSquared pc pq))
            in
            if s - bestScore > 0 then
                farthestFrom3Help pa pb pc rest q s

            else
                farthestFrom3Help pa pb pc rest best bestScore

        [] ->
            best
