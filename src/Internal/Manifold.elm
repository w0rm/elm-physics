module Internal.Manifold exposing (reduce)

{-| Cull candidate contact points within the plane margin down to a bounded
4-point manifold.

@docs reduce

-}

import Internal.Const as Const
import Internal.Vector3 as Vec3 exposing (Vec3)


{-| Relative tie window for the spread scores. Regular polygons (cylinder
caps) have several exactly-equal maximally-spread 4-subsets, and float jitter
would flip the selection between them every few frames — churning the
manifold's feature keys and losing warm start. Scores within the window count
as tied and resolve to the smallest vertex id, which is stable across frames
as long as the candidates are. Well above frame-to-frame score jitter, well
below genuine geometric distinctions.
-}
tieRatio : Float
tieRatio =
    1.0e-3


{-| `(score, id)` beats `(bestScore, bestId)`: strictly better score, or tied
within the window with a smaller id.
-}
better : Float -> Int -> Float -> Int -> Bool
better score id bestScore bestId =
    (score - bestScore - tieRatio * bestScore > 0)
        || ((bestScore - score - tieRatio * bestScore <= 0) && (id - bestId < 0))


{-| Drop points outside the plane's contact margin, then keep the 4 most-spread
(farthest pair, then the two farthest from it); 4 or fewer pass through.
-}
reduce : Vec3 -> Float -> List ( Int, Vec3 ) -> List ( Int, Vec3 )
reduce normal planeConstant points =
    case withinMargin normal (planeConstant - Const.contactBreakingThreshold) points [] of
        (first :: _ :: _ :: _ :: _ :: _) as candidates ->
            farthestPair candidates candidates first first -1

        candidates ->
            candidates


{-| Points within the plane margin. Tail-recursive; output order reversed, fine.
-}
withinMargin : Vec3 -> Float -> List ( Int, Vec3 ) -> List ( Int, Vec3 ) -> List ( Int, Vec3 )
withinMargin normal marginConstant points acc =
    case points of
        (( _, p ) as v) :: rest ->
            if Vec3.dot normal p + marginConstant < 0 then
                withinMargin normal marginConstant rest (v :: acc)

            else
                withinMargin normal marginConstant rest acc

        [] ->
            acc


{-| Scan for the farthest-apart pair, then add the two farthest from it.
-}
farthestPair : List ( Int, Vec3 ) -> List ( Int, Vec3 ) -> ( Int, Vec3 ) -> ( Int, Vec3 ) -> Float -> List ( Int, Vec3 )
farthestPair outer all bestA bestB bestD =
    case outer of
        (( pid, pp ) as p) :: rest ->
            let
                (( _, pq ) as q) =
                    farthestFrom p all

                d =
                    Vec3.distanceSquared pp pq
            in
            if better d pid bestD (Tuple.first bestA) then
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
farthestFrom : ( Int, Vec3 ) -> List ( Int, Vec3 ) -> ( Int, Vec3 )
farthestFrom (( _, pa ) as a) candidates =
    case candidates of
        (( _, ph ) as head) :: tail ->
            farthestFromHelp pa tail head (Vec3.distanceSquared pa ph)

        [] ->
            a


farthestFromHelp : Vec3 -> List ( Int, Vec3 ) -> ( Int, Vec3 ) -> Float -> ( Int, Vec3 )
farthestFromHelp pa remaining best bestScore =
    case remaining of
        (( qid, pq ) as q) :: rest ->
            let
                s =
                    Vec3.distanceSquared pa pq
            in
            if better s qid bestScore (Tuple.first best) then
                farthestFromHelp pa rest q s

            else
                farthestFromHelp pa rest best bestScore

        [] ->
            best


{-| The candidate farthest from the pair `{a, b}`.
-}
farthestFrom2 : ( Int, Vec3 ) -> ( Int, Vec3 ) -> List ( Int, Vec3 ) -> ( Int, Vec3 )
farthestFrom2 (( _, pa ) as a) ( _, pb ) candidates =
    case candidates of
        (( _, ph ) as head) :: tail ->
            farthestFrom2Help pa pb tail head (min (Vec3.distanceSquared pa ph) (Vec3.distanceSquared pb ph))

        [] ->
            a


farthestFrom2Help : Vec3 -> Vec3 -> List ( Int, Vec3 ) -> ( Int, Vec3 ) -> Float -> ( Int, Vec3 )
farthestFrom2Help pa pb remaining best bestScore =
    case remaining of
        (( qid, pq ) as q) :: rest ->
            let
                s =
                    min (Vec3.distanceSquared pa pq) (Vec3.distanceSquared pb pq)
            in
            if better s qid bestScore (Tuple.first best) then
                farthestFrom2Help pa pb rest q s

            else
                farthestFrom2Help pa pb rest best bestScore

        [] ->
            best


{-| The candidate farthest from the set `{a, b, c}`.
-}
farthestFrom3 : ( Int, Vec3 ) -> ( Int, Vec3 ) -> ( Int, Vec3 ) -> List ( Int, Vec3 ) -> ( Int, Vec3 )
farthestFrom3 (( _, pa ) as a) ( _, pb ) ( _, pc ) candidates =
    case candidates of
        (( _, ph ) as head) :: tail ->
            farthestFrom3Help pa pb pc tail head (min (Vec3.distanceSquared pa ph) (min (Vec3.distanceSquared pb ph) (Vec3.distanceSquared pc ph)))

        [] ->
            a


farthestFrom3Help : Vec3 -> Vec3 -> Vec3 -> List ( Int, Vec3 ) -> ( Int, Vec3 ) -> Float -> ( Int, Vec3 )
farthestFrom3Help pa pb pc remaining best bestScore =
    case remaining of
        (( qid, pq ) as q) :: rest ->
            let
                s =
                    min (Vec3.distanceSquared pa pq) (min (Vec3.distanceSquared pb pq) (Vec3.distanceSquared pc pq))
            in
            if better s qid bestScore (Tuple.first best) then
                farthestFrom3Help pa pb pc rest q s

            else
                farthestFrom3Help pa pb pc rest best bestScore

        [] ->
            best
