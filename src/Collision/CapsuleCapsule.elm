module Collision.CapsuleCapsule exposing (addContacts)

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.ContactId as ContactId
import Internal.Vector3 as Vec3 exposing (Vec3)
import Shapes.Capsule exposing (Capsule)


addContacts : Int -> Capsule -> Capsule -> List Contact -> List Contact
addContacts shapeKey capsule1 capsule2 contacts =
    let
        ( pt1, pt2 ) =
            closestSegmentPoints capsule1 capsule2

        distance =
            Vec3.distance pt2 pt1 - capsule1.radius - capsule2.radius

        normal =
            Vec3.direction pt2 pt1
    in
    if distance > 0 then
        contacts

    else
        { shapeKey = shapeKey
        , featureKey = ContactId.simple
        , ni = normal
        , pi = Vec3.add pt1 (Vec3.scale capsule1.radius normal)
        , pj = Vec3.sub pt2 (Vec3.scale capsule2.radius normal)
        }
            :: contacts


closestSegmentPoints : Capsule -> Capsule -> ( Vec3, Vec3 )
closestSegmentPoints capsule1 capsule2 =
    let
        p1 =
            capsule1.position

        p2 =
            capsule2.position

        d1 =
            capsule1.axis

        d2 =
            capsule2.axis

        h1 =
            capsule1.halfLength

        h2 =
            capsule2.halfLength

        r =
            Vec3.sub p1 p2

        b =
            Vec3.dot d1 d2

        c =
            Vec3.dot d1 r

        f =
            Vec3.dot d2 r

        -- 1 - cos²θ = sin²θ between the two axes
        denom =
            1 - b * b
    in
    if denom - Const.parallelTolerance < 0 then
        let
            t =
                max -h2 (min h2 f)

            s =
                max -h1 (min h1 (b * t - c))
        in
        ( Vec3.add p1 (Vec3.scale s d1)
        , Vec3.add p2 (Vec3.scale t d2)
        )

    else
        let
            s0 =
                max -h1 (min h1 ((b * f - c) / denom))

            t0 =
                max -h2 (min h2 (b * s0 + f))

            s =
                max -h1 (min h1 (b * t0 - c))
        in
        ( Vec3.add p1 (Vec3.scale s d1)
        , Vec3.add p2 (Vec3.scale t0 d2)
        )
