module Shapes.Capsule exposing
    ( Capsule
    , atOrigin
    , expandBoundingSphereRadius
    , placeIn
    , raycast
    )

import Internal.Matrix3 as Mat3 exposing (Mat3)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias Capsule =
    { radius : Float
    , halfLength : Float
    , axis : Vec3 -- unit vector along the capsule's long axis (world space)
    , position : Vec3 -- center of the capsule (world space)
    , volume : Float
    , inertia : Mat3
    }


{-| A capsule aligned along the z-axis, centered at the origin.
-}
atOrigin : Float -> Float -> Capsule
atOrigin radius halfLength =
    let
        volume =
            pi * radius * radius * (2 * halfLength + 4 / 3 * radius)
    in
    { radius = radius
    , halfLength = halfLength
    , axis = Vec3.zAxis
    , position = Vec3.zero
    , volume = volume
    , inertia = Mat3.capsuleInertia volume radius halfLength
    }


placeIn : Transform3d coordinates defines -> Capsule -> Capsule
placeIn transform3d { radius, halfLength, axis, position, volume, inertia } =
    { radius = radius
    , halfLength = halfLength
    , axis = Transform3d.directionPlaceIn transform3d axis
    , position = Transform3d.pointPlaceIn transform3d position
    , volume = volume
    , inertia = Transform3d.inertiaRotateIn transform3d inertia
    }


expandBoundingSphereRadius : Capsule -> Float -> Float
expandBoundingSphereRadius { radius, halfLength, position } boundingSphereRadius =
    max (Vec3.length position + halfLength + radius) boundingSphereRadius


{-| Intersect a ray with a capsule. Returned normal is NOT normalized;
Physics.elm normalizes it via Direction3d.unsafe (matches Sphere/Convex.raycast).
-}
raycast : { from : Vec3, direction : Vec3 } -> Capsule -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { from, direction } { radius, halfLength, axis, position } =
    let
        -- Ray origin relative to the capsule center
        p =
            Vec3.sub from position

        pDotAxis =
            Vec3.dot p axis

        dDotAxis =
            Vec3.dot direction axis

        -- Perpendicular components (reject axis projection)
        pPerpX =
            p.x - pDotAxis * axis.x

        pPerpY =
            p.y - pDotAxis * axis.y

        pPerpZ =
            p.z - pDotAxis * axis.z

        dPerpX =
            direction.x - dDotAxis * axis.x

        dPerpY =
            direction.y - dDotAxis * axis.y

        dPerpZ =
            direction.z - dDotAxis * axis.z

        -- Sphere cap helper: returns the first non-negative t, if any.
        -- epOffset is the offset from capsule center to cap center along axis.
        sphereCapT epOffset =
            let
                -- Ray origin relative to this sphere cap center
                qx =
                    p.x - epOffset * axis.x

                qy =
                    p.y - epOffset * axis.y

                qz =
                    p.z - epOffset * axis.z

                b =
                    2 * (direction.x * qx + direction.y * qy + direction.z * qz)

                c =
                    qx * qx + qy * qy + qz * qz - radius * radius

                delta =
                    b * b - 4 * c
            in
            if delta < 0 then
                Nothing

            else
                let
                    sqrtDelta =
                        sqrt delta

                    t1 =
                        (-b - sqrtDelta) / 2
                in
                if t1 >= 0 then
                    Just t1

                else
                    let
                        t2 =
                            (-b + sqrtDelta) / 2
                    in
                    if t2 >= 0 then
                        Just t2

                    else
                        Nothing

        -- Cylinder body intersection (restricted to |axial| <= halfLength)
        cylAv =
            dPerpX * dPerpX + dPerpY * dPerpY + dPerpZ * dPerpZ

        cylBv =
            2 * (pPerpX * dPerpX + pPerpY * dPerpY + pPerpZ * dPerpZ)

        cylCv =
            pPerpX * pPerpX + pPerpY * pPerpY + pPerpZ * pPerpZ - radius * radius

        cylinderT =
            if cylAv < 1.0e-10 then
                -- Ray is parallel to the capsule axis — no cylinder-body hit
                Nothing

            else
                let
                    delta =
                        cylBv * cylBv - 4 * cylAv * cylCv
                in
                if delta < 0 then
                    Nothing

                else
                    let
                        sqrtDelta =
                            sqrt delta

                        t =
                            (-cylBv - sqrtDelta) / (2 * cylAv)

                        tValid =
                            if t >= 0 then
                                t

                            else
                                (-cylBv + sqrtDelta) / (2 * cylAv)
                    in
                    if tValid < 0 then
                        Nothing

                    else
                        let
                            axialCoord =
                                pDotAxis + tValid * dDotAxis
                        in
                        if axialCoord < -halfLength || axialCoord > halfLength then
                            Nothing

                        else
                            Just tValid

        tCap1 =
            sphereCapT halfLength

        tCap2 =
            sphereCapT -halfLength

        bestT =
            minPositive (minPositive tCap1 tCap2) cylinderT
    in
    case bestT of
        Nothing ->
            Nothing

        Just t ->
            let
                hitPoint =
                    { x = from.x + direction.x * t
                    , y = from.y + direction.y * t
                    , z = from.z + direction.z * t
                    }

                -- Axial coordinate of the hit point relative to capsule center
                axialCoord =
                    pDotAxis + t * dDotAxis

                clampedAxial =
                    if axialCoord > halfLength then
                        halfLength

                    else if axialCoord < -halfLength then
                        -halfLength

                    else
                        axialCoord

                -- Closest point on the capsule axis to the hit point
                closestOnAxisX =
                    position.x + clampedAxial * axis.x

                closestOnAxisY =
                    position.y + clampedAxial * axis.y

                closestOnAxisZ =
                    position.z + clampedAxial * axis.z
            in
            Just
                { distance = t
                , point = hitPoint

                -- Not normalized: Physics.elm normalises via Direction3d.unsafe
                , normal =
                    { x = hitPoint.x - closestOnAxisX
                    , y = hitPoint.y - closestOnAxisY
                    , z = hitPoint.z - closestOnAxisZ
                    }
                }


minPositive : Maybe Float -> Maybe Float -> Maybe Float
minPositive a b =
    case ( a, b ) of
        ( Nothing, _ ) ->
            b

        ( _, Nothing ) ->
            a

        ( Just ta, Just tb ) ->
            if ta <= tb then
                a

            else
                b
