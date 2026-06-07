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


{-| Raycast treats the ray as parallel to the capsule axis when the squared
perpendicular component of its direction falls below this.
-}
rayParallelTolerance : Float
rayParallelTolerance =
    1.0e-10


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


raycast : { from : Vec3, direction : Vec3 } -> Capsule -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { from, direction } { radius, halfLength, axis, position } =
    let
        p =
            Vec3.sub from position

        pDotAxis =
            Vec3.dot p axis

        dDotAxis =
            Vec3.dot direction axis

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

        -- The capsule is contained within the infinite cylinder around its
        -- axis line, so a ray that misses this cylinder also misses the capsule.
        cylA =
            dPerpX * dPerpX + dPerpY * dPerpY + dPerpZ * dPerpZ

        cylB =
            2 * (pPerpX * dPerpX + pPerpY * dPerpY + pPerpZ * dPerpZ)

        cylC =
            pPerpX * pPerpX + pPerpY * pPerpY + pPerpZ * pPerpZ - radius * radius

        capEntry capOffset =
            let
                qx =
                    p.x - capOffset * axis.x

                qy =
                    p.y - capOffset * axis.y

                qz =
                    p.z - capOffset * axis.z

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
                    t =
                        (-b - sqrt delta) / 2
                in
                if t < 0 then
                    Nothing

                else
                    Just t

        maybeT =
            if cylA - rayParallelTolerance < 0 then
                if cylC > 0 then
                    Nothing

                else if dDotAxis < 0 then
                    capEntry halfLength

                else
                    capEntry -halfLength

            else
                let
                    delta =
                        cylB * cylB - 4 * cylA * cylC
                in
                if delta < 0 then
                    Nothing

                else
                    let
                        t =
                            (-cylB - sqrt delta) / (2 * cylA)
                    in
                    if t < 0 then
                        Nothing

                    else
                        let
                            axial =
                                pDotAxis + t * dDotAxis
                        in
                        if axial - halfLength > 0 then
                            capEntry halfLength

                        else if axial + halfLength < 0 then
                            capEntry -halfLength

                        else
                            Just t
    in
    case maybeT of
        Nothing ->
            Nothing

        Just t ->
            let
                hitPoint =
                    { x = from.x + direction.x * t
                    , y = from.y + direction.y * t
                    , z = from.z + direction.z * t
                    }

                clampedAxial =
                    clamp -halfLength halfLength (pDotAxis + t * dDotAxis)
            in
            Just
                { distance = t
                , point = hitPoint
                , normal =
                    { x = hitPoint.x - position.x - clampedAxial * axis.x
                    , y = hitPoint.y - position.y - clampedAxial * axis.y
                    , z = hitPoint.z - position.z - clampedAxial * axis.z
                    }
                }
