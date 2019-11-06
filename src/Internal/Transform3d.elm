module Internal.Transform3d exposing
    ( Transform3d
    , atOrigin
    , atPoint
    , directionPlaceIn
    , directionRelativeTo
    , inverse
    , moveTo
    , normalize
    , orientation
    , originPoint
    , placeIn
    , pointPlaceIn
    , pointRelativeTo
    , relativeTo
    , rotateAroundOwn
    , rotateBy
    , translateBy
    )

import Internal.Matrix3 exposing (Mat3)
import Internal.Vector3 as Vec3 exposing (Vec3)


type Transform3d coordinates defines
    = Transform3d Vec3 Orientation3d


atOrigin : Transform3d coordinates defines
atOrigin =
    Transform3d Vec3.zero identity


atPoint : Vec3 -> Transform3d coordinates defines
atPoint point =
    Transform3d point identity


pointPlaceIn : Transform3d coordinates defines -> Vec3 -> Vec3
pointPlaceIn (Transform3d globalOrigin globalOrientation) localPoint =
    Vec3.add globalOrigin (rotate globalOrientation localPoint)


pointRelativeTo : Transform3d coordinates defines -> Vec3 -> Vec3
pointRelativeTo (Transform3d localOrigin localOrientation) worldPoint =
    derotate localOrientation (Vec3.sub worldPoint localOrigin)


directionRelativeTo : Transform3d coordinates defines -> Vec3 -> Vec3
directionRelativeTo (Transform3d _ localOrientation) worldVector =
    derotate localOrientation worldVector


directionPlaceIn : Transform3d coordinates defines -> Vec3 -> Vec3
directionPlaceIn (Transform3d _ globalOrientation) worldVector =
    rotate globalOrientation worldVector


placeIn :
    Transform3d globalCoordinates { defines : localCoordinates }
    -> Transform3d localCoordinates defines
    -> Transform3d globalCoordinates defines
placeIn (Transform3d globalPosition globalOrientation) (Transform3d localPosition localOrientation) =
    Transform3d
        (Vec3.add globalPosition (rotate globalOrientation localPosition))
        (mul globalOrientation localOrientation)


relativeTo :
    Transform3d globalCoordinates { defines : localCoordinates }
    -> Transform3d globalCoordinates defines
    -> Transform3d localCoordinates defines
relativeTo ((Transform3d _ (Orientation3d x y z w)) as t1) (Transform3d p2 o2) =
    Transform3d
        (pointRelativeTo t1 p2)
        -- Assuming normalized quaternion, we can conjugate it to inverse
        (mul (Orientation3d -x -y -z w) o2)


{-| The same as `Transform.relativeTo Transform.atOrigin`
-}
inverse :
    Transform3d globalCoordinates { defines : localCoordinates }
    -> Transform3d localCoordinates { defines : globalCoordinates }
inverse ((Transform3d _ (Orientation3d x y z w)) as transform3d) =
    Transform3d
        (pointRelativeTo transform3d Vec3.zero)
        -- Assuming normalized quaternion, we can conjugate it to inverse
        (Orientation3d -x -y -z w)


normalize : Transform3d coordinates defines -> Transform3d coordinates defines
normalize (Transform3d localOrigin (Orientation3d x y z w)) =
    let
        len =
            sqrt (x * x + y * y + z * z + w * w)
    in
    Transform3d localOrigin (Orientation3d (x / len) (y / len) (z / len) (w / len))


originPoint : Transform3d coordinates defines -> Vec3
originPoint (Transform3d localOrigin _) =
    localOrigin


orientation : Transform3d coordinates defines -> Mat3
orientation (Transform3d _ (Orientation3d x y z w)) =
    { m11 = 1 - 2 * y * y - 2 * z * z
    , m12 = 2 * x * y - 2 * w * z
    , m13 = 2 * x * z + 2 * w * y
    , m21 = 2 * x * y + 2 * w * z
    , m22 = 1 - 2 * x * x - 2 * z * z
    , m23 = 2 * y * z - 2 * w * x
    , m31 = 2 * x * z - 2 * w * y
    , m32 = 2 * y * z + 2 * w * x
    , m33 = 1 - 2 * x * x - 2 * y * y
    }


moveTo :
    Vec3
    -> Transform3d coordinates defines1
    -> Transform3d coordinates defines2
moveTo newOrigin (Transform3d _ localOrientation) =
    Transform3d newOrigin localOrientation


translateBy : Vec3 -> Transform3d coordinates defines1 -> Transform3d coordinates defines2
translateBy vector (Transform3d localOrigin localOrientation) =
    Transform3d (Vec3.add vector localOrigin) localOrientation


rotateAroundOwn : Vec3 -> Float -> Transform3d coordinates defines1 -> Transform3d coordinates defines2
rotateAroundOwn axis angle (Transform3d localOrigin localOrientation) =
    Transform3d localOrigin (mul localOrientation (fromAngleAxis angle axis))


{-| angularDistance = angularVelocity x dt
-}
rotateBy : Vec3 -> Transform3d coordinates defines1 -> Transform3d coordinates defines2
rotateBy { x, y, z } (Transform3d localOrigin (Orientation3d qx qy qz qw)) =
    Transform3d localOrigin
        (Orientation3d
            (qx + (x * qw + y * qz - z * qy) * 0.5)
            (qy + (y * qw + z * qx - x * qz) * 0.5)
            (qz + (z * qw + x * qy - y * qx) * 0.5)
            (qw + (-x * qx - y * qy - z * qz) * 0.5)
        )


type Orientation3d
    = Orientation3d Float Float Float Float


identity : Orientation3d
identity =
    Orientation3d 0 0 0 1


fromAngleAxis : Float -> Vec3 -> Orientation3d
fromAngleAxis angle axis =
    let
        { x, y, z } =
            Vec3.normalize axis

        theta =
            angle * 0.5

        c =
            cos theta

        s =
            sin theta
    in
    Orientation3d (x * s) (y * s) (z * s) c


mul : Orientation3d -> Orientation3d -> Orientation3d
mul (Orientation3d q1x q1y q1z q1w) (Orientation3d q2x q2y q2z q2w) =
    Orientation3d
        (q1x * q2w + q1y * q2z - q1z * q2y + q1w * q2x)
        (-q1x * q2z + q1y * q2w + q1z * q2x + q1w * q2y)
        (q1x * q2y - q1y * q2x + q1z * q2w + q1w * q2z)
        (-q1x * q2x - q1y * q2y - q1z * q2z + q1w * q2w)


rotate : Orientation3d -> Vec3 -> Vec3
rotate (Orientation3d qx qy qz qw) { x, y, z } =
    let
        ix =
            qw * x + qy * z - qz * y

        iy =
            qw * y + qz * x - qx * z

        iz =
            qw * z + qx * y - qy * x

        iw =
            -qx * x - qy * y - qz * z
    in
    { x = ix * qw + iw * -qx + iy * -qz - iz * -qy
    , y = iy * qw + iw * -qy + iz * -qx - ix * -qz
    , z = iz * qw + iw * -qz + ix * -qy - iy * -qx
    }


derotate : Orientation3d -> Vec3 -> Vec3
derotate (Orientation3d qx qy qz qw) { x, y, z } =
    let
        ix =
            -qw * x + qy * z - qz * y

        iy =
            -qw * y + qz * x - qx * z

        iz =
            -qw * z + qx * y - qy * x

        iw =
            -qx * x - qy * y - qz * z
    in
    { x = ix * -qw + iw * -qx + iy * -qz - iz * -qy
    , y = iy * -qw + iw * -qy + iz * -qx - ix * -qz
    , z = iz * -qw + iw * -qz + ix * -qy - iy * -qx
    }
