module Transform3dFromFrame3dTest exposing (conversion)

import Angle
import Axis3d
import Direction3d
import Expect exposing (Expectation)
import Frame3d exposing (Frame3d)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 exposing (Vec3)
import Length exposing (Meters)
import Point3d
import Test exposing (Test, describe, test)


toFrame3d : Transform3d coords defines -> Frame3d Meters coords defines
toFrame3d transform3d =
    let
        t =
            Transform3d.orientation transform3d
    in
    Frame3d.unsafe
        { originPoint = Point3d.unsafe (Transform3d.originPoint transform3d)
        , xDirection = Direction3d.unsafe { x = t.m11, y = t.m21, z = t.m31 }
        , yDirection = Direction3d.unsafe { x = t.m12, y = t.m22, z = t.m32 }
        , zDirection = Direction3d.unsafe { x = t.m13, y = t.m23, z = t.m33 }
        }


fromFrame3d : Frame3d Meters coords defines -> Transform3d coords defines
fromFrame3d frame3d =
    let
        origin =
            Point3d.unwrap (Frame3d.originPoint frame3d)

        x =
            Direction3d.unwrap (Frame3d.xDirection frame3d)

        y =
            Direction3d.unwrap (Frame3d.yDirection frame3d)

        z =
            Direction3d.unwrap (Frame3d.zDirection frame3d)
    in
    Transform3d.fromOriginAndBasis origin x y z


conversion : Test
conversion =
    let
        -- note this has to be right handed frame, use
        -- Frame3d.isRightHanded to check and reverse one of the axis
        frame3d =
            Frame3d.atPoint (Point3d.meters 0.5 0.6 0.7)
                |> Frame3d.rotateAround Axis3d.x (Angle.radians (pi / 5))
                |> Frame3d.rotateAround Axis3d.y (Angle.radians (pi / 5))

        point3d =
            Point3d.meters 0.234 0.234 0.234
    in
    describe "Transform3d.fromOriginAndBasis"
        [ test "creates the correct frame3d for 180 rotation" <|
            \_ ->
                frame3d
                    |> fromFrame3d
                    |> toFrame3d
                    |> expectFrame3d frame3d
        , test "transforms the point exactly the same for 180 rotation" <|
            \_ ->
                expectVec3
                    (Point3d.unwrap (Point3d.placeIn frame3d point3d))
                    (Transform3d.pointPlaceIn (frame3d |> fromFrame3d) (Point3d.toMeters point3d))
        ]


expectFrame3d : Frame3d Meters coords define -> Frame3d Meters coords define -> Expectation
expectFrame3d frame3d =
    let
        origin =
            Point3d.unwrap (Frame3d.originPoint frame3d)

        x =
            Direction3d.unwrap (Frame3d.xDirection frame3d)

        y =
            Direction3d.unwrap (Frame3d.yDirection frame3d)

        z =
            Direction3d.unwrap (Frame3d.zDirection frame3d)
    in
    Expect.all
        [ Frame3d.originPoint >> Point3d.unwrap >> expectVec3 origin
        , Frame3d.xDirection >> Direction3d.unwrap >> expectVec3 x
        , Frame3d.yDirection >> Direction3d.unwrap >> expectVec3 y
        , Frame3d.zDirection >> Direction3d.unwrap >> expectVec3 z
        ]


expectVec3 : Vec3 -> Vec3 -> Expectation
expectVec3 vec3 =
    Expect.all
        [ .x >> Expect.within (Expect.Absolute 0.00001) vec3.x
        , .y >> Expect.within (Expect.Absolute 0.00001) vec3.y
        , .z >> Expect.within (Expect.Absolute 0.00001) vec3.z
        ]
