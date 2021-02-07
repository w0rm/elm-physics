module Common.Camera exposing
    ( Camera
    , camera
    , mouseDirection
    , resize
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4 exposing (Vec4)


type alias Camera =
    { from :
        { x : Float
        , y : Float
        , z : Float
        }
    , to :
        { x : Float
        , y : Float
        , z : Float
        }
    , width : Float
    , height : Float
    , cameraTransform : Mat4
    , perspectiveTransform : Mat4
    }


camera :
    { from : { x : Float, y : Float, z : Float }
    , to : { x : Float, y : Float, z : Float }
    }
    -> Camera
camera { from, to } =
    { from = from
    , to = to
    , width = 1
    , height = 1
    , cameraTransform = Mat4.makeLookAt (Vec3.fromRecord from) (Vec3.fromRecord to) Vec3.k
    , perspectiveTransform = Mat4.identity
    }


resize : Float -> Float -> Camera -> Camera
resize width height camera_ =
    { camera_
        | width = width
        , height = height
        , perspectiveTransform = Mat4.makePerspective 24 (width / height) 5 2000
    }


{-| Converts mouse coordinates into direction within within the world coordinate system.
-}
mouseDirection : Camera -> Float -> Float -> { x : Float, y : Float, z : Float }
mouseDirection { cameraTransform, perspectiveTransform, width, height } x y =
    let
        homogeneousClipCoordinates =
            Vec4.vec4 (x * 2 / width - 1) (1 - y * 2 / height) -1 1

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspectiveTransform)

        vec4CameraCoordinates =
            transform4 invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4
                (Vec4.getX vec4CameraCoordinates)
                (Vec4.getY vec4CameraCoordinates)
                -1
                0

        vec4WorldCoordinates =
            transform4 (Mat4.inverseOrthonormal cameraTransform) direction

        vec3WorldCoordinates =
            Vec3.vec3
                (Vec4.getX vec4WorldCoordinates)
                (Vec4.getY vec4WorldCoordinates)
                (Vec4.getZ vec4WorldCoordinates)
    in
    vec3WorldCoordinates
        |> Vec3.normalize
        |> Vec3.toRecord


transform4 : Mat4 -> Vec4 -> Vec4
transform4 mat v =
    let
        r =
            Mat4.toRecord mat
    in
    Vec4.vec4
        (Vec4.dot (Vec4.vec4 r.m11 r.m12 r.m13 r.m14) v)
        (Vec4.dot (Vec4.vec4 r.m21 r.m22 r.m23 r.m24) v)
        (Vec4.dot (Vec4.vec4 r.m31 r.m32 r.m33 r.m34) v)
        (Vec4.dot (Vec4.vec4 r.m41 r.m42 r.m43 r.m44) v)
