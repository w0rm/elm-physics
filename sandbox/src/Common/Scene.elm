module Common.Scene exposing (view)

import Common.Camera exposing (Camera)
import Common.Math as Math
import Common.Meshes as Meshes exposing (Attributes)
import Common.Settings exposing (Settings)
import Common.Shaders as Shaders
import Direction3d exposing (Direction3d)
import Frame3d
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes as Attributes
import Internal.Body as InternalBody
import Internal.Shape exposing (CenterOfMassCoordinates)
import Internal.Transform3d as Transform3d
import Length exposing (Meters)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics exposing (Body, BodyCoordinates, WorldCoordinates)
import Physics.Types
import Point3d exposing (Point3d)
import WebGL exposing (Entity, Mesh)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend
import WebGL.Settings.DepthTest


type alias Params =
    { settings : Settings
    , bodies : List ( Mesh Attributes, Body )
    , contacts : List (Point3d Meters WorldCoordinates)
    , camera : Camera
    , floorOffset :
        { x : Float
        , y : Float
        , z : Float
        }
    }


view : Params -> Html msg
view { settings, bodies, contacts, floorOffset, camera } =
    let
        lightDirection =
            Vec3.normalize (Vec3.vec3 -1 -1 -1)

        sceneParams =
            { lightDirection = lightDirection
            , camera = camera
            , debugWireframes = settings.debugWireframes
            , debugCenterOfMass = settings.debugCenterOfMass
            , shadow =
                Math.makeShadow
                    (Vec3.fromRecord floorOffset)
                    Vec3.k
                    lightDirection
            }
    in
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.3 0.3 0.3 1
        ]
        [ Attributes.width (round camera.width)
        , Attributes.height (round camera.height)
        , Attributes.style "position" "absolute"
        , Attributes.style "top" "0"
        , Attributes.style "left" "0"
        ]
        ([ ( True
           , \entities -> List.foldl (addBodyEntities sceneParams) entities bodies
           )
         , ( settings.debugContacts
           , \entities -> List.foldl (addContactIndicator sceneParams) entities contacts
           )
         ]
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.foldl (<|) []
        )


type alias SceneParams =
    { lightDirection : Vec3
    , camera : Camera
    , debugWireframes : Bool
    , debugCenterOfMass : Bool
    , shadow : Mat4
    }


addBodyEntities : SceneParams -> ( Mesh Attributes, Body ) -> List Entity -> List Entity
addBodyEntities ({ lightDirection, shadow, camera, debugWireframes, debugCenterOfMass } as sceneParams) ( mesh, body ) entities =
    let
        frame =
            Physics.frame body

        transform =
            Frame3d.toMat4 frame

        color =
            Vec3.vec3 0.9 0.9 0.9

        addCenterOfMass acc =
            if debugCenterOfMass then
                case Physics.centerOfMass body of
                    Just com ->
                        acc
                            |> addContactIndicator sceneParams com
                            |> addEigenvectorAxes sceneParams body

                    Nothing ->
                        acc

            else
                acc
    in
    entities
        |> addCenterOfMass
        |> (if debugWireframes then
                (::)
                    (WebGL.entityWith defaultSettings
                        Shaders.wireframeVertex
                        Shaders.wireframeFragment
                        mesh
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = color
                        , lightDirection = lightDirection
                        , transform = transform
                        }
                    )

            else
                (::)
                    (WebGL.entityWith defaultSettings
                        Shaders.vertex
                        Shaders.fragment
                        mesh
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = color
                        , lightDirection = lightDirection
                        , transform = transform
                        }
                    )
           )
        |> (if debugWireframes then
                identity

            else
                (::)
                    (WebGL.entityWith defaultSettings
                        Shaders.vertex
                        Shaders.shadowFragment
                        mesh
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = Vec3.vec3 0.25 0.25 0.25
                        , lightDirection = lightDirection
                        , transform = Mat4.mul shadow transform
                        }
                    )
           )


{-| Render a collision point for the purpose of debugging
-}
addContactIndicator : SceneParams -> Point3d Meters WorldCoordinates -> List Entity -> List Entity
addContactIndicator { lightDirection, camera } point tail =
    WebGL.entityWith defaultSettings
        Shaders.vertex
        Shaders.fragment
        Meshes.contact
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , color = Vec3.vec3 1 0 0
        , lightDirection = lightDirection
        , transform = Frame3d.toMat4 (Frame3d.atPoint point)
        }
        :: tail


{-| Render the principal-axes (eigenvector) frame of a body as three colored
line segments and the Poinsot inertia ellipsoid as three cross-section
loops in the principal planes. Each axis is scaled by 1/sqrt(eigenvalue)
(the Poinsot ellipsoid radius along that axis), normalized so the longest
principal axis equals the body's bounding sphere radius.
-}
addEigenvectorAxes : SceneParams -> Body -> List Entity -> List Entity
addEigenvectorAxes sceneParams (Physics.Types.Body internalBody) tail =
    if internalBody.mass > 0 then
        let
            invI =
                internalBody.invInertia

            maxInvI =
                max invI.x (max invI.y invI.z)

            scaleFor v =
                internalBody.boundingSphereRadius * sqrt (v / maxInvI)

            transform =
                Mat4.scale3
                    (scaleFor invI.x)
                    (scaleFor invI.y)
                    (scaleFor invI.z)
                    (centerOfMassMat4 internalBody)

            ellipseColor =
                vec3 1 1 0
        in
        axisEntity sceneParams transform xAxisLine (vec3 1 0 0)
            :: axisEntity sceneParams transform yAxisLine (vec3 0 1 0)
            :: axisEntity sceneParams transform zAxisLine (vec3 0 0 1)
            :: axisEntity sceneParams transform circleXY ellipseColor
            :: axisEntity sceneParams transform circleXZ ellipseColor
            :: axisEntity sceneParams transform circleYZ ellipseColor
            :: tail

    else
        tail


axisEntity : SceneParams -> Mat4 -> Mesh Attributes -> Vec3 -> Entity
axisEntity { lightDirection, camera } transform mesh color =
    WebGL.entityWith defaultSettings
        Shaders.vertex
        Shaders.colorFragment
        mesh
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , color = color
        , lightDirection = lightDirection
        , transform = transform
        }


centerOfMassMat4 : InternalBody.Body -> Mat4
centerOfMassMat4 body =
    let
        { m11, m21, m31, m12, m22, m32, m13, m23, m33 } =
            Transform3d.orientation body.transform3d

        comFrame3d : Frame3d.Frame3d Meters WorldCoordinates { defines : CenterOfMassCoordinates }
        comFrame3d =
            Frame3d.unsafe
                { originPoint = Point3d.fromMeters (Transform3d.originPoint body.transform3d)
                , xDirection = Direction3d.unsafe { x = m11, y = m21, z = m31 }
                , yDirection = Direction3d.unsafe { x = m12, y = m22, z = m32 }
                , zDirection = Direction3d.unsafe { x = m13, y = m23, z = m33 }
                }
    in
    Frame3d.toMat4 comFrame3d


xAxisLine : Mesh Attributes
xAxisLine =
    axisLine 1 0 0


yAxisLine : Mesh Attributes
yAxisLine =
    axisLine 0 1 0


zAxisLine : Mesh Attributes
zAxisLine =
    axisLine 0 0 1


axisLine : Float -> Float -> Float -> Mesh Attributes
axisLine x y z =
    WebGL.lines
        [ ( { position = vec3 0 0 0, barycentric = vec3 0 0 0 }
          , { position = vec3 x y z, barycentric = vec3 0 0 0 }
          )
        ]


circleXY : Mesh Attributes
circleXY =
    circleLoop (\c s -> vec3 c s 0)


circleXZ : Mesh Attributes
circleXZ =
    circleLoop (\c s -> vec3 c 0 s)


circleYZ : Mesh Attributes
circleYZ =
    circleLoop (\c s -> vec3 0 c s)


circleLoop : (Float -> Float -> Vec3) -> Mesh Attributes
circleLoop toPos =
    let
        segments =
            48
    in
    List.range 0 (segments - 1)
        |> List.map
            (\i ->
                let
                    t =
                        2 * pi * toFloat i / toFloat segments
                in
                { position = toPos (cos t) (sin t), barycentric = vec3 0 0 0 }
            )
        |> WebGL.lineLoop


defaultSettings : List Setting
defaultSettings =
    [ WebGL.Settings.Blend.add
        WebGL.Settings.Blend.one
        WebGL.Settings.Blend.oneMinusSrcAlpha
    , WebGL.Settings.DepthTest.default
    ]
