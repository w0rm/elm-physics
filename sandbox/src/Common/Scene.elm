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
import Length exposing (Meters)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
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
                        addContactIndicator sceneParams (Point3d.placeIn frame com) acc

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


defaultSettings : List Setting
defaultSettings =
    [ WebGL.Settings.Blend.add
        WebGL.Settings.Blend.one
        WebGL.Settings.Blend.oneMinusSrcAlpha
    , WebGL.Settings.DepthTest.default
    ]
