module Common.Scene exposing (view)

import Common.Camera exposing (Camera)
import Common.Math as Math
import Common.Meshes as Meshes exposing (Meshes)
import Common.Settings exposing (Settings)
import Common.Shaders as Shaders
import Frame3d
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes as Attributes
import Length exposing (Meters)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.Debug as Debug exposing (FaceNormal, UniqueEdge)
import Physics.World as World exposing (RaycastResult, World)
import Point3d exposing (Point3d)
import WebGL exposing (Entity)


type alias Params a =
    { settings : Settings
    , world : World a
    , camera : Camera
    , meshes : a -> Meshes
    , raycastResult : Maybe (RaycastResult a)
    , floorOffset :
        Maybe
            { x : Float
            , y : Float
            , z : Float
            }
    }


view : Params a -> Html msg
view { settings, world, floorOffset, camera, raycastResult, meshes } =
    let
        lightDirection =
            Vec3.normalize (Vec3.vec3 -1 -1 -1)

        sceneParams =
            { lightDirection = lightDirection
            , camera = camera
            , debugWireframes = settings.debugWireframes
            , debugNormals = settings.debugNormals
            , debugEdges = settings.debugEdges
            , raycastResult = raycastResult
            , meshes = meshes
            , shadow =
                Maybe.map
                    (\offset ->
                        Math.makeShadow
                            (Vec3.fromRecord offset)
                            Vec3.k
                            lightDirection
                    )
                    floorOffset
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

        --, onMouseClick sceneClick
        ]
        ([ ( True
           , \entities -> List.foldl (addBodyEntities sceneParams) entities (World.getBodies world)
           )
         , ( settings.debugContacts
           , \entities -> List.foldl (addContactIndicator sceneParams) entities (Debug.getContacts world)
           )
         ]
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.foldl (<|) []
        )


type alias SceneParams a =
    { lightDirection : Vec3
    , camera : Camera
    , debugWireframes : Bool
    , debugNormals : Bool
    , debugEdges : Bool
    , shadow : Maybe Mat4
    , raycastResult : Maybe (RaycastResult a)
    , meshes : a -> Meshes
    }


addBodyEntities : SceneParams a -> Body a -> List Entity -> List Entity
addBodyEntities ({ meshes, lightDirection, shadow, camera, debugWireframes, debugEdges, debugNormals, raycastResult } as sceneParams) body entities =
    let
        transform =
            Frame3d.toMat4 (Body.getFrame3d body)

        addEdges acc =
            if debugEdges then
                List.foldl (addEdgeIndicator sceneParams transform)
                    acc
                    (Debug.getUniqueEdges body)

            else
                acc

        color =
            Vec3.vec3 0.9 0.9 0.9

        normals =
            case raycastResult of
                Just res ->
                    if Body.getData res.body == Body.getData body then
                        [ { normal = res.normal, point = res.point } ]

                    else
                        []

                Nothing ->
                    []

        addNormals acc =
            if debugNormals then
                List.foldl (addNormalIndicator sceneParams transform)
                    acc
                    (normals ++ Debug.getFaceNormals body)

            else
                acc

        { mesh, wireframe } =
            meshes (Body.getData body)
    in
    entities
        |> addEdges
        |> addNormals
        |> (if debugWireframes then
                (::)
                    (WebGL.entity
                        Shaders.vertex
                        Shaders.wireframeFragment
                        wireframe
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = color
                        , lightDirection = lightDirection
                        , transform = transform
                        }
                    )

            else
                (::)
                    (WebGL.entity
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
        |> (case ( shadow, debugWireframes ) of
                ( Just shadowTransform, False ) ->
                    (::)
                        (WebGL.entity
                            Shaders.vertex
                            Shaders.shadowFragment
                            mesh
                            { camera = camera.cameraTransform
                            , perspective = camera.perspectiveTransform
                            , color = Vec3.vec3 0.25 0.25 0.25
                            , lightDirection = lightDirection
                            , transform = Mat4.mul shadowTransform transform
                            }
                        )

                _ ->
                    identity
           )


{-| Render collision point for the purpose of debugging
-}
addContactIndicator : SceneParams a -> Point3d Meters WorldCoordinates -> List Entity -> List Entity
addContactIndicator { lightDirection, camera } point tail =
    WebGL.entity
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


{-| Render shape face normals for the purpose of debugging
-}
addNormalIndicator : SceneParams a -> Mat4 -> FaceNormal -> List Entity -> List Entity
addNormalIndicator { lightDirection, camera } transform { normal, point } tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        Meshes.normal
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , lightDirection = lightDirection
        , color = Vec3.vec3 1 0 1
        , transform =
            Math.makeRotateKTo (Direction3d.toVec3 normal)
                |> Mat4.mul
                    (Point3d.toVec3 point
                        |> Mat4.makeTranslate
                        |> Mat4.mul transform
                    )
        }
        :: tail


{-| Render shapes' unique edge for the purpose of debugging
-}
addEdgeIndicator : SceneParams a -> Mat4 -> UniqueEdge -> List Entity -> List Entity
addEdgeIndicator { lightDirection, camera } transform { direction, point } tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        Meshes.edge
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , lightDirection = lightDirection
        , color = Vec3.vec3 0 1 0
        , transform =
            Math.makeRotateKTo (Direction3d.toVec3 direction)
                |> Mat4.mul
                    (Point3d.toVec3 point
                        |> Mat4.makeTranslate
                        |> Mat4.mul transform
                    )
        }
        :: tail
