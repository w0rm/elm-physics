module Common.Scene exposing (interpolatedBodies, view)

import Common.Camera exposing (Camera)
import Common.Meshes as Meshes exposing (Attributes, Meshes)
import Common.Settings exposing (Settings)
import Common.Shaders as Shaders
import Direction3d
import Frame3d
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
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
import WebGL.Settings.StencilTest


{-| Construct the `bodies` parameter for `view` by interpolating each
body between its previous and current simulation states. `prev` and
`curr` must be in the same order (typically they are — `Physics.simulate`
preserves the list shape).
-}
interpolatedBodies :
    Float
    -> List ( id, Body )
    -> List ( id, Body )
    -> (id -> Maybe Meshes)
    ->
        List
            ( Meshes
            , Body
            , Frame3d.Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
            )
interpolatedBodies a prev curr lookupMesh =
    List.filterMap identity
        (List.map2
            (\( id, p ) ( _, c ) ->
                Maybe.map
                    (\mesh -> ( mesh, c, Physics.interpolatedFrame a p c ))
                    (lookupMesh id)
            )
            prev
            curr
        )


type alias Params =
    { settings : Settings
    , bodies :
        List
            ( Meshes
            , Body
            , Frame3d.Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
            )
    , contacts : List (Point3d Meters WorldCoordinates)
    , camera : Camera
    , contactRadius : Float
    , floorOffset :
        { x : Float
        , y : Float
        , z : Float
        }
    }


{-| World-space direction towards the single scene light. Surfaces whose
outward normal points towards it are lit; everything else only gets the
ambient term and therefore reads as "in shadow".
-}
sceneLight : Vec3
sceneLight =
    Vec3.normalize (vec3 -0.5 0.4 1.4)


bodyColor : Vec3
bodyColor =
    vec3 0.9 0.9 0.9


floorColor : Vec3
floorColor =
    vec3 0.35 0.35 0.35


view : Params -> Html msg
view { settings, bodies, contacts, floorOffset, camera, contactRadius } =
    let
        sceneParams =
            { lightDirection = sceneLight
            , camera = camera
            , contactRadius = contactRadius
            , debugWireframes = settings.debugWireframes
            , debugCenterOfMass = settings.debugCenterOfMass
            , debugInertia = settings.debugInertia
            }

        prepared =
            List.map
                (\( meshes, body, frame ) ->
                    { meshes = meshes
                    , body = body
                    , transform = Frame3d.toMat4 frame
                    }
                )
                bodies

        uniforms transform color =
            { camera = camera.cameraTransform
            , perspective = camera.perspectiveTransform
            , transform = transform
            , color = color
            , lightDirection = sceneLight
            }

        floorTransform =
            Mat4.makeTranslate (Vec3.fromRecord floorOffset)

        litEntities =
            if settings.debugWireframes then
                List.map
                    (\{ meshes, transform } ->
                        WebGL.entityWith defaultSettings
                            Shaders.wireframeVertex
                            Shaders.wireframeFragment
                            meshes.mesh
                            (uniforms transform bodyColor)
                    )
                    prepared

            else
                -- Pass 1: ambient base color for the floor and every body.
                (WebGL.entityWith ambientSettings
                    Shaders.vertex
                    Shaders.ambientFragment
                    floorMesh
                    (uniforms floorTransform floorColor)
                    :: List.map
                        (\{ meshes, transform } ->
                            WebGL.entityWith ambientSettings
                                Shaders.vertex
                                Shaders.ambientFragment
                                meshes.mesh
                                (uniforms transform bodyColor)
                        )
                        prepared
                )
                    -- Pass 2: each body's shadow volume into the stencil buffer.
                    ++ List.map
                        (\{ meshes, transform } ->
                            WebGL.entityWith shadowVolumeSettings
                                Shaders.shadowVolumeVertex
                                Shaders.shadowVolumeFragment
                                meshes.shadow
                                (uniforms transform bodyColor)
                        )
                        prepared
                    -- Pass 3: add the directional term only where unshadowed.
                    ++ (WebGL.entityWith diffuseSettings
                            Shaders.vertex
                            Shaders.diffuseFragment
                            floorMesh
                            (uniforms floorTransform floorColor)
                            :: List.map
                                (\{ meshes, transform } ->
                                    WebGL.entityWith diffuseSettings
                                        Shaders.vertex
                                        Shaders.diffuseFragment
                                        meshes.mesh
                                        (uniforms transform bodyColor)
                                )
                                prepared
                       )

        overlayEntities =
            List.concatMap (centerOfMassEntities sceneParams) prepared
                ++ (if settings.debugContacts then
                        List.foldr (addContactIndicator sceneParams) [] contacts

                    else
                        []
                   )
    in
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.stencil 0
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
        (litEntities ++ overlayEntities)


type alias SceneParams =
    { lightDirection : Vec3
    , camera : Camera
    , contactRadius : Float
    , debugWireframes : Bool
    , debugCenterOfMass : Bool
    , debugInertia : Bool
    }


{-| Center-of-mass dot (and, with inertia debugging on, the principal axes)
for a single body, drawn as a non-shadowed overlay.
-}
centerOfMassEntities : SceneParams -> { a | body : Body } -> List Entity
centerOfMassEntities sceneParams { body } =
    if sceneParams.debugCenterOfMass then
        case Physics.centerOfMass body of
            Just com ->
                let
                    withDot =
                        addContactIndicator sceneParams com []
                in
                if sceneParams.debugInertia then
                    addEigenvectorAxes sceneParams body withDot

                else
                    withDot

            Nothing ->
                []

    else
        []


{-| Render a collision point for the purpose of debugging
-}
addContactIndicator : SceneParams -> Point3d Meters WorldCoordinates -> List Entity -> List Entity
addContactIndicator { lightDirection, camera, contactRadius } point tail =
    WebGL.entityWith defaultSettings
        Shaders.vertex
        Shaders.fragment
        Meshes.contact
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , color = Vec3.vec3 1 0 0
        , lightDirection = lightDirection
        , transform =
            Mat4.scale3 contactRadius
                contactRadius
                contactRadius
                (Frame3d.toMat4 (Frame3d.atPoint point))
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
                internalBody.geometry.boundingSphereRadius * sqrt (v / maxInvI)

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


{-| A large ground quad in the xy-plane that receives the bodies' shadows.
It is rendered (ambient + diffuse) but never casts a shadow of its own.
-}
floorMesh : Mesh Attributes
floorMesh =
    let
        size =
            50

        corner x y =
            { position = vec3 x y 0, barycentric = vec3 0 0 0 }
    in
    WebGL.triangles
        [ ( corner -size -size, corner size -size, corner size size )
        , ( corner size size, corner -size size, corner -size -size )
        ]


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


{-| Settings for the overlays (wireframes, contacts, debug axes) that draw
on top of the shaded scene with ordinary depth testing.
-}
defaultSettings : List Setting
defaultSettings =
    [ WebGL.Settings.Blend.add
        WebGL.Settings.Blend.one
        WebGL.Settings.Blend.oneMinusSrcAlpha
    , WebGL.Settings.DepthTest.default
    ]


{-| Pass 1: write opaque ambient color and depth for front faces.
-}
ambientSettings : List Setting
ambientSettings =
    [ WebGL.Settings.DepthTest.default
    , WebGL.Settings.cullFace WebGL.Settings.back
    ]


{-| Pass 2: count shadow-volume entries/exits into the stencil buffer with
the z-pass technique, without touching color or depth.
-}
shadowVolumeSettings : List Setting
shadowVolumeSettings =
    [ WebGL.Settings.DepthTest.less { write = False, near = 0, far = 1 }
    , WebGL.Settings.colorMask False False False False
    , WebGL.Settings.StencilTest.testSeparate
        { ref = 1, mask = 0xFF, writeMask = 0xFF }
        { test = WebGL.Settings.StencilTest.always
        , fail = WebGL.Settings.StencilTest.keep
        , zfail = WebGL.Settings.StencilTest.keep
        , zpass = WebGL.Settings.StencilTest.incrementWrap
        }
        { test = WebGL.Settings.StencilTest.always
        , fail = WebGL.Settings.StencilTest.keep
        , zfail = WebGL.Settings.StencilTest.keep
        , zpass = WebGL.Settings.StencilTest.decrementWrap
        }
    ]


{-| Pass 3: add the directional term where the stencil is zero (lit).
-}
diffuseSettings : List Setting
diffuseSettings =
    [ WebGL.Settings.DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
    , WebGL.Settings.StencilTest.test
        { ref = 0
        , mask = 0xFF
        , test = WebGL.Settings.StencilTest.equal
        , fail = WebGL.Settings.StencilTest.keep
        , zfail = WebGL.Settings.StencilTest.keep
        , zpass = WebGL.Settings.StencilTest.keep
        , writeMask = 0x00
        }
    , WebGL.Settings.cullFace WebGL.Settings.back
    , WebGL.Settings.Blend.add WebGL.Settings.Blend.one WebGL.Settings.Blend.one
    ]
