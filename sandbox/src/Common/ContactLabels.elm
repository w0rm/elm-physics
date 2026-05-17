module Common.ContactLabels exposing (view)

{-| Render contact-id labels at each contact point in screen space.
The label projection uses the camera's perspective × view matrices and
returns `Nothing` for points behind the camera (so off-screen contacts
silently disappear).
-}

import Common.Camera exposing (Camera)
import Html exposing (Html)
import Html.Attributes
import Length exposing (Meters)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Physics exposing (WorldCoordinates)
import Point3d exposing (Point3d)


view :
    Camera
    -> List { id : String, point : Point3d Meters WorldCoordinates }
    -> Html msg
view camera contacts =
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "pointer-events" "none"
        ]
        (List.filterMap (label camera) contacts)


label :
    Camera
    -> { id : String, point : Point3d Meters WorldCoordinates }
    -> Maybe (Html msg)
label camera entry =
    let
        p =
            Point3d.toMeters entry.point
    in
    case project camera p of
        Nothing ->
            Nothing

        Just { x, y } ->
            Just
                (Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "left" (String.fromFloat x ++ "px")
                    , Html.Attributes.style "top" (String.fromFloat y ++ "px")
                    , Html.Attributes.style "transform" "translate(8px, -8px)"
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "font-family" "monospace"
                    , Html.Attributes.style "font-size" "11px"
                    , Html.Attributes.style "background" "rgba(180, 30, 30, 0.85)"
                    , Html.Attributes.style "padding" "1px 4px"
                    , Html.Attributes.style "border-radius" "2px"
                    , Html.Attributes.style "white-space" "nowrap"
                    ]
                    [ Html.text entry.id ]
                )


project :
    Camera
    -> { x : Float, y : Float, z : Float }
    -> Maybe { x : Float, y : Float }
project camera point =
    let
        forwardX =
            camera.to.x - camera.from.x

        forwardY =
            camera.to.y - camera.from.y

        forwardZ =
            camera.to.z - camera.from.z

        toX =
            point.x - camera.from.x

        toY =
            point.y - camera.from.y

        toZ =
            point.z - camera.from.z

        front =
            toX * forwardX + toY * forwardY + toZ * forwardZ
    in
    if front <= 0 then
        Nothing

    else
        let
            mvp =
                Mat4.mul camera.perspectiveTransform camera.cameraTransform

            ndc =
                Mat4.transform mvp (Vec3.fromRecord point)
        in
        Just
            { x = (Vec3.getX ndc + 1) * 0.5 * camera.width
            , y = (1 - Vec3.getY ndc) * 0.5 * camera.height
            }
