module Shapes.Plane exposing (Plane, placeIn, raycast)

import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias Plane =
    { normal : Vec3
    , position : Vec3
    }


placeIn : Transform3d coordinates defines -> Plane -> Plane
placeIn transform3d { normal, position } =
    { normal = Transform3d.directionPlaceIn transform3d normal
    , position = Transform3d.pointPlaceIn transform3d position
    }


raycast : { from : Vec3, direction : Vec3 } -> Plane -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { from, direction } { normal, position } =
    let
        dot =
            Vec3.dot direction normal
    in
    if dot < 0 then
        let
            pointToFrom =
                Vec3.sub position from

            scalar =
                Vec3.dot normal pointToFrom / dot
        in
        if scalar >= 0 then
            Just
                { distance = scalar
                , point =
                    { x = direction.x * scalar + from.x
                    , y = direction.y * scalar + from.y
                    , z = direction.z * scalar + from.z
                    }
                , normal = normal
                }

        else
            Nothing

    else
        Nothing
