module Extra.Expect exposing
    ( contacts
    , frame3d
    , mat3
    , transform3d
    , vec3
    , vec3s
    )

import Direction3d
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Frame3d exposing (Frame3d)
import Internal.Contact exposing (Contact)
import Internal.Matrix3 exposing (Mat3)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 exposing (Vec3)
import Point3d


contacts : List Contact -> List Contact -> Expectation
contacts =
    expectList contact


vec3s : List Vec3 -> List Vec3 -> Expectation
vec3s =
    expectList vec3


contact : Contact -> Contact -> Expectation
contact =
    expectAll (List.map (Tuple.pair vec3) [ .pi, .pj, .ni ])


vec3 : Vec3 -> Vec3 -> Expectation
vec3 =
    expectAll (List.map (Tuple.pair (Expect.within tolerance)) [ .x, .y, .z ])


mat3 : Mat3 -> Mat3 -> Expectation
mat3 =
    expectAll
        (List.map (Tuple.pair (Expect.within tolerance))
            [ .m11, .m21, .m31, .m12, .m22, .m32, .m13, .m23, .m33 ]
        )


transform3d : Transform3d coords define -> Transform3d coords define -> Expectation
transform3d transform =
    Expect.all
        [ \subj -> vec3 (Transform3d.originPoint transform) (Transform3d.originPoint subj)
        , \subj -> mat3 (Transform3d.orientation transform) (Transform3d.orientation subj)
        ]


frame3d : Frame3d units coords define -> Frame3d units coords define -> Expectation
frame3d =
    expectAll
        (List.map (Tuple.pair vec3)
            [ Frame3d.originPoint >> Point3d.unwrap
            , Frame3d.xDirection >> Direction3d.unwrap
            , Frame3d.yDirection >> Direction3d.unwrap
            , Frame3d.zDirection >> Direction3d.unwrap
            ]
        )


expectList : (a -> a -> Expectation) -> List a -> List a -> Expectation
expectList fn l1 l2 =
    Expect.all
        ((\_ ->
            Expect.equal (List.length l1) (List.length l2)
                |> Expect.onFail "List sizes do not match"
         )
            :: List.map2 (\a b -> \_ -> fn a b) l1 l2
        )
        ()


expectAll : List ( b -> b -> Expectation, a -> b ) -> a -> a -> Expectation
expectAll expectations a b =
    Expect.all
        (List.map
            (\( expectation, getter ) ->
                \_ -> expectation (getter a) (getter b)
            )
            expectations
        )
        ()
        |> Expect.onFail ("Expected " ++ Debug.toString a ++ ", got " ++ Debug.toString b)


tolerance : FloatingPointTolerance
tolerance =
    Absolute 0.0001
