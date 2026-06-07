module Car exposing (main)

{-| This shows how hinge constrains can be used to assemble a car.
Use the arrow keys to steer and speed!
-}

import Angle
import Axis3d
import Block3d
import Browser.Events as Events
import Common.Demo as Demo
import Common.Meshes as Meshes
import Dict exposing (Dict)
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Json.Decode
import Length
import Mass
import Physics exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape
import Plane3d
import Point3d
import Sphere3d
import Torque
import Vector3d


type Command
    = Speed Float
    | Steer Float


type Msg
    = KeyDown Command
    | KeyUp Command


type alias State =
    { speeding : Float
    , steering : Float
    }


initialState : State
initialState =
    { speeding = 0, steering = 0 }


main : Program () (Demo.Model String State) (Demo.Msg Msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = initialBodies
                , lookupMesh = \_ id -> Dict.get id initialMeshes
                , camera =
                    { from = { x = -60, y = 60, z = 40 }
                    , to = { x = 0, y = -7, z = 0 }
                    }
                , initialState = initialState
                }
    in
    Demo.program
        { base
            | settingsInit = identity
            , constrain = \state _ -> constrainCar state.steering
            , preSimulate = preSimulate
            , update = update
            , subscriptions = \_ -> subscriptions
            , reset = \_ -> initialState
            , controls = [ "↑/↓ accelerate · ←/→ steer" ]
        }


update : Msg -> State -> List ( String, Body ) -> ( State, List ( String, Body ), Cmd Msg )
update msg state bodies =
    case msg of
        KeyDown (Speed k) ->
            ( { state | speeding = k }, bodies, Cmd.none )

        KeyDown (Steer k) ->
            ( { state | steering = k }, bodies, Cmd.none )

        KeyUp (Speed k) ->
            ( { state
                | speeding =
                    if k == state.speeding then
                        0

                    else
                        state.speeding
              }
            , bodies
            , Cmd.none
            )

        KeyUp (Steer k) ->
            ( { state
                | steering =
                    if k == state.steering then
                        0

                    else
                        state.steering
              }
            , bodies
            , Cmd.none
            )


preSimulate : Duration -> State -> List ( String, Body ) -> ( State, List ( String, Body ) )
preSimulate _ state bodies =
    if state.speeding == 0 then
        ( state, bodies )

    else
        let
            newBodies =
                List.map
                    (\( id, body ) ->
                        if id == "wheel1" || id == "wheel2" then
                            ( id, applySpeed state.speeding body )

                        else
                            ( id, body )
                    )
                    bodies
        in
        ( state, newBodies )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        ]


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "ArrowRight" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "ArrowUp" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "ArrowDown" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )


applySpeed : Float -> Body -> Body
applySpeed speed body =
    let
        torque =
            Vector3d.withLength (Torque.newtonMeters (speed * 20))
                (Direction3d.placeIn (Physics.frame body) Direction3d.positiveX)
    in
    body
        |> Physics.applyTorque torque


constrainCar : Float -> String -> Maybe (String -> List Constraint)
constrainCar steering id1 =
    let
        steeringAngle =
            steering * pi / 8

        dx =
            cos steeringAngle

        dy =
            sin steeringAngle

        hinge1 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 3 3 0)
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )
                (Axis3d.through
                    (Point3d.meters 0 0 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )

        hinge2 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -3 3 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge3 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -3 -3 0)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        hinge4 =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 3 -3 0)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
    in
    if id1 == "base" then
        Just
            (\id2 ->
                case id2 of
                    "wheel1" ->
                        [ hinge1 ]

                    "wheel2" ->
                        [ hinge2 ]

                    "wheel3" ->
                        [ hinge3 ]

                    "wheel4" ->
                        [ hinge4 ]

                    _ ->
                        []
            )

    else
        Nothing


initialBodies : List ( String, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorZ)

        slopeBlock3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )

        slopeBody =
            Physics.static [ ( Shape.block slopeBlock3d, Material.wood ) ]
                |> Physics.rotateAround Axis3d.x (Angle.radians (pi / 16))
                |> Physics.moveTo (Point3d.meters 0 -2 1)

        bottom =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 3, Length.meters 6, Length.meters 1 )

        top =
            Block3d.centeredOn
                (Frame3d.atPoint (Point3d.meters 0 1 1))
                ( Length.meters 2, Length.meters 3, Length.meters 1.5 )

        baseBody =
            Physics.dynamic
                [ ( Shape.sum [ Shape.block top, Shape.block bottom ], Material.wood ) ]
                |> Physics.scaleMassTo (Mass.kilograms 10)
                |> Physics.moveTo (Point3d.meters 0 0 5)

        sphere3d =
            Sphere3d.atOrigin (Length.meters 1.2)

        wheelBody =
            Physics.sphere sphere3d Material.rubber
                |> Physics.scaleMassTo (Mass.kilograms 2)

        offset =
            Point3d.meters 0 0 5
    in
    [ ( "floor", floorBody )
    , ( "slope", slopeBody )
    , ( "base", baseBody )
    , ( "wheel1"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters 3 3 0)
      )
    , ( "wheel2"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters -3 3 0)
      )
    , ( "wheel3"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters -3 -3 0)
      )
    , ( "wheel4"
      , wheelBody
            |> Physics.moveTo offset
            |> Physics.translateBy (Vector3d.meters 3 -3 0)
      )
    ]


initialMeshes : Dict String Meshes.Meshes
initialMeshes =
    let
        slopeBlock3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )

        bottom =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 3, Length.meters 6, Length.meters 1 )

        top =
            Block3d.centeredOn
                (Frame3d.atPoint (Point3d.meters 0 1 1))
                ( Length.meters 2, Length.meters 3, Length.meters 1.5 )

        sphere3d =
            Sphere3d.atOrigin (Length.meters 1.2)

        wheelMesh =
            Meshes.fromTriangles (Meshes.sphere 2 sphere3d)
    in
    Dict.fromList
        [ ( "floor", Meshes.fromTriangles [] )
        , ( "slope", Meshes.fromTriangles (Meshes.block slopeBlock3d) )
        , ( "base", Meshes.fromTriangles (Meshes.block bottom ++ Meshes.block top) )
        , ( "wheel1", wheelMesh )
        , ( "wheel2", wheelMesh )
        , ( "wheel3", wheelMesh )
        , ( "wheel4", wheelMesh )
        ]
