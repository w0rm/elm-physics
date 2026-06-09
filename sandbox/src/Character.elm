module Character exposing (main)

{-| Character controller demo, modelled directly on cannon-es's
PointerLockControlsCannon: capsule body (axis +Z), mild friction,
high linear damping, additive velocity from input. Rotation is
locked via [Physics.lock](Physics#lock) so the capsule slides
upright instead of toppling.

Arrow keys to move, Space to jump.

-}

import Block3d exposing (Block3d)
import Browser.Events as Events
import Common.Demo as Demo
import Common.Meshes as Meshes
import Cylinder3d exposing (Cylinder3d)
import Density
import Dict exposing (Dict)
import Direction3d
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d
import Json.Decode
import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Physics exposing (Body, BodyCoordinates)
import Physics.Lock as Lock
import Physics.Material as Material exposing (Material)
import Physics.Shape as Shape
import Plane3d
import Point3d
import Quantity exposing (Quantity, Rate)
import Speed exposing (Speed)
import Vector3d


walkSpeed : Speed
walkSpeed =
    Speed.metersPerSecond 4


maxDriveForce : Force
maxDriveForce =
    Force.newtons 250


driveGain : Quantity Float (Rate Force.Newtons Speed.MetersPerSecond)
driveGain =
    Quantity.rate (Force.newtons 50) (Speed.metersPerSecond 1)


jumpSpeed : Speed
jumpSpeed =
    Speed.metersPerSecond 4


playerMaterial : Material Material.Dense
playerMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.2
        , bounciness = 0
        }


stairMaterial : Material Material.Dense
stairMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.02
        , bounciness = 0
        }


boxMaterial : Material Material.Dense
boxMaterial =
    Material.dense
        { density = Density.kilogramsPerCubicMeter 700
        , friction = 0.4
        , bounciness = 0
        }


type alias State =
    { forwardInput : Float
    , rightInput : Float
    , grounded : Bool
    , jumpRequested : Bool
    }


initialState : State
initialState =
    { forwardInput = 0
    , rightInput = 0
    , grounded = False
    , jumpRequested = False
    }


type Msg
    = KeyDown Key
    | KeyUp Key


type Key
    = KeyForward
    | KeyBack
    | KeyLeft
    | KeyRight
    | KeyJump


main : Program () (Demo.Model String State) (Demo.Msg Msg)
main =
    let
        base =
            Demo.defaults
                { initialBodies = initialBodies
                , lookupMesh = \_ id -> Dict.get id initialMeshes
                , camera =
                    { from = { x = 0, y = -12, z = 8 }
                    , to = { x = 0, y = 0, z = 0.7 }
                    }
                , initialState = initialState
                }
    in
    Demo.program
        { base
            | floorOffset = Demo.floorAtZero
            , preSimulate = preSimulate
            , postSimulate = postSimulate
            , update = update
            , subscriptions = \_ -> subscriptions
            , reset = \_ -> initialState
            , controls = [ "↑↓←→ move · space jump" ]
        }


update : Msg -> State -> List ( String, Body ) -> ( State, List ( String, Body ), Cmd Msg )
update msg state bodies =
    case msg of
        KeyDown KeyForward ->
            ( { state | forwardInput = 1 }, bodies, Cmd.none )

        KeyDown KeyBack ->
            ( { state | forwardInput = -1 }, bodies, Cmd.none )

        KeyDown KeyLeft ->
            ( { state | rightInput = -1 }, bodies, Cmd.none )

        KeyDown KeyRight ->
            ( { state | rightInput = 1 }, bodies, Cmd.none )

        KeyDown KeyJump ->
            ( { state | jumpRequested = True }, bodies, Cmd.none )

        KeyUp KeyForward ->
            ( { state
                | forwardInput =
                    if state.forwardInput == 1 then
                        0

                    else
                        state.forwardInput
              }
            , bodies
            , Cmd.none
            )

        KeyUp KeyBack ->
            ( { state
                | forwardInput =
                    if state.forwardInput == -1 then
                        0

                    else
                        state.forwardInput
              }
            , bodies
            , Cmd.none
            )

        KeyUp KeyLeft ->
            ( { state
                | rightInput =
                    if state.rightInput == -1 then
                        0

                    else
                        state.rightInput
              }
            , bodies
            , Cmd.none
            )

        KeyUp KeyRight ->
            ( { state
                | rightInput =
                    if state.rightInput == 1 then
                        0

                    else
                        state.rightInput
              }
            , bodies
            , Cmd.none
            )

        KeyUp KeyJump ->
            ( state, bodies, Cmd.none )


preSimulate : Duration -> State -> List ( String, Body ) -> ( State, List ( String, Body ) )
preSimulate dt state bodies =
    let
        wantJump =
            state.jumpRequested && state.grounded

        updatedBodies =
            if state.grounded || wantJump then
                List.map
                    (\( id, body ) ->
                        if id == "player" then
                            let
                                b1 =
                                    if state.grounded then
                                        drivePlayer (Duration.inMilliseconds dt) state.rightInput state.forwardInput body

                                    else
                                        body

                                b2 =
                                    if wantJump then
                                        jumpPlayer b1

                                    else
                                        b1
                            in
                            ( id, b2 )

                        else
                            ( id, body )
                    )
                    bodies

            else
                bodies

        newState =
            { state
                | jumpRequested = False
                , grounded =
                    if wantJump then
                        False

                    else
                        state.grounded
            }
    in
    ( newState, updatedBodies )


postSimulate : Duration -> List ( String, Body ) -> Physics.Contacts String -> State -> State
postSimulate _ bodies contacts state =
    { state | grounded = playerGrounded bodies contacts }


drivePlayer : Float -> Float -> Float -> Body -> Body
drivePlayer dtMs right forward body =
    let
        vel =
            Physics.velocity body

        rawForceX =
            Quantity.at driveGain (Quantity.minus (Vector3d.xComponent vel) (Quantity.multiplyBy right walkSpeed))

        rawForceY =
            Quantity.at driveGain (Quantity.minus (Vector3d.yComponent vel) (Quantity.multiplyBy forward walkSpeed))

        magnitude =
            Quantity.sqrt (Quantity.plus (Quantity.squared rawForceX) (Quantity.squared rawForceY))

        scale =
            if Quantity.greaterThan maxDriveForce magnitude then
                Quantity.ratio maxDriveForce magnitude

            else
                1

        duration =
            Duration.milliseconds dtMs

        impulse =
            Vector3d.xyz
                (Quantity.times duration (Quantity.multiplyBy scale rawForceX))
                (Quantity.times duration (Quantity.multiplyBy scale rawForceY))
                Quantity.zero
    in
    Physics.applyImpulse impulse (Physics.originPoint body) body


jumpPlayer : Body -> Body
jumpPlayer body =
    let
        duration =
            Duration.seconds 1

        jumpForce =
            Quantity.times (Quantity.per duration jumpSpeed) playerMass

        impulse =
            Vector3d.withLength (Quantity.times duration jumpForce) Direction3d.z
    in
    Physics.applyImpulse impulse (Physics.originPoint body) body


playerGrounded : List ( String, Body ) -> Physics.Contacts String -> Bool
playerGrounded bodies contacts =
    case List.filter (\( id, _ ) -> id == "player") bodies of
        [] ->
            False

        ( _, player ) :: _ ->
            let
                threshold =
                    Point3d.zCoordinate (Physics.originPoint player)
                        |> Quantity.minus playerCylinderHalfLength
                        |> Quantity.minus (Length.meters 0.1)
            in
            Physics.contactPoints (\a _ -> a == "player")
                contacts
                |> List.concatMap (\( _, _, pts ) -> pts)
                |> List.any
                    (\pt -> Point3d.zCoordinate pt |> Quantity.lessThan threshold)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        ]


keyDecoder : (Key -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case String.toLower key of
                    "arrowup" ->
                        Json.Decode.succeed (toMsg KeyForward)

                    "arrowdown" ->
                        Json.Decode.succeed (toMsg KeyBack)

                    "arrowleft" ->
                        Json.Decode.succeed (toMsg KeyLeft)

                    "arrowright" ->
                        Json.Decode.succeed (toMsg KeyRight)

                    " " ->
                        Json.Decode.succeed (toMsg KeyJump)

                    _ ->
                        Json.Decode.fail "ignored key"
            )


playerMass : Mass
playerMass =
    Mass.kilograms 5


playerRadius : Length
playerRadius =
    Length.meters 0.3


playerCylinderHalfLength : Length
playerCylinderHalfLength =
    Length.meters 0.4


playerCapsule : Cylinder3d Meters BodyCoordinates
playerCapsule =
    Cylinder3d.centeredOn Point3d.origin
        Direction3d.z
        { radius = playerRadius
        , length = Quantity.twice playerCylinderHalfLength
        }


boxBlock : Block3d Meters BodyCoordinates
boxBlock =
    Block3d.centeredOn Frame3d.atOrigin
        ( Length.meters 0.5, Length.meters 0.5, Length.meters 0.8 )


stairBlocks : List (Block3d Meters BodyCoordinates)
stairBlocks =
    let
        stepHeight =
            Length.meters 0.2

        stepDepth =
            Length.meters 0.5

        width =
            Length.meters 1.5

        topPlatformDepth =
            Length.meters 1.2

        numSteps =
            4

        halfWidth =
            Quantity.half width
    in
    List.map
        (\level ->
            let
                yMin =
                    Quantity.multiplyBy (toFloat level) stepDepth

                yMax =
                    Quantity.plus yMin
                        (if level == numSteps then
                            topPlatformDepth

                         else
                            stepDepth
                        )

                zMax =
                    Quantity.multiplyBy (toFloat (level + 1)) stepHeight
            in
            Block3d.from
                (Point3d.xyz (Quantity.negate halfWidth) yMin Quantity.zero)
                (Point3d.xyz halfWidth yMax zMax)
        )
        (List.range 0 4)


initialBodies : List ( String, Body )
initialBodies =
    let
        floorBody =
            Physics.plane Plane3d.xy Material.wood
                |> Physics.moveTo (Point3d.fromMeters Demo.floorAtZero)

        player =
            Physics.capsule playerCapsule playerMaterial
                |> Physics.scaleMassTo playerMass
                |> Physics.moveTo (Point3d.xyz Quantity.zero (Length.meters -4) (Quantity.plus playerCylinderHalfLength playerRadius))
                |> Physics.lock Lock.allRotation

        boxAt x y =
            Physics.block boxBlock boxMaterial
                |> Physics.scaleMassTo (Mass.kilograms 5)
                |> Physics.moveTo (Point3d.meters x y 0.4)

        boxes =
            List.indexedMap
                (\idx ( x, y ) -> ( "box-" ++ String.fromInt idx, boxAt x y ))
                [ ( -3, -2 )
                , ( -3, 0 )
                , ( -3, 2 )
                , ( -4, 1 )
                , ( 3, -2 )
                , ( 3, 0 )
                , ( 3, 2 )
                , ( 4, 1 )
                ]

        stairsBody =
            Physics.static
                (List.map (\b -> ( Shape.block b, stairMaterial )) stairBlocks)
                |> Physics.moveTo (Point3d.meters 0 1 0)
    in
    ( "floor", floorBody )
        :: ( "player", player )
        :: ( "stairs", stairsBody )
        :: boxes


initialMeshes : Dict String Meshes.Meshes
initialMeshes =
    let
        playerMesh =
            Meshes.fromTriangles (Meshes.capsule 12 playerCapsule)

        boxMesh =
            Meshes.fromTriangles (Meshes.block boxBlock)

        floorMesh =
            Meshes.fromTriangles []

        stairsMesh =
            Meshes.fromTriangleGroups (List.map Meshes.block stairBlocks)

        boxKeys =
            List.range 0 7
                |> List.map (\i -> "box-" ++ String.fromInt i)
    in
    Dict.fromList
        (( "floor", floorMesh )
            :: ( "player", playerMesh )
            :: ( "stairs", stairsMesh )
            :: List.map (\k -> ( k, boxMesh )) boxKeys
        )
