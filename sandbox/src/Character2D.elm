module Character2D exposing (main)

{-| 2D character controller demo. Motion is locked to the world XZ
plane: Y translation is locked, and all rotation is locked. The
player is a capsule with axis +Z. The camera looks straight down the
-Y axis, so the simulation reads as a classic side-scrolling
platformer.

Left/Right arrows to move, Space to jump.

-}

import Angle
import Axis3d
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


planarLock : List Lock.Lock
planarLock =
    Lock.translateY :: Lock.allRotation


type alias State =
    { rightInput : Float
    , grounded : Bool
    , jumpRequested : Bool
    }


initialState : State
initialState =
    { rightInput = 0
    , grounded = False
    , jumpRequested = False
    }


type Msg
    = KeyDown Key
    | KeyUp Key


type Key
    = KeyLeft
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
                    { from = { x = 0, y = -14, z = 2 }
                    , to = { x = 0, y = 0, z = 1.2 }
                    }
                , initialState = initialState
                }
    in
    Demo.program
        { base
            | floorOffset = Demo.floorAtZero
            , preSimulate = preSimulate
            , postSimulate = postSimulate
            , cameraTarget = cameraTarget
            , update = update
            , subscriptions = \_ -> subscriptions
            , reset = \_ -> initialState
            , controls = [ "←/→ move · space jump" ]
        }


update : Msg -> State -> List ( String, Body ) -> ( State, List ( String, Body ), Cmd Msg )
update msg state bodies =
    case msg of
        KeyDown KeyLeft ->
            ( { state | rightInput = -1 }, bodies, Cmd.none )

        KeyDown KeyRight ->
            ( { state | rightInput = 1 }, bodies, Cmd.none )

        KeyDown KeyJump ->
            ( { state | jumpRequested = True }, bodies, Cmd.none )

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
                                        drivePlayer (Duration.inMilliseconds dt) state.rightInput body

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


{-| Track the player's X with the orbit camera by sliding the orbit
target sideways. Z and Y are kept fixed so the user can still orbit
freely around the player.
-}
cameraTarget : State -> List ( String, Body ) -> Maybe Demo.Vec3R
cameraTarget _ bodies =
    case List.filter (\( id, _ ) -> id == "player") bodies of
        [] ->
            Nothing

        ( _, player ) :: _ ->
            let
                origin =
                    Point3d.toMeters (Physics.originPoint player)
            in
            Just { x = origin.x, y = 0, z = 1.2 }


drivePlayer : Float -> Float -> Body -> Body
drivePlayer dtMs right body =
    let
        currentVx =
            Vector3d.xComponent (Physics.velocity body)

        force =
            Quantity.at driveGain (Quantity.minus currentVx (Quantity.multiplyBy right walkSpeed))
                |> Quantity.clamp (Quantity.negate maxDriveForce) maxDriveForce

        impulse =
            Vector3d.xyz
                (Quantity.times (Duration.milliseconds dtMs) force)
                Quantity.zero
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
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 0.5, Length.meters 0.5, Length.meters 0.3 )


plankBlock : Block3d Meters BodyCoordinates
plankBlock =
    Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 4, Length.meters 0.6, Length.meters 0.1 )


plankTilt : Angle.Angle
plankTilt =
    Angle.degrees -20


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
                xMin =
                    Quantity.multiplyBy (toFloat level) stepDepth

                xMax =
                    Quantity.plus xMin
                        (if level == numSteps then
                            topPlatformDepth

                         else
                            stepDepth
                        )

                zMax =
                    Quantity.multiplyBy (toFloat (level + 1)) stepHeight
            in
            Block3d.from
                (Point3d.xyz xMin (Quantity.negate halfWidth) Quantity.zero)
                (Point3d.xyz xMax halfWidth zMax)
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
                |> Physics.moveTo (Point3d.xyz (Length.meters -4) Quantity.zero (Quantity.plus playerCylinderHalfLength playerRadius))
                |> Physics.lock planarLock

        boxAt x z =
            Physics.block boxBlock boxMaterial
                |> Physics.scaleMassTo (Mass.kilograms 5)
                |> Physics.moveTo (Point3d.meters x 0 z)
                |> Physics.lock planarLock

        boxes =
            List.indexedMap
                (\idx ( x, z ) -> ( "box-" ++ String.fromInt idx, boxAt x z ))
                [ ( -1.5, 0.15 )
                , ( 5, 0.15 )
                , ( 5, 0.45 )
                , ( 11, 0.15 )
                ]

        stairsBody =
            Physics.static
                (List.map (\b -> ( Shape.block b, stairMaterial )) stairBlocks)
                |> Physics.moveTo (Point3d.meters 0 0 0)

        plankBody =
            Physics.static [ ( Shape.block plankBlock, Material.wood ) ]
                |> Physics.rotateAround Axis3d.y plankTilt
                |> Physics.moveTo (Point3d.meters 8 0 0.8)
    in
    ( "floor", floorBody )
        :: ( "player", player )
        :: ( "stairs", stairsBody )
        :: ( "plank", plankBody )
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

        plankMesh =
            Meshes.fromTriangles (Meshes.block plankBlock)

        boxKeys =
            List.range 0 3
                |> List.map (\i -> "box-" ++ String.fromInt i)
    in
    Dict.fromList
        (( "floor", floorMesh )
            :: ( "player", playerMesh )
            :: ( "stairs", stairsMesh )
            :: ( "plank", plankMesh )
            :: List.map (\k -> ( k, boxMesh )) boxKeys
        )
