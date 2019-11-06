module Physics.World exposing
    ( World, empty, setGravity, add
    , simulate, getBodies, raycast, RaycastResult
    , keepIf, update
    , constrain, constrainIf
    )

{-|

@docs World, empty, setGravity, add

@docs simulate, getBodies, raycast, RaycastResult

@docs keepIf, update

@docs constrain, constrainIf

-}

import Acceleration exposing (Acceleration)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Internal.Body as InternalBody
import Internal.BroadPhase as BroadPhase
import Internal.Constraint as InternalConstraint exposing (ConstraintGroup)
import Internal.Solver as Solver
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Internal.World as Internal exposing (Protected(..))
import Length exposing (Meters)
import Physics.Body exposing (Body)
import Physics.Constraint exposing (Constraint)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Point3d exposing (Point3d)


{-| Physical world is our abstract playground for physical simulations.
-}
type alias World data =
    Protected data


{-| Let be the new world!
-}
empty : World data
empty =
    Protected
        { bodies = []
        , constraints = []
        , freeIds = []
        , nextBodyId = 0
        , gravity = Vec3.zero
        }


{-| Set the [standard gravity](https://en.wikipedia.org/wiki/Standard_gravity) and its direction, e.g.:

    planetEarth =
        setGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
            world

-}
setGravity :
    Acceleration
    -> Direction3d WorldCoordinates
    -> World data
    -> World data
setGravity acceleration direction (Protected world) =
    Protected
        { world
            | gravity =
                Vec3.scale
                    (Acceleration.inMetersPerSecondSquared acceleration)
                    (Direction3d.unwrap direction)
        }


{-| Add a body to the world.

    worldWithFloor =
        add planeBody world

Check the [Physics.Body](Physics-Body) module for possible bodies.

-}
add : Body data -> World data -> World data
add (InternalBody.Protected body) (Protected world) =
    case world.freeIds of
        [] ->
            Protected
                { world
                    | bodies =
                        { body | id = world.nextBodyId }
                            :: world.bodies
                    , nextBodyId = world.nextBodyId + 1
                }

        freeId :: restFreeIds ->
            Protected
                { world
                    | bodies = { body | id = freeId } :: world.bodies
                    , freeIds = restFreeIds
                }


{-| Simulate the world, given the number of seconds since the last frame.

To animate, call this on a message from [onAnimationFrameDelta](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrameDelta):

    simulatedWorld =
        World.simulate (Duration.seconds (1 / 60)) world

Make sure to pass a short duration, such that bodies do not travel
through each other during the single frame. This depends on minimum body
size and maximum velocity.

-}
simulate : Duration -> World data -> World data
simulate dt (Protected world) =
    world
        |> Internal.addGravityForces
        |> Solver.solve (Duration.inSeconds dt) (BroadPhase.getContacts world)
        |> Protected


{-| Get all bodies from the world.

Use this to convert bodies into visual representation,
e.g. WebGL entities.

-}
getBodies : World data -> List (Body data)
getBodies (Protected { bodies }) =
    List.map InternalBody.Protected bodies


{-| Find the closest intersection of a ray against
all the bodies in the world. Except for particles,
because they have no size.
-}
raycast :
    Point3d Meters WorldCoordinates
    -> Direction3d WorldCoordinates
    -> World data
    -> Maybe (RaycastResult data)
raycast from direction (Protected world) =
    case Internal.raycast { direction = Direction3d.unwrap direction, from = Point3d.toMeters from } world of
        Just { body, point, normal } ->
            Just
                { body = InternalBody.Protected body
                , point = Point3d.fromMeters (Transform3d.pointRelativeTo (Transform3d.placeIn body.transform3d (Transform3d.inverse body.centerOfMassTransform3d)) point)
                , normal = Direction3d.unsafe (Transform3d.directionRelativeTo (Transform3d.placeIn body.transform3d (Transform3d.inverse body.centerOfMassTransform3d)) normal)
                }

        _ ->
            Nothing


{-| The Raycast result includes the intersected body,
intersection point and normal vector on the face,
expressed within the local body coordinate system.

Use the `Frame3d` from [Body.getFrame3d](Physics-Body#getFrame3d)
to transform the result into world coordinates.

-}
type alias RaycastResult data =
    { body : Body data
    , point : Point3d Meters BodyCoordinates
    , normal : Direction3d BodyCoordinates
    }


{-| Keep bodies that satisfy the test.
-}
keepIf : (Body data -> Bool) -> World data -> World data
keepIf fn (Protected world) =
    let
        ( keptBodies, removedBodies ) =
            List.partition (InternalBody.Protected >> fn) world.bodies
    in
    Protected
        { world
            | bodies = keptBodies
            , freeIds = List.foldl (.id >> (::)) world.freeIds removedBodies
        }


{-| Apply a function to every body in the world.
-}
update : (Body data -> Body data) -> World data -> World data
update fn (Protected world) =
    let
        -- Keep the old ids
        internalUpdate body =
            let
                (InternalBody.Protected updatedBody) =
                    fn (InternalBody.Protected body)
            in
            { updatedBody | id = body.id }
    in
    Protected { world | bodies = List.map internalUpdate world.bodies }


{-| Configure constraints between pairs of bodies. Constraints allow to limit the
freedom of movement of two bodies with relation to each other.

Check the [Physics.Constraint](Physics-Constraint) module for possible constraints.

    worldWithACar : World { part : String }
    worldWithACar =
        constrain
            (\b1 b2 ->
                case
                    ( (Body.getData b1).part
                    , (Body.getData b2).part
                    )
                of
                    ( "wheel1", "base" ) ->
                        [ hingeConstraint1 ]

                    ( "wheel2", "base" ) ->
                        [ hingeConstraint2 ]

                    ( "wheel3", "base" ) ->
                        [ hingeConstraint3 ]

                    ( "wheel4", "base" ) ->
                        [ hingeConstraint4 ]

                    _ ->
                        []
            )
            worldWithCarParts

Note that this example only works for a single car, otherwise it would
connect wheels of one car with the base of another another. You might want
to use `constrainIf` to apply constraints on a subset of bodies.

    constrain =
        constrainIf (always True)

-}
constrain :
    (Body data -> Body data -> List Constraint)
    -> World data
    -> World data
constrain =
    constrainIf (always True)


{-| Configure constraints for a subset of bodies that satisfy the test.

For the above example we can tag each part of a car with the `carId`,
and preselect parts of a single car with:

    constrainCar carId =
        constrainIf
            (\body ->
                (Body.getData body).carId == carId
            )

-}
constrainIf :
    (Body data -> Bool)
    -> (Body data -> Body data -> List Constraint)
    -> World data
    -> World data
constrainIf test fn (Protected world) =
    let
        -- Filter the bodies for permutations
        filteredBodies =
            List.filter
                (\body -> test (InternalBody.Protected body))
                world.bodies

        -- Keep untouched constraints
        filteredConstraints =
            List.filter
                (\{ bodyId1, bodyId2 } -> not (List.any (\body -> body.id == bodyId1 || body.id == bodyId2) filteredBodies))
                world.constraints

        -- Add constraints for two bodies
        addFor : InternalBody.Body data -> InternalBody.Body data -> List ConstraintGroup -> List ConstraintGroup
        addFor body1 body2 constraintGroup =
            case fn (InternalBody.Protected body1) (InternalBody.Protected body2) of
                [] ->
                    constraintGroup

                constraints ->
                    { bodyId1 = body1.id
                    , bodyId2 = body2.id
                    , constraints = List.map (InternalConstraint.relativeToCenterOfMass body1.centerOfMassTransform3d body2.centerOfMassTransform3d) constraints
                    }
                        :: constraintGroup

        -- Add constraints for all combinations of bodies
        addConstraintsHelp : List (InternalBody.Body data) -> List ConstraintGroup -> List ConstraintGroup
        addConstraintsHelp list result =
            case list of
                body1 :: rest ->
                    addConstraintsHelp rest
                        (List.foldl
                            (\body2 constraints ->
                                constraints
                                    |> addFor body1 body2
                                    |> addFor body2 body1
                            )
                            result
                            rest
                        )

                [] ->
                    result
    in
    Protected
        { world
            | constraints = addConstraintsHelp filteredBodies filteredConstraints
        }
