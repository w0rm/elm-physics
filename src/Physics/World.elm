module Physics.World exposing
    ( World, empty, withGravity, add
    , simulate, bodies, contacts
    , raycast, RaycastResult
    , keepIf, update
    , constrain, constrainIf
    )

{-|

@docs World, empty, withGravity, add

@docs simulate, bodies, contacts

@docs raycast, RaycastResult

@docs keepIf, update

@docs constrain, constrainIf

-}

import Acceleration exposing (Acceleration)
import Array
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Internal.Body as InternalBody
import Internal.BroadPhase as BroadPhase
import Internal.Constraint as InternalConstraint exposing (ConstraintGroup)
import Internal.Contact as InternalContact
import Internal.Solver as Solver
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Internal.World as Internal exposing (Protected(..))
import Length exposing (Meters)
import Physics.Body exposing (Body)
import Physics.Constraint exposing (Constraint)
import Physics.Contact exposing (Contact)
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
        , contactGroups = []
        , simulatedBodies = Array.empty
        }


{-| Set the [standard gravity](https://en.wikipedia.org/wiki/Standard_gravity) and its direction, e.g.:

    planetEarth =
        withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
            world

-}
withGravity :
    Acceleration
    -> Direction3d WorldCoordinates
    -> World data
    -> World data
withGravity acceleration direction (Protected world) =
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
        |> BroadPhase.addContacts
        |> Solver.solve (Duration.inSeconds dt)
        |> Protected


{-| Get all bodies from the world in unspecified order.

Use this to convert bodies into visual representation,
e.g. WebGL entities.

-}
bodies : World data -> List (Body data)
bodies (Protected world) =
    List.foldl
        (\body result -> InternalBody.Protected body :: result)
        []
        world.bodies


{-| Get all contacts from the last simulation frame.
-}
contacts : World data -> List (Contact data)
contacts (Protected { contactGroups, simulatedBodies }) =
    let
        mapContact :
            InternalBody.Body data
            -> InternalBody.Body data
            -> InternalContact.Contact
            ->
                { point : Point3d Meters WorldCoordinates
                , normal : Direction3d WorldCoordinates
                }
        mapContact oldBody newBody =
            let
                transform =
                    Transform3d.atOrigin
                        |> Transform3d.relativeTo oldBody.transform3d
                        |> Transform3d.placeIn newBody.transform3d
            in
            \{ ni, pi } ->
                { point =
                    pi
                        |> Transform3d.pointPlaceIn transform
                        |> Point3d.fromMeters
                , normal =
                    ni
                        |> Transform3d.directionPlaceIn transform
                        |> Direction3d.unsafe
                }
    in
    List.filterMap
        (\contactGroup ->
            case Array.get contactGroup.body1.id simulatedBodies of
                Just newBody1 ->
                    case Array.get contactGroup.body2.id simulatedBodies of
                        Just newBody2 ->
                            Just
                                (InternalContact.Protected
                                    { body1 = newBody1
                                    , body2 = newBody2
                                    , points =
                                        List.foldl
                                            (\point result ->
                                                mapContact
                                                    contactGroup.body1
                                                    newBody1
                                                    point
                                                    :: result
                                            )
                                            []
                                            contactGroup.contacts
                                    }
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        )
        contactGroups


{-| Find the closest intersection of a ray against
all the bodies in the world. Except for particles,
because they have no size.
-}
raycast :
    Axis3d Meters WorldCoordinates
    -> World data
    -> Maybe (RaycastResult data)
raycast ray (Protected world) =
    case
        Internal.raycast
            { direction = Direction3d.unwrap (Axis3d.direction ray)
            , from = Point3d.toMeters (Axis3d.originPoint ray)
            }
            world
    of
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

Use the `Frame3d` from [Body.frame](Physics-Body#frame)
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

        removedIds =
            List.foldl (.id >> (::)) [] removedBodies

        keptConstraints =
            List.foldl
                (\c result ->
                    if List.member c.bodyId1 removedIds || List.member c.bodyId2 removedIds then
                        result

                    else
                        c :: result
                )
                []
                world.constraints
    in
    Protected
        { world
            | bodies = keptBodies
            , constraints = keptConstraints
            , freeIds = removedIds ++ world.freeIds
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
    Protected
        { world
            | bodies =
                List.foldl
                    (\body result -> internalUpdate body :: result)
                    []
                    world.bodies
        }


{-| Configure constraints between pairs of bodies. Constraints allow to limit the
freedom of movement of two bodies with relation to each other.

Check the [Physics.Constraint](Physics-Constraint) module for possible constraints.

    worldWithACar : World { part : String }
    worldWithACar =
        constrain
            (\b1 b2 ->
                case
                    ( (Body.data b1).part
                    , (Body.data b2).part
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
connect wheels of one car with the base of another. You might want
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
                (Body.data body).carId == carId
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
                    , constraints =
                        List.foldl
                            (\constraint result ->
                                InternalConstraint.relativeToCenterOfMass
                                    body1.centerOfMassTransform3d
                                    body2.centerOfMassTransform3d
                                    constraint
                                    :: result
                            )
                            []
                            constraints
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
