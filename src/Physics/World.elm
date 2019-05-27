module Physics.World exposing
    ( World, empty, add, setGravity
    , simulate, getBodies, raycast, RaycastResult
    , keepIf, update
    , constrain, constrainIf
    )

{-|

@docs World, empty, add, setGravity

@docs simulate, getBodies, raycast, RaycastResult

@docs keepIf, update

@docs constrain, constrainIf

-}

import Internal.Body as InternalBody
import Internal.Constraint exposing (ConstraintGroup)
import Internal.NarrowPhase as NarrowPhase
import Internal.Quaternion as Quaternion
import Internal.Solver as Solver
import Internal.Vector3 as Vec3
import Internal.World as Internal exposing (Protected(..))
import Physics.Body exposing (Body)
import Physics.Constraint exposing (Constraint)


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


{-| Adds a body to the world.

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
                    | bodies = { body | id = world.nextBodyId } :: world.bodies
                    , nextBodyId = world.nextBodyId + 1
                }

        freeId :: restFreeIds ->
            Protected
                { world
                    | bodies = { body | id = freeId } :: world.bodies
                    , freeIds = restFreeIds
                }


{-| Set the [standard gravity](https://en.wikipedia.org/wiki/Standard_gravity), e.g.:

    planetEarth =
        setGravity { x = 0, y = 0, z = -9.80665 } world

-}
setGravity : { x : Float, y : Float, z : Float } -> World data -> World data
setGravity gravity (Protected world) =
    Protected { world | gravity = gravity }


{-| Simulate the world, given the number of milliseconds since the last frame.

Call this function on a message from the [onAnimationFrame](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame) subscription.

-}
simulate : Float -> World data -> World data
simulate dt (Protected world) =
    world
        |> Internal.addGravityForces
        |> Solver.solve (dt / 1000) (NarrowPhase.getContacts world)
        |> Protected


{-| Get all bodies from the world.

Use this to convert bodies into visual representation,
e.g. WebGL entities.

-}
getBodies : World data -> List (Body data)
getBodies (Protected { bodies }) =
    List.map InternalBody.Protected bodies


{-| Find the closest intersection of a ray against
all the bodies in the world.
-}
raycast :
    { from : { x : Float, y : Float, z : Float }
    , direction : { x : Float, y : Float, z : Float }
    }
    -> World data
    -> Maybe (RaycastResult data)
raycast ray (Protected world) =
    case Internal.raycast { ray | direction = Vec3.normalize ray.direction } world of
        Just { body, point, normal } ->
            Just
                { body = InternalBody.Protected body

                -- convert into the local body coordinate system:
                , point = Quaternion.derotate body.orientation (Vec3.sub point body.position)
                , normal = Quaternion.derotate body.orientation normal
                }

        Nothing ->
            Nothing


{-| The Raycast result includes the intersected body,
intersection point and normal vector on the face,
expressed within the local body coordinate system.
-}
type alias RaycastResult data =
    { body : Body data
    , point : { x : Float, y : Float, z : Float }
    , normal : { x : Float, y : Float, z : Float }
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
                case ( (Body.getData b1).part, (Body.getData b2).part ) of
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
constrain : (Body data -> Body data -> List Constraint) -> World data -> World data
constrain =
    constrainIf (always True)


{-| Configure constraints for a subset of bodies that satisfy the test.

For the above example we can tag each part of a car with the `carId`,
and preselect parts of a single car with:

    constrainCar carId =
        constrainIf (\body -> (Body.getData body).carId == carId)

-}
constrainIf : (Body data -> Bool) -> (Body data -> Body data -> List Constraint) -> World data -> World data
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
                    , constraints = constraints
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
