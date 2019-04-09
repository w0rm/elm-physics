module Physics.World exposing
    ( World, empty, add, setGravity
    , simulate, getBodies
    , keepIf, update
    )

{-|

@docs World, empty, add, setGravity

@docs simulate, getBodies

@docs keepIf, update

-}

import Internal.Body as InternalBody
import Internal.Const as Const
import Internal.NarrowPhase as NarrowPhase
import Internal.Solver as Solver
import Internal.Vector3 as Vec3
import Internal.World as Internal exposing (Protected(..))
import Physics.Body exposing (Body)


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
        , freeIds = []
        , nextBodyId = 0
        , gravity = Const.zero3
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
        |> Internal.tick (dt / 1000)
        |> Protected


{-| Get all bodies from the world.

Use this to convert bodies into visual representation,
e.g. WebGL entities.

-}
getBodies : World data -> List (Body data)
getBodies (Protected { bodies }) =
    List.map InternalBody.Protected bodies



-- Extra


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
