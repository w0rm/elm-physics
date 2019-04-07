module Physics.World exposing
    ( World, empty, setGravity, add
    , simulate, getBodies
    , keepIf, update
    )

{-|

@docs World, empty, setGravity, add

@docs simulate, getBodies

@docs keepIf, update

-}

import Internal.Body as InternalBody
import Internal.Const as Const
import Internal.NarrowPhase as NarrowPhase
import Internal.Solver as Solver
import Internal.Vector3 as Vec3 exposing (Vec3)
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


{-| Set the [gravity](https://en.wikipedia.org/wiki/Standard_gravity), e.g.:

    planetEarth =
        setGravity { x = 0, y = 0, z = -9.80665 } world

-}
setGravity : Vec3 -> World data -> World data
setGravity gravity (Protected world) =
    Protected { world | gravity = gravity }


{-| You can also add bodies to the world.

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



{- Future

   type Joint
       = Joint

   type Constraint
       = Constraint

   {-| Join two bodies with a constraint.
   -}
   join : Joint -> (Body data -> Bool) -> (Body data -> Bool) -> World data -> World data
   join _ _ _ =
       identity

   {-| The opposite of join.
   -}
   detach : (Body data -> Bool) -> (Body data -> Bool) -> World data -> World data
   detach _ _ =
       identity


   {-| Reset all joints in the world.
   -}
   setJoints : (Body data -> Body data -> Maybe Joint) -> World data -> World data
   setJoints _ =
       identity

   {-| -}
   addConstraint : (Body data -> Bool) -> Constraint -> World data -> World data
   addConstraint _ _ =
       identity
-}
