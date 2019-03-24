module Physics.World exposing
    ( World, empty, add
    , simulate, getBodies
    , filter, map
    )

{-|

@docs World, empty, setGravity, add

@docs simulate, getBodies

@docs filter, map

-}

import AltMath.Vector3 as Vec3 exposing (Vec3)
import Physics.Body exposing (Body)


{-| Physical world is our abstract playground for physical simulations.
-}
type World data
    = World


{-| Let be the new world!
-}
empty : World data
empty =
    World


{-| Set the [gravity](https://en.wikipedia.org/wiki/Standard_gravity), e.g.:

    planetEarth =
        setGravity { x = 0, y = 0, z = -9.80665 } world

-}
setGravity : Vec3 -> World data -> World data
setGravity _ =
    identity


{-| You can also add bodies to the world.

    worldWithFloor =
        add planeBody world

Check the [Physics.Body](Physics-Body) module for possible bodies.

-}
add : Body data -> World data -> World data
add _ _ =
    World


{-| Simulate the world, given the time delta since the last frame.

Call this function on a message from the [onAnimationFrame](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame) subscription.

-}
simulate : Float -> World data -> World data
simulate _ =
    identity


{-| Get all bodies from the world.

Use this to convert bodies into visual representation,
e.g. [WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL) entities.

-}
getBodies : World data -> List (Body data)
getBodies _ =
    []



-- Extra


{-| Keep bodies that satisfy the test.
-}
filter : (Body data -> Bool) -> World data -> World data
filter _ =
    identity


{-| Apply a function to every body in the world.
-}
map : (Body data -> Body data) -> World data -> World data
map _ =
    identity



-- Future


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
