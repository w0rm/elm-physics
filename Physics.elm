module Physics
    exposing
        ( step
        , foldl
        , contacts
        , World
        , world
        , setGravity
        , addBody
        , Body
        , body
        , BodyId
        , setMass
        , addShape
        , rotateBy
        , offsetBy
        , Shape
        , ShapeId
        , box
        , plane
        , sphere
        )

{-| Highly experimental toy physics engine in Elm.

The API is currently shaping up and will be most likely changed.


## World

@docs World, world, setGravity, addBody, BodyId


## Body

@docs Body, body, setMass, addShape, rotateBy, offsetBy, ShapeId


## Shape

@docs Shape, box, plane, sphere


## Physics

@docs step, foldl, contacts

-}

import Physics.World as World
import Physics.Body as Body exposing (Body, BodyId)
import Physics.Shape as Shape exposing (Shape, ShapeId)
import Physics.NarrowPhase as NarrowPhase exposing (..)
import Physics.Solver as Solver exposing (..)
import Physics.Quaternion as Quaternion
import Time exposing (Time)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Dict
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.ConvexPolyhedron as ConvexPolyhedron


{-| -}
type World
    = World World.World


{-| Let be the new world!
-}
world : World
world =
    World World.world


{-| Call it to set the world's gravity,
cause by default it's an open space

    world
        |> setGravity (vec3 0 0 -10)

-}
setGravity : Vec3 -> World -> World
setGravity gravity (World world) =
    World (World.setGravity gravity world)


{-| You can also add bodies to the world
-}
addBody : Body -> World -> ( World, BodyId )
addBody (Body body) (World world) =
    ( World (World.addBody body world)
    , World.getNextBodyId world
    )


{-| Pass-through definition for convenience of importers
-}
type alias BodyId =
    Body.BodyId


{-| Pass-through definition for convenience of importers
-}
type alias ShapeId =
    Shape.ShapeId


{-| Body is a solid matter without any moving parts
-}
type Body
    = Body Body.Body


{-| This gives you an empty body, add shapes to turn it into something meaningful
-}
body : Body
body =
    Body Body.body


{-| After creating a body, you have to give it a mass. Bodies without mass don't move!
-}
setMass : Float -> Body -> Body
setMass mass (Body body) =
    Body (Body.setMass mass body)


{-| Adds [shapes](#Shape) to the body. For example, call this to add a box:

    body
        |> addShape (box (vec3 1 1 1))

-}
addShape : Shape -> Body -> ( Body, ShapeId )
addShape (Shape shape) (Body body) =
    ( Body (Body.addShape shape body)
    , Body.getNextShapeId body
    )


{-| Rotate the body around the axis by a specific angle from its current orientation
-}
rotateBy : Vec3 -> Float -> Body -> Body
rotateBy axis angle (Body body) =
    Body
        { body
            | quaternion =
                Quaternion.mul
                    (Quaternion.fromAngleAxis angle axis)
                    body.quaternion
        }


{-| Move the body from its current position
-}
offsetBy : Vec3 -> Body -> Body
offsetBy offset (Body body) =
    Body { body | position = Vec3.add offset body.position }


{-| Shape is an integral part of a body. It is positioned in the body's coordinate system.

We support two types of shapes, a [box](#box) and a [plane](#plane).

-}
type Shape
    = Shape Shape.Shape


{-| A box shape defined by its half extends. To create a 2x2 box, call this:

    box (vec3 1 1 1)

-}
box : Vec3 -> Shape
box halfExtends =
    Shape (Shape.Convex (ConvexPolyhedron.fromBox halfExtends))


{-| A sphere shape defined by its radius.
-}
sphere : Float -> Shape
sphere radius =
    { radius = radius }
        |> Shape.Sphere
        |> Shape


{-| A plane shape, with a normal pointing in the direction of the z axis
-}
plane : Shape
plane =
    Shape Shape.Plane


{-| Simulate the world, given the time delta since the last step.

Call this function on a message from the AnimationFrame subscription!

-}
step : Time -> World -> World
step dt (World world) =
    world
        |> World.addGravityForces
        |> Solver.solve dt (NarrowPhase.getContacts world)
        |> World.tick dt
        |> World


{-| Fold over each shape in the world. Use this to convert shapes into visual representation, e.g. WebGL entities.

    entities =
        foldl
        ({ transform, bodyId, shapeId } currentEntities ->
            makeEntity transform bodyId shapeId :: currentEntities
        )
        []
        world

  - `transform` is a transformation matrix that offsets and rotates a shape into the world coorditantes

  - `bodyId` is an id of a body the shape belongs to, each new body in the world gets and id starting from 0

  - `shapeId` is an id of the shape, each new shape in a body gets and id starting from 0

Use `bodyId` and `shapeId` to link additional information from the outside of the engine, e.g. which mesh to render
or what color should this shape be.

-}
foldl : ({ transform : Mat4, bodyId : BodyId, shapeId : ShapeId } -> a -> a) -> a -> World -> a
foldl fn acc (World { bodies }) =
    Dict.foldl
        (\bodyId { position, quaternion, shapes, shapeTransforms } acc1 ->
            Dict.foldl
                (\shapeId shape ->
                    fn
                        { transform =
                            case Dict.get shapeId shapeTransforms of
                                Just transform ->
                                    (Quaternion.toMat4 transform.quaternion)
                                        |> Mat4.mul (Mat4.makeTranslate transform.position)
                                        |> Mat4.mul (Quaternion.toMat4 quaternion)
                                        |> Mat4.mul (Mat4.makeTranslate position)

                                Nothing ->
                                    Mat4.mul
                                        (Mat4.makeTranslate position)
                                        (Quaternion.toMat4 quaternion)
                        , shapeId = shapeId
                        , bodyId = bodyId
                        }
                )
                acc1
                shapes
        )
        acc
        bodies


{-| Get the contact points in the world for visual debugging
-}
contacts : World -> List Vec3
contacts (World ({ bodies } as world)) =
    List.foldl
        (\{ bodyId1, bodyId2, ri, rj } ->
            [ ( bodyId1, ri )
            , ( bodyId2, rj )
            ]
                |> List.filterMap
                    (\( bodyId, r ) ->
                        Maybe.map (.position >> Vec3.add r) (Dict.get bodyId bodies)
                    )
                |> (++)
        )
        []
        -- TODO: maybe cache the previous contacts in the world
        (NarrowPhase.getContacts world)
