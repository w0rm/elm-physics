module Physics exposing
    ( World, world, setGravity, addBody, BodyId
    , Body, body, setMass, addShape, ShapeId, rotateBy, offsetBy
    , Shape, box, sphere, plane
    , step, fold
    , foldContacts, foldFaceNormals, foldUniqueEdges
    )

{-| Highly experimental toy physics engine in Elm.

The API is currently shaping up and will be most likely changed.


## World

@docs World, world, setGravity, addBody, BodyId


## Body

@docs Body, body, setMass, addShape, ShapeId, rotateBy, offsetBy


## Shape

@docs Shape, box, sphere, plane


## Physics

@docs step, fold


## Debug

The following functions are exposed for debugging.
Check the [demos](https://github.com/w0rm/elm-physics/blob/master/examples/Common/Demo.elm#L325) for usage details.

@docs foldContacts, foldFaceNormals, foldUniqueEdges

-}

import Dict
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Body as Body exposing (Body, BodyId)
import Physics.ConvexPolyhedron as ConvexPolyhedron
import Physics.NarrowPhase as NarrowPhase exposing (..)
import Physics.Quaternion as Quaternion
import Physics.Shape as Shape exposing (Shape, ShapeId)
import Physics.Solver as Solver exposing (..)
import Physics.World as World


{-| -}
type World
    = World World.World


{-| Let be the new world!
-}
world : World
world =
    World World.world


{-| Call it to set the world’s gravity,
cause by default it’s an open space:

    setGravity (vec3 0 0 -10) world
    --> worldWithGravity

-}
setGravity : Vec3 -> World -> World
setGravity gravity (World world_) =
    World (World.setGravity gravity world_)


{-| You can also add bodies to the world. The function
returns updated world and an id of the body.

    addBody body world
    --> (newWorld, bodyId)

-}
addBody : Body -> World -> ( World, BodyId )
addBody (Body body_) (World world_) =
    ( World (World.addBody body_ world_)
    , World.getNextBodyId world_
    )


{-| A unique identifier of a body in the world.
It can be used to link additional information, like meshes.
-}
type alias BodyId =
    Body.BodyId


{-| Body is a solid matter without any moving parts.
-}
type Body
    = Body Body.Body


{-| This gives you an empty body, add shapes to turn it
into something meaningful.
-}
body : Body
body =
    Body Body.body


{-| After creating a body, you have to give it a mass.
Bodies without mass don’t move!

    setMass 5 boxBody
    --> heavyBoxBody

-}
setMass : Float -> Body -> Body
setMass mass (Body body_) =
    Body (Body.setMass mass body_)


{-| Adds a [shape](#Shape) at the center of the body
and returns a tuple of updated body and a shape id.
For example, call this to add a box:

    addShape (box (vec3 1 1 1)) body
    --> (newBody, shapeId)

-}
addShape : Shape -> Body -> ( Body, ShapeId )
addShape (Shape shape) (Body body_) =
    ( Body (Body.addShape shape body_)
    , Body.getNextShapeId body_
    )


{-| Same as BodyId, but for shapes within a body.
-}
type alias ShapeId =
    Shape.ShapeId


{-| Rotate the body around the axis by a specific angle
from its current orientation.

    rotateBy (vec3 1 0 0) pi body
    --> upsideDownBody

-}
rotateBy : Vec3 -> Float -> Body -> Body
rotateBy axis angle (Body body_) =
    Body
        { body_
            | quaternion =
                Quaternion.mul
                    (Quaternion.fromAngleAxis angle axis)
                    body_.quaternion
        }


{-| Move the body from its current position.

    offsetBy (vec3 0 0 12) body
    --> elevatedBody

-}
offsetBy : Vec3 -> Body -> Body
offsetBy offset (Body body_) =
    Body { body_ | position = Vec3.add offset body_.position }


{-| Shape is an integral part of a body. It is positioned in the
body’s coordinate system.

We support the following shapes:

  - [box](#box),
  - [sphere](#sphere),
  - [plane](#plane).

-}
type Shape
    = Shape Shape.Shape


{-| A box shape is defined by its half extends.
To create a 2x2 box, call this:

    box (vec3 1 1 1)

-}
box : Vec3 -> Shape
box halfExtends =
    Shape (Shape.Convex (ConvexPolyhedron.fromBox halfExtends))


{-| A sphere shape is defined by its radius.
-}
sphere : Float -> Shape
sphere radius =
    Shape (Shape.Sphere radius)


{-| A plane shape, with a normal pointing in the direction of the z axis.
-}
plane : Shape
plane =
    Shape Shape.Plane


{-| Simulate the world, given the time delta since the last step.

Call this function on a message from the [onAnimationFrame](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame) subscription!

-}
step : Float -> World -> World
step dt (World world_) =
    world_
        |> World.addGravityForces
        |> Solver.solve dt (NarrowPhase.getContacts world_)
        |> World.tick dt
        |> World


{-| Fold over each shape in the world. Use this to convert shapes into visual representation, e.g. WebGL entities.

    entities =
        fold
        ({ transform, bodyId, shapeId } currentEntities ->
            makeEntity transform bodyId shapeId :: currentEntities
        )
        []
        world

  - `transform` is a transformation matrix that offsets and rotates a shape into the world coorditantes

  - `bodyId` is an id of the body the shape belongs to

  - `shapeId` is an id of the shape within the body

Use `bodyId` and `shapeId` to link additional information from the outside of the engine, e.g. which mesh to render
or what color should this shape be.

-}
fold : ({ transform : Mat4, bodyId : BodyId, shapeId : ShapeId } -> a -> a) -> a -> World -> a
fold fn acc world_ =
    foldOverShapes
        (\transform bodyId _ shapeId _ tail ->
            fn
                { transform = transform
                , bodyId = bodyId
                , shapeId = shapeId
                }
                tail
        )
        acc
        world_


{-| Fold over the contact points in the world.
-}
foldContacts : (Vec3 -> a -> a) -> a -> World -> a
foldContacts fn acc (World ({ bodies } as world_)) =
    List.foldl
        (\{ bodyId1, bodyId2, ri, rj } acc1 ->
            [ ( bodyId1, ri )
            , ( bodyId2, rj )
            ]
                |> List.filterMap
                    (\( bodyId, r ) ->
                        Maybe.map (.position >> Vec3.add r) (Dict.get bodyId bodies)
                    )
                |> List.foldl fn acc1
        )
        acc
        -- TODO: maybe cache the previous contacts in the world
        (NarrowPhase.getContacts world_)


foldOverShapes : (Mat4 -> BodyId -> Body.Body -> ShapeId -> Shape.Shape -> a -> a) -> a -> World -> a
foldOverShapes fn acc (World { bodies }) =
    Dict.foldl
        (\bodyId ({ position, quaternion, shapes, shapeTransforms } as body_) acc1 ->
            Dict.foldl
                (\shapeId shape acc2 ->
                    fn
                        (case Dict.get shapeId shapeTransforms of
                            Just transform ->
                                Quaternion.toMat4 transform.quaternion
                                    |> Mat4.mul (Mat4.makeTranslate transform.position)
                                    |> Mat4.mul (Quaternion.toMat4 quaternion)
                                    |> Mat4.mul (Mat4.makeTranslate position)

                            Nothing ->
                                Mat4.mul
                                    (Mat4.makeTranslate position)
                                    (Quaternion.toMat4 quaternion)
                        )
                        bodyId
                        body_
                        shapeId
                        shape
                        acc2
                )
                acc1
                shapes
        )
        acc
        bodies


{-| Fold over all the face normals in the world,
where each face normal consists of a normal vector for a face
and a reference point within the face.
These are both expressed within the local shape coordinate system.
The transform to the current world coordinates from the shape coordinates is also provided.
-}
foldFaceNormals : (Mat4 -> Vec3 -> Vec3 -> a -> a) -> a -> World -> a
foldFaceNormals fn acc world_ =
    foldOverShapes
        (\transform _ _ _ shape acc1 ->
            case shape of
                Shape.Plane ->
                    acc1

                Shape.Sphere _ ->
                    acc1

                Shape.Convex convex ->
                    ConvexPolyhedron.foldFaceNormals
                        (fn transform)
                        acc1
                        convex
        )
        acc
        world_


{-| Fold over all the unique edges in the world, where each unique edge
consists of a unit direction vector that runs parallel to an edge of a
face in the shape.
A vertex point of the shape and a transform to the current world coordinates
from the shape coordinates is also provided for context. Both the vector and
the point are expressed within the local shape coordinate system.
-}
foldUniqueEdges : (Mat4 -> Vec3 -> Vec3 -> a -> a) -> a -> World -> a
foldUniqueEdges fn acc world_ =
    foldOverShapes
        (\transform _ _ _ shape acc1 ->
            case shape of
                Shape.Plane ->
                    acc1

                Shape.Sphere _ ->
                    acc1

                Shape.Convex convex ->
                    ConvexPolyhedron.foldUniqueEdges
                        (fn transform)
                        acc1
                        convex
        )
        acc
        world_
