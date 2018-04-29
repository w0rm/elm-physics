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
        , setMass
        , addShape
        , rotateBy
        , offsetBy
        , Shape
        , box
        , plane
        )

{-|


# Physics

Higlhy experimental toy physics engine in Elm.


## World

@docs World, world, setGravity, addBody


## Body

@docs Body, body, addShapes


## Shape

@docs Shape, box, plane


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


{-| The world is the main state
-}
type World
    = World World.World


{-| Let be the world
-}
world : World
world =
    World World.world


{-| You can change the world's gravity
-}
setGravity : Vec3 -> World -> World
setGravity gravity (World world) =
    World (World.setGravity gravity world)


{-| You can also add bodies to the world
-}
addBody : Body -> World -> World
addBody (Body body) (World world) =
    World (World.addBody body world)


{-| Body is a solid thing that exists in the world
-}
type Body
    = Body Body.Body


{-| This gives you an empty body, add shapes to turn it into smth
-}
body : Body
body =
    Body Body.body


{-| When creating a body, you have to give it a mass. Bodies without mass don't move!
-}
setMass : Float -> Body -> Body
setMass mass (Body body) =
    Body (Body.setMass mass body)


{-| Adds [shapes](#Shape) to the body
-}
addShape : Shape -> Body -> Body
addShape (Shape shape) (Body body) =
    Body (Body.addShape shape body)


{-| Rotate the body around the axis
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


{-| Move the body
-}
offsetBy : Vec3 -> Body -> Body
offsetBy offset (Body body) =
    Body { body | position = Vec3.add offset body.position }


{-| Shape is an integral part of the body. It is positioned in the body's coordinate system.

We support two types of shapes, a [box](#box) and a [plane](#plane).

-}
type Shape
    = Shape Shape.Shape


{-| A box shape defined by its half extends
-}
box : Vec3 -> Shape
box halfExtends =
    Shape (Shape.Box halfExtends)


{-| A plane shape, with a normal pointing in the direction of the z axis
-}
plane : Shape
plane =
    Shape Shape.Plane


{-| Simulate the world, given the time delta since the last step.
-}
step : Time -> World -> World
step dt (World world) =
    world
        |> World.addGravityForces
        |> Solver.solve dt (NarrowPhase.getContacts world)
        |> World.tick dt
        |> World


{-| Fold over each shape in the world. This is useful to convert shapes into WebGL entities.
-}
foldl : ({ transform : Mat4, bodyId : BodyId, shapeId : ShapeId } -> a -> a) -> a -> World -> a
foldl fn acc (World { bodies }) =
    Dict.foldl
        (\bodyId { position, quaternion, shapes, shapeOffsets, shapeOrientations } acc1 ->
            Dict.foldl
                (\shapeId shape ->
                    fn
                        { transform =
                            Shape.transform shape
                                |> Mat4.mul (Quaternion.toMat4 (Maybe.withDefault Quaternion.identity (Dict.get shapeId shapeOrientations)))
                                |> Mat4.mul (Mat4.makeTranslate (Maybe.withDefault Body.zero3 (Dict.get shapeId shapeOffsets)))
                                |> Mat4.mul (Quaternion.toMat4 quaternion)
                                |> Mat4.mul (Mat4.makeTranslate position)
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
