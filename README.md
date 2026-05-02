# 3D Physics Engine

![elm-physics](https://unsoundscapes.com/elm-physics/examples/elm-physics.gif)

```elm
type Id = Ball | Floor


bodies : List ( Id, Body )
bodies =
    [ ( Ball
      , Physics.sphere
            (Sphere3d.atOrigin (Length.meters 0.5))
            Material.rubber
            |> Physics.moveTo (Point3d.meters 0 0 5)
      )
    , ( Floor, Physics.plane Plane3d.xy Material.wood )
    ]


step model =
    let
        ( newBodies, newContacts ) =
            Physics.simulate
                { onEarth | contacts = model.contacts }
                model.bodies
    in
    { model | bodies = newBodies, contacts = newContacts }
```

# Features

- **Pure** — `simulate` is a function, not a stateful world; deterministic, replayable, time-travel friendly.
- **Your list is the world** — bodies live in `List ( id, Body )`; add with `(::)`, remove with `List.filter`.
- **Type-safe coordinates and units** — built on [elm-geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/) and [elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/); phantom types keep `WorldCoordinates` and `BodyCoordinates` apart, and forces, masses, velocities, and durations all carry units.
- **Compound bodies** — combine shapes with `Shape.plus` / `minus` / `sum` and per-shape densities; mass, center of mass, and the full inertia tensor are derived for you.
- **Declarative constraints and collisions** — pass `constrain` and `collide` as functions to `simulate`; the engine asks per body-pair what applies, so there's no constraint registry and no filter-group API.
- **Warm-started solver** — feed last frame's contacts back into `simulate` for stable stacks.

# Examples

- Lack ([source](https://github.com/w0rm/elm-physics/tree/main/examples/src/Lack.elm), [demo](https://unsoundscapes.com/elm-physics/examples/lack/))
- Duckling ([source](https://github.com/w0rm/elm-physics/tree/main/examples/src/Duckling.elm), [demo](https://unsoundscapes.com/elm-physics/examples/duckling/))
- Raycast ([source](https://github.com/w0rm/elm-physics/tree/main/examples/src/Raycast.elm), [demo](https://unsoundscapes.com/elm-physics/examples/raycast/))
- RaycastCar ([source](https://github.com/w0rm/elm-physics/tree/main/examples/src/RaycastCar.elm), [demo](https://unsoundscapes.com/elm-physics/examples/raycast-car/))

# Prior Work

Inspired by [Cannon.js](https://github.com/schteppe/cannon.js) and [Bullet](https://github.com/bulletphysics/bullet3) — this project is an experiment in what a purely functional 3D physics engine can look like.
