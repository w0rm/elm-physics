module Physics exposing (..)

import Physics.World as World exposing (World)
import Physics.NarrowPhase as NarrowPhase exposing (..)
import Physics.Solver as Solver exposing (..)
import Time exposing (Time)


step : Time -> World -> World
step dt world =
    world
        |> World.addGravityForces
        |> Solver.solve dt (NarrowPhase.getContacts world)
        |> World.tick dt
