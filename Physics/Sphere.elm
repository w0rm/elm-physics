module Physics.Sphere
    exposing
        ( Sphere
        , boundingRadius
        )

import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Transform exposing (Transform)


type alias Sphere =
    { radius : Float
    }


boundingRadius : Float -> Transform -> Float 
boundingRadius radius transform = 
    transform.position
        |> Vec3.length
        |> (+) radius

{- Unused methods translated from cannon.js
   calculateLocalInertia : Sphere -> Float -> Vec3
   calculateLocalInertia { radius } mass =
       let
           scale =
               0.4 * mass * radius * radius
       in
           (vec3 scale scale scale)


   volume : SphereShape -> Float
   volume { radius } =
       pi / 0.75 * radius
-}
