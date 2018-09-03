module Common.Bodies exposing
    ( DemoBody(..)
    , getBody
    , getMesh
    , getWireframe
    )

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltPhysics
import Common.Meshes as Meshes exposing (Attributes)
import WebGL exposing (Entity, Mesh, Shader)


type DemoBody
    = DemoBox
    | DemoSphere


getBody : DemoBody -> (AltPhysics.Body -> AltPhysics.Body) -> ( DemoBody, AltPhysics.Body )
getBody demoBody fn =
    case demoBody of
        DemoBox ->
            ( DemoBox, fn box )

        DemoSphere ->
            ( DemoSphere, fn sphere )


getMesh : DemoBody -> Mesh Attributes
getMesh demoBody =
    case demoBody of
        DemoBox ->
            boxMesh

        DemoSphere ->
            sphereMesh


getWireframe : DemoBody -> Mesh Attributes
getWireframe demoBody =
    case demoBody of
        DemoBox ->
            boxWireframe

        DemoSphere ->
            sphereWireframe



-- BOX


boxHalfExtends : Vec3
boxHalfExtends =
    vec3 1 1 1


boxMesh : Mesh Attributes
boxMesh =
    Meshes.makeBox boxHalfExtends


boxWireframe : Mesh Attributes
boxWireframe =
    Meshes.makeBoxWireframe boxHalfExtends


{-| A constant cube-shaped body with unit sides and mass of 5
-}
box : AltPhysics.Body
box =
    AltPhysics.body
        |> AltPhysics.setMass 5
        |> AltPhysics.addShape (AltPhysics.box boxHalfExtends)
        |> Tuple.first



-- SPHERE


sphereRadius : Float
sphereRadius =
    1.2


sphereMesh : Mesh Attributes
sphereMesh =
    Meshes.makeSphere 2 sphereRadius


sphereWireframe : Mesh Attributes
sphereWireframe =
    Meshes.makeSphereWireframe 2 sphereRadius


{-| A constant cube-shaped body with unit sides and mass of 5
-}
sphere : AltPhysics.Body
sphere =
    AltPhysics.body
        |> AltPhysics.setMass 5
        |> AltPhysics.addShape (AltPhysics.sphere sphereRadius)
        |> Tuple.first
