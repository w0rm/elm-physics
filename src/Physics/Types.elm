module Physics.Types exposing (Body(..), Constraint, Contacts(..), Lock, Material(..), Shape(..))

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Body as InternalBody
import Internal.Constraint as InternalConstraint
import Internal.Contact as InternalContact
import Internal.Coordinates exposing (BodyCoordinates)
import Internal.Lock as InternalLock
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Internal.SolverBody as SolverBody
import Internal.Vector3 exposing (Vec3)


type Body
    = Body InternalBody.Body


type Contacts id
    = Contacts
        { lambdas : Dict String Float
        , iterations : Int
        , dt : Float
        , gravity : Vec3
        , contactGroups : List InternalContact.ContactGroup
        , solverBodies : Array (SolverBody.SolverBody id)
        }


type alias Constraint =
    InternalConstraint.Constraint BodyCoordinates


type alias Lock =
    InternalLock.Lock


type Material kind
    = Material InternalMaterial.Material


type Shape
    = Shape (List ( InternalShape.Shape BodyCoordinates, Float ))
