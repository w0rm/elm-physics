module Physics.Types exposing (Body(..), Constraint, Contacts(..), Lock, Material(..), Shape(..))

import Array exposing (Array)
import Internal.Body as InternalBody
import Internal.Constraint as InternalConstraint
import Internal.Contact as InternalContact
import Internal.ContactCache exposing (ContactCache)
import Internal.Coordinates exposing (BodyCoordinates)
import Internal.Equation as Equation
import Internal.Lock as InternalLock
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape


type Body
    = Body InternalBody.Body


type Contacts id
    = Contacts
        { warmStart : ContactCache Equation.WarmStart
        , iterations : Int
        , pairGroups : List InternalContact.PairGroup
        , bodies : Array ( id, InternalBody.Body )
        }


type alias Constraint =
    InternalConstraint.Constraint BodyCoordinates


type alias Lock =
    InternalLock.Lock


type Material kind
    = Material InternalMaterial.Material


type Shape
    = Shape (List ( InternalShape.Shape BodyCoordinates, Float ))
