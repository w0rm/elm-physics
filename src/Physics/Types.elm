module Physics.Types exposing (Body(..), Contacts(..), Constraint, Material(..), Shape(..))

import Dict exposing (Dict)
import Internal.Body as InternalBody
import Internal.Constraint as InternalConstraint
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Length exposing (Meters)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Point3d exposing (Point3d)


type Body
    = Body InternalBody.Body


type Contacts id
    = Contacts
        { lambdas : Dict String Float
        , iterations : Int
        , contactPoints : List ( id, id, List (Point3d Meters WorldCoordinates) )
        }


type alias Constraint =
    InternalConstraint.Constraint BodyCoordinates


type Material kind
    = Material InternalMaterial.Material


type Shape
    = Shape (List ( InternalShape.Shape BodyCoordinates, Float ))
