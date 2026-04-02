module Physics.Types exposing (Body(..), Constraint, Material(..), Shape(..))

import Internal.Body as InternalBody
import Internal.Constraint as InternalConstraint
import Internal.Material as InternalMaterial
import Internal.Shape as InternalShape
import Physics.Coordinates exposing (BodyCoordinates)


type Body
    = Body InternalBody.Body


type alias Constraint =
    InternalConstraint.Constraint BodyCoordinates


type Material kind
    = Material InternalMaterial.Material


type Shape
    = Shape (List ( InternalShape.Shape BodyCoordinates, Float ))
