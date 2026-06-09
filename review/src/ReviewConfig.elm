module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
        -- These modules pad a constructor with () fields on purpose so it shares
        -- an object shape with its sibling, letting V8 read the `$` discriminator
        -- from one monomorphic hidden class. The args are meant to be unused.
        |> Review.Rule.ignoreErrorsForFiles
            [ "src/Internal/ContactCache.elm"
            , "src/Internal/VertexBuffer.elm"
            , "src/Shapes/Convex.elm"
            , "src/Collision/ConvexConvex.elm"
            , "src/Collision/CapsuleConvex.elm"
            ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule (Simplify.expectNaN Simplify.defaults)
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    ]
