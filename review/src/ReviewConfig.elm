module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoEmptyText
import NoUnsafePorts
import NoUnusedPorts
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoFloatIds
import NoMissingDocumentation
import NoMissingSubscriptionsCall
import NoRecursiveUpdate
import NoUselessSubscriptions
import NoMissingTypeConstructor
import NoInconsistentAliases
import NoModuleOnExposedNames
import NoRedundantConcat
import NoRegex
import NoSinglePatternCase
import NoTypeAliasConstructorCall
import NoUnmatchedUnit
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoAlways
import Documentation.ReadmeLinksPointToCurrentVersion
import Docs.NoMissing exposing (exposedModules, onlyExposed)
import NoPrimitiveTypeAlias

config : List Rule
config =
    [ 
    NoDuplicatePorts.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoExposingEverything.rule
    , NoImportingEverything.rule ["Html"]
    , NoMissingTypeAnnotation.rule 
    , NoFloatIds.rule
    , NoRecursiveUpdate.rule 
    , NoInconsistentAliases.config
    []
    |> NoInconsistentAliases.noMissingAliases
    |> NoInconsistentAliases.rule 
    , NoModuleOnExposedNames.rule 
    , NoRedundantConcat.rule
    , NoRegex.rule
    , NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument 
    , NoTypeAliasConstructorCall.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule 
    , NoUnused.Modules.rule 
    , NoUnused.Parameters.rule 
    , NoUnused.Patterns.rule
    , NoPrimitiveTypeAlias.rule
    ]
    |> List.map (Review.Rule.ignoreErrorsForDirectories [ "tests/" ])