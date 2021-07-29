module Camperdown.Config.Check exposing (check, isValid)

import Camperdown.Config.Config exposing (ParserConfig)
import Set


type alias ConfigResult =
    Result String String


{-| True if and only if the config satisies the rules.
-}
isValid : ParserConfig -> Bool
isValid config =
    [] == List.filter (\result -> not (isOk result)) (check config)


{-| 'check config' is a list of reports on whether the parser config satisfies the
rules: either (Err <reason why a rule is not satisfied) or
(Ok <short descrption of rule>).
-}
check : ParserConfig -> List ConfigResult
check config =
    List.map (\f -> f config) rules


rules : List (ParserConfig -> ConfigResult)
rules =
    [ rightBracketNotInAnnotationFirstChars
    , annotationStartSymbolsAppearInAnnotationFirstChars
    , annotationStartSymbolsAppearInMeaningfulCharacters
    ]



-- CONSISTENCY RULES
--escapablesAreUnionOfMeaningfulAndFirstCharsOfStarters : ParserConfig -> ConfigResult
--escapablesAreUnionOfMeaningfulAndFirstCharsOfStarters config =
--    config.escapable == Set.union config.meaningful


rightBracketNotInAnnotationFirstChars : ParserConfig -> ConfigResult
rightBracketNotInAnnotationFirstChars config =
    if Set.member ']' config.annotationFirstChars then
        Err "']' cannot be a member of annotationFirstChars"

    else
        Ok "annotationFirstChars does not contain ']'"


annotationStartSymbolsAppearInAnnotationFirstChars : ParserConfig -> ConfigResult
annotationStartSymbolsAppearInAnnotationFirstChars config =
    let
        annotationStartSymbols =
            List.map .startSymbol config.annotationOpts |> Set.fromList

        difference =
            Set.diff annotationStartSymbols (Set.map String.fromChar config.annotationFirstChars)

        differenceReport =
            difference |> Set.toList |> String.join ", "
    in
    if difference == Set.empty then
        Ok "All annotation start symbols appear in annotationFirstChars "

    else
        Err <| "These characters appear in 'annotationOpts' but not in 'annotationFirstChars': " ++ differenceReport


annotationStartSymbolsAppearInMeaningfulCharacters : ParserConfig -> ConfigResult
annotationStartSymbolsAppearInMeaningfulCharacters config =
    let
        annotationStartSymbols =
            List.map .startSymbol config.annotationOpts |> Set.fromList

        difference =
            Set.diff annotationStartSymbols (Set.map String.fromChar config.meaningful)

        differenceReport =
            difference |> Set.toList |> String.join ", "
    in
    if difference == Set.empty then
        Ok "All annotation start symbols appear in the set of meaningful characters"

    else
        Err <| "These characters appear in 'annotationOpts' but not in 'meaningful': " ++ differenceReport



-- HELPERS


isOk : Result a b -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
