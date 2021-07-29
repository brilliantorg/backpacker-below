module Camperdown.Config.Build exposing
    ( annotation
    , badAnnotation
    , build
    , buildSomething
    , buildWithReordering
    , build_
    , minimal
    , parserConfigFromResult
    , reOrder
    , unionOfSetList
    )

import Camperdown.Config.Config exposing (ParserConfig)
import Camperdown.Occurs exposing (Occurs(..))
import List.Extra
import Set exposing (Set)


{-| Used in the build process to carry state.
-}
type alias ParserConfigObject =
    { pc : ParserConfig, errors : List String, order : Int, reOrder : List Int }


{-| Attempt to build a parser config. Succeed with `Ok config`, faile with
`Err errorlList`.
-}
build : List (ParserConfigObject -> ParserConfigObject) -> Result (List String) ParserConfig
build builders =
    List.foldl (\builder config -> builder config) { pc = minimal, errors = [], order = -1, reOrder = [] } builders
        |> resultFromParserConfigObj


{-| dAn unsafe builder of parser configurations: error checking is ignored,
and so the configuation _may_ be invalid. Used for testing
-}
buildSomething : List (ParserConfigObject -> ParserConfigObject) -> ParserConfig
buildSomething builders =
    case build builders of
        Err _ ->
            minimal

        Ok pc ->
            pc



-- MAPPERS


resultFromParserConfigObj : ParserConfigObject -> Result (List String) ParserConfig
resultFromParserConfigObj pco =
    if pco.errors == [] then
        Ok pco.pc

    else
        Err pco.errors


parserConfigFromResult : Result (List String) ParserConfig -> ParserConfig
parserConfigFromResult result =
    case result of
        Ok pc ->
            pc

        Err _ ->
            minimal



-- BUILD


{-| Build a parser config with error messages. That is, if configObject.errors is
nonempty, then the config is invalid. We have yet to assure ourselves of the
whether the config if valid if configObject.errors is empty.
-}
build_ : List (ParserConfigObject -> ParserConfigObject) -> ParserConfigObject
build_ builders =
    List.foldl (\builder config -> builder config) { pc = minimal, errors = [], order = -1, reOrder = [] } builders



-- BUILD INSTRUCTIONS


badAnnotation : String -> String -> ParserConfigObject -> ParserConfigObject
badAnnotation startSymbol endSymbol configObj =
    if List.member startSymbol (startSymbols configObj.pc) then
        configObj

    else
        let
            pc =
                configObj.pc
        in
        { pc = { pc | annotationOpts = makeAnnotationOpt startSymbol endSymbol Never :: pc.annotationOpts }, errors = [], order = 1, reOrder = [] }


{-| Construct a builder from

    - a startSymbol
    - an endSymbol
    - a value of type Occurs: Never, Always, Sometimes

The partially applied function

    annotation startSymbol endSymbol occurs

is the constructed builder. It has type

    ParserConfigObject -> ParserConfigObject

and is used in a fold to construct a parser configuration
from `minimal`, which is a minimal parser config.

Note the `ifApply` pipeline. It checks the proposed
build for obvious errors. The `order: Int` field
of the `ParserConfigObject` is used to keep track of
the position of the builder in the **foldl** pipeline.
This information is used to reorder the build instructions
if that proves necessary.

-}
annotation : String -> String -> Occurs -> ParserConfigObject -> ParserConfigObject
annotation startSymbol endSymbol occurs configObj =
    case String.uncons startSymbol of
        Nothing ->
            { configObj | errors = "Could not get start character" :: configObj.errors }

        Just _ ->
            configObj
                |> (\co -> { co | order = co.order + 1 })
                |> ifApply (startSymbolIsInStartSymbols startSymbol) (transmitErrorOfExistingStartSymbol startSymbol)
                |> ifApply (prefixOfStartSymbolIsAlreadyInStartSymbols startSymbol) (transmitErrorOfStartSymbolPrefix startSymbol)
                |> ifApply (\_ -> True) (constructAnnotation startSymbol endSymbol occurs)


{-| Actually construct the builder. Used above in `annotation` in
the last stage of the `ifApply` pipeline.
-}
constructAnnotation : String -> String -> Occurs -> ParserConfigObject -> ParserConfigObject
constructAnnotation startSymbol endSymbol occurs configObj =
    let
        oldPc =
            configObj.pc

        newPC =
            { oldPc
                | annotationOpts = makeAnnotationOpt startSymbol endSymbol occurs :: oldPc.annotationOpts
                , annotationFirstChars = addToSet startSymbol oldPc.annotationFirstChars
                , meaningful = addToSet startSymbol oldPc.meaningful
            }
    in
    { configObj | pc = newPC, errors = configObj.errors }



-- ATTEMPT TO BUILD EVEN IF THE BUILDER LIST IS DEFECTIVE


{-| A first draft: apply one reordering if the build instructions are out-of-order.

TODO: this needs work (and thought!)

-}
buildWithReordering : List (ParserConfigObject -> ParserConfigObject) -> Result (List String) ParserConfig
buildWithReordering builders =
    let
        parserConfig =
            build_ builders

        newBuilders =
            reOrder parserConfig builders
    in
    build_ newBuilders |> resultFromParserConfigObj


{-| Reorder the build instructions: take the first offender
and put it at the head of the builder list.
-}
reOrder : ParserConfigObject -> List (ParserConfigObject -> ParserConfigObject) -> List (ParserConfigObject -> ParserConfigObject)
reOrder configObj builders =
    case List.head configObj.reOrder of
        Nothing ->
            builders

        Just k ->
            let
                ( first, second ) =
                    List.Extra.splitAt k builders
            in
            second ++ first


{-| If the test succeeds, return `transform a`, otherwise
return `a`.
-}
ifApply : (a -> Bool) -> (a -> a) -> a -> a
ifApply test transform a =
    if test a then
        transform a

    else
        a



-- CONFIGURATION VALIDITY TESTS


startSymbolIsInStartSymbols : String -> ParserConfigObject -> Bool
startSymbolIsInStartSymbols startSymbol configObj =
    List.member startSymbol (startSymbols configObj.pc)


prefixOfStartSymbolIsAlreadyInStartSymbols : String -> ParserConfigObject -> Bool
prefixOfStartSymbolIsAlreadyInStartSymbols startSymbol configObj =
    String.dropLeft 1 startSymbol /= "" && List.member (String.dropLeft 1 startSymbol) (startSymbols configObj.pc)



-- ERROR FORWARDING


transmitErrorOfExistingStartSymbol : String -> ParserConfigObject -> ParserConfigObject
transmitErrorOfExistingStartSymbol startSymbol configObj =
    { configObj | errors = ("The startSymbol |" ++ startSymbol ++ "| already exists.  You cannot add it twice.") :: configObj.errors, order = 0 }


transmitErrorOfStartSymbolPrefix : String -> ParserConfigObject -> ParserConfigObject
transmitErrorOfStartSymbolPrefix startSymbol configObj =
    { configObj
        | errors =
            ("A prefix |"
                ++ String.dropLeft 1 startSymbol
                ++ "| of the startSymbol "
                ++ startSymbol
                ++ " already exists.  This is not allowed.  Try putting the annotation with the longer start symbol before the shorter one."
            )
                :: configObj.errors
        , reOrder = configObj.order :: configObj.reOrder
    }



-- MINIMAL CONFIGURATION


startSymbols : ParserConfig -> List String
startSymbols config =
    List.map .startSymbol config.annotationOpts


makeAnnotationOpt : String -> String -> Occurs -> { startSymbol : String, endSymbol : Maybe String, commandOccursAfterwards : Occurs }
makeAnnotationOpt startSymbol endSymbol occurs =
    { startSymbol = startSymbol, endSymbol = Just endSymbol, commandOccursAfterwards = occurs }


automaticallyMeaningful : Set Char
automaticallyMeaningful =
    Set.fromList [ '\\', ']', '\n' ]


automaticallyEscapable : Set Char
automaticallyEscapable =
    Set.fromList [ '\\', '[', ']', '!', '?', ':', '(', ')' ]


standardVerbatimMarkers : List String
standardVerbatimMarkers =
    [ "%%%", "$$$", "```" ]


basicAnnotationFirstChars : Set Char
basicAnnotationFirstChars =
    Set.fromList [ '"', '[' ]


basicVerbatimOpts : Set Char
basicVerbatimOpts =
    Set.fromList [ '`', '$' ]


minimal : ParserConfig
minimal =
    { verbatimOpts = basicVerbatimOpts
    , annotationOpts =
        []
    , annotationFirstChars = basicAnnotationFirstChars
    , meaningful = automaticallyMeaningful
    , escapable =
        unionOfSetList
            [ automaticallyEscapable
            , firstChars standardVerbatimMarkers
            , basicAnnotationFirstChars
            , basicVerbatimOpts
            , firstChars standardVerbatimMarkers
            ]
    , verbatimMarkers = standardVerbatimMarkers
    }



-- HELPERS


addToSet : String -> Set Char -> Set Char
addToSet startSymbol annotationFirstChars =
    case String.uncons startSymbol of
        Just ( c, _ ) ->
            Set.insert c annotationFirstChars

        Nothing ->
            annotationFirstChars


firstChars : List String -> Set Char
firstChars set =
    set
        |> List.map firstChar
        |> Maybe.Extra.values
        |> Set.fromList


firstChar : String -> Maybe Char
firstChar str =
    String.uncons str
        |> Maybe.map Tuple.first


unionOfSetList : List (Set comparable) -> Set comparable
unionOfSetList sets =
    List.foldl (\set acc -> Set.union set acc) Set.empty sets
