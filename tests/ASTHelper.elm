module ASTHelper exposing (..)

import Camperdown.Config.Configurations exposing (markup)
import Camperdown.Parse exposing (parse)
import Camperdown.Parse.Syntax exposing (..)
import List.Extra
import Maybe.Extra


{-|

> parse markup "_foo_"
> Just (Annotation ({ end = { column = 2, line = 1 }, start = { column = 1, line = 1 } },"_") [Raw "foo"] (Just ({ end = { column = 6, line = 1 }, start = { column = 5, line = 1 } },"_")) Nothing)

-}
getAnnotationPipeline doc =
    doc
        |> .prelude
        |> List.map paragraphContents
        |> Maybe.Extra.values
        |> List.head
        |> Maybe.withDefault []
        |> List.head



-- ACCESSORS


{-|

    > parse markup "_foo_" |> getAnnotationPipeline |> Maybe.andThen annotationOfText1
      Just "\_" : Maybe String

-}
annotationComponent1 : Text -> Maybe String
annotationComponent1 text =
    case text of
        Annotation ( _, str ) _ _ _ ->
            Just str

        _ ->
            Nothing


annotationComponent2 : Text -> Maybe (List Text)
annotationComponent2 text =
    case text of
        Annotation _ t _ _ ->
            Just t

        _ ->
            Nothing


annotationComponent3 : Text -> Maybe String
annotationComponent3 text =
    case text of
        Annotation _ _ (Just ( _, str )) _ ->
            Just str

        _ ->
            Nothing


annotationComponent4 : Text -> Maybe Command
annotationComponent4 text =
    case text of
        Annotation _ _ _ (Just ( _, cmd )) ->
            Just cmd

        _ ->
            Nothing


preludeElement : Int -> { a | prelude : List Element } -> Maybe Element
preludeElement n =
    \x -> x.prelude |> List.Extra.getAt n


paragraphContents : Element -> Maybe Markup
paragraphContents element =
    case element of
        Paragraph { contents } ->
            Just contents

        _ ->
            Nothing


command : Element -> Maybe Element
command element =
    case element of
        Command data ->
            Just (Command data)

        _ ->
            Nothing


commandChild : Element -> Maybe Divert
commandChild element =
    case element of
        Command data ->
            data.child

        _ ->
            Nothing


itemChildren : Element -> Maybe (List Element)
itemChildren element =
    case element of
        Item data ->
            Just data.children

        _ ->
            Nothing


nestedElements : Divert -> List Element
nestedElements divert =
    case divert of
        Nested elements ->
            elements

        _ ->
            []



-- ELEMENT PREDICATES


isCommand : Element -> Bool
isCommand element =
    case element of
        Command _ ->
            True

        _ ->
            False


isParagraph : Element -> Bool
isParagraph element =
    case element of
        Paragraph _ ->
            True

        _ ->
            False


isPreformatted : Element -> Bool
isPreformatted element =
    case element of
        Preformatted _ ->
            True

        _ ->
            False



-- TEXT PREDICATES


isAnnotation : Text -> Bool
isAnnotation text =
    case text of
        Annotation _ _ _ _ ->
            True

        _ ->
            False


isRaw : Text -> Bool
isRaw text =
    case text of
        Raw _ ->
            True

        _ ->
            False


isVerbatim : Text -> Bool
isVerbatim text =
    case text of
        Verbatim _ _ ->
            True

        _ ->
            False


isInlineProblem : Text -> Bool
isInlineProblem text =
    case text of
        InlineProblem _ ->
            True

        _ ->
            False



-- OTHER


matchAnnotation : String -> String -> String -> Text -> Bool
matchAnnotation a b c txt =
    case txt of
        Annotation ( _, aa ) bb (Just ( _, cc )) _ ->
            a == aa && [ Raw b ] == bb && c == cc

        _ ->
            False


hasProblem : List Element -> Bool
hasProblem elements =
    List.map isProblem elements
        |> someAreTrue


isProblem : Element -> Bool
isProblem element =
    case element of
        Problem _ ->
            True

        _ ->
            False



-- BOOLEAN HELPERS


allAreTrue : List Bool -> Bool
allAreTrue values =
    List.foldl (\acc b -> acc && b) True values


someAreTrue : List Bool -> Bool
someAreTrue values =
    List.foldl (\acc b -> acc || b) True values
