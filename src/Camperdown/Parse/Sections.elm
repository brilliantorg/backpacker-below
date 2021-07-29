module Camperdown.Parse.Sections exposing (Section, splitIntoSections)

import Camperdown.Loc as Loc
import Camperdown.Parse.Line as Line exposing (Line, Lines)
import Camperdown.Parse.Syntax as Syn
import Camperdown.Util as Util



{- The goal here is to split a document into sections: the prelude section at the beginning
   (which might be the whole document), and subsequent sections that have headings: non-indented
   lines that start with a `#` character.

   See PARSE.md for more.
-}


type alias Section =
    { level : Int
    , label : Syn.Label
    , problems : List Loc.Location
    , contents : Lines
    }


{-| Split a document into sections.
-}
splitIntoSections : String -> { prelude : Lines, sections : List Section }
splitIntoSections str =
    let
        lines =
            String.lines str
                |> Util.oneIndexedMap getIndent

        ( prelude, rest ) =
            getSectionContent Nothing lines

        sections =
            getAllSections [] rest
    in
    { prelude = prelude, sections = sections }


getIndent : Int -> String -> Line
getIndent n str =
    captureWhitespace n 0 (String.toList str)


captureWhitespace : Int -> Int -> List Char -> Line
captureWhitespace lineNum indent chars =
    case chars of
        [] ->
            { lineNum = lineNum, contents = Nothing }

        ' ' :: rest ->
            captureWhitespace lineNum (indent + 1) rest

        '\t' :: rest ->
            -- Ugh I hate tabs, maybe change this behavior?
            captureWhitespace lineNum (indent + 1) rest

        _ ->
            { lineNum = lineNum, contents = Just { indent = indent, str = String.fromList chars } }


getSectionContent : Lines -> List Line -> ( Lines, Lines )
getSectionContent accum lines =
    case lines of
        [] ->
            ( Line.reverse accum, Nothing )

        line :: rest ->
            case Maybe.map (\{ indent, str } -> ( indent, String.uncons str )) line.contents of
                Nothing ->
                    getSectionContent (Line.consNothing accum line.lineNum) rest

                Just ( _, Nothing ) ->
                    -- Impossible, if the string was empty the whole thing would be Nothing
                    getSectionContent (Line.consNothing accum line.lineNum) rest

                Just ( indent, Just ( ch, str ) ) ->
                    if indent == 0 && ch == '#' then
                        -- A new section starts here! Don't include this line
                        ( Line.reverse accum
                        , Just
                            ( { lineNum = line.lineNum
                              , contents = { indent = indent, str = String.cons ch str }
                              }
                            , rest
                            )
                        )

                    else
                        -- Append this to the section
                        getSectionContent (Line.consJust accum line.lineNum indent (String.cons ch str)) rest


getAllSections : List Section -> Lines -> List Section
getAllSections accum lines =
    case lines of
        Nothing ->
            List.reverse accum

        Just ( { lineNum, contents }, rest ) ->
            -- By invariant, we know that contents.str starts with a `#` character,
            -- and that contents.indent is 0.
            let
                ( level, label, problems ) =
                    getLabelLine lineNum 0 (String.toList contents.str)

                ( passageContents, laterPassages ) =
                    getSectionContent Nothing rest

                section =
                    { level = level
                    , label = label
                    , contents = passageContents
                    , problems = problems
                    }
            in
            getAllSections (section :: accum) laterPassages


getLabelLine : Int -> Int -> List Char -> ( Int, Syn.Label, List Loc.Location )
getLabelLine lineNum level chars =
    case chars of
        [] ->
            ( level, Syn.Anonymous lineNum, [] )

        '#' :: rest ->
            getLabelLine lineNum (level + 1) rest

        ch :: _ ->
            case String.trim (String.fromList chars) of
                "" ->
                    ( level, Syn.Anonymous lineNum, [] )

                label ->
                    ( level
                    , Syn.Named <|
                        Loc.locate
                            { line = lineNum, column = 1 }
                            { line = lineNum, column = level + 1 + List.length chars }
                            label
                    , if ch == ' ' then
                        []

                      else
                        -- It's an error for the `#` characters to not be followed by whitespace
                        -- mark this as a problem for tracking.
                        [ { start = { line = lineNum, column = level + 1 }
                          , end = { line = lineNum, column = level + 2 }
                          }
                        ]
                    )
