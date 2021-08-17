module Camperdown.Parse.Pieces exposing (Ending(..), Piece, PieceType(..), pieces)

import Camperdown.Config exposing (Config)
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Parse.Line as Line exposing (FirstLine, Line, Lines)
import Camperdown.Parse.Parse as Parse
import Camperdown.Parse.Syntax as Syn
import Parser.Advanced



{- The goal here is to split a section (see Sections.elm) into pieces, and feed each
   piece through the Elm Parser.Advanced parser separately.

   See PARSE.md for details
-}


type Ending
    = EndsWithoutDivert
    | EndsImmediate Loc.Location Syn.Markup
    | EndsNamed (Loc String)
    | EndsNested Syn.Markup


type PieceType
    = Paragraph Syn.Markup
    | Item Loc.Location Syn.Markup
    | Command (Loc Syn.Mark) Syn.Command Ending
    | Preformatted String
    | Problem
        { errorLocation : Loc.Location
        , errorMessage : String
        }


type alias Piece =
    { lines : { start : Int, end : Int }
    , indent : Int
    , piece : PieceType
    }


pieces : Config -> Lines -> List Piece
pieces =
    accumPieces []


accumPieces : List Piece -> Config -> Lines -> List Piece
accumPieces accum config maybeLines =
    case maybeLines of
        Nothing ->
            List.reverse accum

        Just lines ->
            let
                ( piece, rest ) =
                    getNextPiece config lines
            in
            accumPieces (piece :: accum) config rest


isMarked : Config -> String -> Basics.Bool
isMarked config str =
    String.startsWith "!" str
        || String.startsWith "?" str
        || String.startsWith ":" str
        || String.startsWith "#" str
        || isVerbatimMarked config str


isVerbatimMarked : Config -> String -> Bool
isVerbatimMarked config str =
    List.any (\prefix -> String.startsWith prefix str) config.verbatimMarkers


getNextPiece : Config -> ( FirstLine, List Line ) -> ( Piece, Lines )
getNextPiece config ( firstLine, lines ) =
    if String.startsWith "!" firstLine.contents.str then
        getCommandPiece config Syn.Bang firstLine lines

    else if String.startsWith "?" firstLine.contents.str then
        getCommandPiece config Syn.Huh firstLine lines

    else if String.startsWith ":" firstLine.contents.str then
        getItemPiece config firstLine lines

    else if String.startsWith "#" firstLine.contents.str then
        getProblemPiece config firstLine lines

    else if isVerbatimMarked config firstLine.contents.str then
        getVerbatimPiece config firstLine lines

    else
        getParagraphLines config firstLine lines


getCommandPiece : Config -> Syn.Mark -> FirstLine -> List Line -> ( Piece, Lines )
getCommandPiece config mark firstLine lines =
    let
        markLoc =
            { start = { line = firstLine.lineNum, column = firstLine.contents.indent + 1 }
            , end = { line = firstLine.lineNum, column = firstLine.contents.indent + 2 }
            }

        { location, withLeadingSpace, rest } =
            getPiece config False firstLine lines
    in
    ( { lines = { start = location.start.line, end = location.end.line }
      , indent = firstLine.contents.indent
      , piece =
            case Parser.Advanced.run (Parse.lineCommand config { line = firstLine.lineNum }) withLeadingSpace of
                Ok (Parse.CommandSimple cmd) ->
                    Command ( markLoc, mark ) cmd EndsWithoutDivert

                Ok (Parse.CommandWithDivert cmd label) ->
                    Command ( markLoc, mark ) cmd (EndsNamed label)

                Ok (Parse.CommandWithImmediateCont cmd loc markup) ->
                    Command ( markLoc, mark ) cmd (EndsImmediate loc markup)

                Ok (Parse.CommandWithNestedCont cmd markup) ->
                    Command ( markLoc, mark ) cmd (EndsNested markup)

                Ok (Parse.Er { errorLocation, errorMessage }) ->
                    Problem
                        { errorLocation = errorLocation
                        , errorMessage = errorMessage
                        }

                Err _ ->
                    Problem
                        { errorLocation = location
                        , errorMessage = "Unexpected parse error parsing a command. (That shouldn't happen!)"
                        }
      }
    , rest
    )


getItemPiece : Config -> FirstLine -> List Line -> ( Piece, Lines )
getItemPiece config firstLine lines =
    let
        { location, withLeadingSpace, rest } =
            getPiece config False firstLine lines
    in
    case Parser.Advanced.run (Parse.lineItem config { line = firstLine.lineNum }) withLeadingSpace of
        Ok markup ->
            ( { lines = { start = location.start.line, end = location.end.line }
              , indent = firstLine.contents.indent
              , piece =
                    Item
                        { start = { line = firstLine.lineNum, column = firstLine.contents.indent + 1 }
                        , end = { line = firstLine.lineNum, column = firstLine.contents.indent + 2 }
                        }
                        markup
              }
            , rest
            )

        Err _ ->
            ( { lines = { start = location.start.line, end = location.end.line }
              , indent = firstLine.contents.indent
              , piece =
                    Problem
                        { errorLocation = location
                        , errorMessage = "Unexpected parse error parsing a paragraph. (That shouldn't happen!)"
                        }
              }
            , rest
            )


getProblemPiece : Config -> FirstLine -> List Line -> ( Piece, Lines )
getProblemPiece config firstLine lines =
    let
        { location, rest } =
            getPiece config True firstLine lines
    in
    ( { lines = { start = location.start.line, end = location.end.line }
      , indent = firstLine.contents.indent
      , piece = Problem { errorLocation = location, errorMessage = "Indented lines cannot begin with the section divider `#`. Use a text escape `\\#` if you actually mean to start a line of text with this symbol." }
      }
    , rest
    )


getVerbatimPiece : Config -> FirstLine -> List Line -> ( Piece, Lines )
getVerbatimPiece config firstLine lines =
    let
        { location, withoutLeadingSpace, rest } =
            getPiece config True firstLine lines
    in
    ( { lines = { start = location.start.line, end = location.end.line }
      , indent = firstLine.contents.indent
      , piece = Preformatted withoutLeadingSpace
      }
    , rest
    )


getParagraphLines : Config -> FirstLine -> List Line -> ( Piece, Lines )
getParagraphLines config firstLine lines =
    let
        { location, withLeadingSpace, rest } =
            getPiece config False firstLine lines
    in
    case Parser.Advanced.run (Parse.paragraph config { line = firstLine.lineNum }) withLeadingSpace of
        Ok markup ->
            ( { lines = { start = location.start.line, end = location.end.line }
              , indent = firstLine.contents.indent
              , piece = Paragraph markup
              }
            , rest
            )

        Err _ ->
            ( { lines = { start = location.start.line, end = location.end.line }
              , indent = firstLine.contents.indent
              , piece =
                    Problem
                        { errorLocation = location
                        , errorMessage = "Unexpected parse error parsing a paragraph. (That shouldn't happen!)"
                        }
              }
            , rest
            )


{-| This is the central command of this whole algorithm: you've just encountered
a line with a marker. Now we need to follow the rules for what a piece is, and
find how many lines are in the same piece with this line.
-}
getPiece :
    Config
    -> Bool
    -> FirstLine
    -> List Line
    ->
        { location : Loc.Location
        , withLeadingSpace : String
        , withoutLeadingSpace : String
        , rest : Maybe ( FirstLine, List Line )
        }
getPiece config isVerbatim firstLine lines =
    let
        start =
            { line = firstLine.lineNum, column = firstLine.contents.indent + 1 }

        { end, pieceLines, rest } =
            getPieceLines
                { config = config
                , baseIndent =
                    if isMarked config firstLine.contents.str then
                        firstLine.contents.indent + 1

                    else
                        firstLine.contents.indent
                , isVerbatim = isVerbatim
                }
                lines
                { last =
                    { line = firstLine.lineNum
                    , column = firstLine.contents.indent + String.length firstLine.contents.str
                    }
                , accum = []
                }
    in
    { location = { start = start, end = end }
    , withLeadingSpace =
        linesToString 0 firstLine pieceLines
    , withoutLeadingSpace =
        linesToString firstLine.contents.indent firstLine pieceLines
    , rest = rest
    }


getPieceLines :
    { config : Config, baseIndent : Int, isVerbatim : Bool }
    -> List Line
    -> { last : Loc.Position, accum : List Line }
    -> { end : Loc.Position, pieceLines : List Line, rest : Maybe ( FirstLine, List Line ) }
getPieceLines ({ config, baseIndent, isVerbatim } as store) lines { last, accum } =
    let
        loop =
            getPieceLines store
    in
    case lines of
        [] ->
            { end = last, pieceLines = List.reverse accum, rest = Nothing }

        line :: rest ->
            case line.contents of
                Nothing ->
                    if isVerbatim then
                        loop rest { last = { line = line.lineNum, column = 1 }, accum = line :: accum }

                    else
                        { end = last, pieceLines = List.reverse accum, rest = Line.findNext rest }

                Just ({ indent, str } as contents) ->
                    if indent >= baseIndent && (isVerbatim || not (isMarked config str)) then
                        loop rest
                            { last =
                                { line = line.lineNum
                                , column = indent + String.length str
                                }
                            , accum = line :: accum
                            }

                    else
                        { end = last
                        , pieceLines = List.reverse accum
                        , rest = Just ( { lineNum = line.lineNum, contents = contents }, rest )
                        }


linesToString : Int -> FirstLine -> List Line -> String
linesToString baseIndent first rest =
    ((String.repeat (first.contents.indent - baseIndent) " " ++ first.contents.str)
        :: List.map
            (\line ->
                case line.contents of
                    Nothing ->
                        ""

                    Just { indent, str } ->
                        String.repeat (indent - baseIndent) " " ++ str
            )
            rest
    )
        |> List.intersperse "\n"
        |> String.concat
