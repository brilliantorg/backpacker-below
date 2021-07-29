module Camperdown.Parse.Hierarchy exposing (elements)

import Camperdown.Loc as Loc
import Camperdown.Parse.Pieces as Piece exposing (Piece)
import Camperdown.Parse.Syntax as Syn



{- The goal here is to re-group a list of pieces into their hierarchical elements.

   When a piece is encountered that is expected to have children,
   `getMoreIndented pieces` is called to separate the remainder of the stream into two
   bits: the bit that, by viture of its increased indent, is a child of that piece,
   and the bit that's not.

       >    - I'm a piece that might have children
       ✓         - I'm child 1 of that piece!
       ✓       - I'm child 2 of that piece!
       ✓          - I'm a child (possibly of child 2, if child 2 can have children)
       ✓       - I'm a child (possibly of child 2, if child 2 can have children)
       X    - I am not a child of that first piece
       X          - I am not a child of that first piece
       X        - I am not a child of that first piece
       X          - I am not a child of that first piece

   `getElements` is then called recursively on the first segment, which parses
   all of the child pieces into proper children.

   This is not really sufficient for reasonable Camperdown: a later pass
   needs to be performed to mark as a problem places where child elements do not
   all have the _same_ indentation level. Without this as-yet unimpelemented
   pass, the parse structure could get really mysterious of someone forgets a
   chevron.

   See PARSE.md for details

-}


elements : List Piece -> List Syn.Element
elements pieces =
    getElements [] pieces
        |> (\elems -> checkElementsNesting TopLevel elems [])


getElements : List Syn.Element -> List Piece -> List Syn.Element
getElements accum pieces =
    case getNextElement pieces of
        Nothing ->
            List.reverse accum

        Just ( elem, rest ) ->
            getElements (elem :: accum) rest


getNextElement : List Piece -> Maybe ( Syn.Element, List Piece )
getNextElement pieces =
    case pieces of
        [] ->
            Nothing

        piece :: rest ->
            case piece.piece of
                Piece.Paragraph markup ->
                    Just ( Syn.Paragraph { contents = markup }, rest )

                Piece.Item markLoc markup ->
                    let
                        ( childPieces, remainder, end ) =
                            getMoreIndentedPieces [] piece.indent piece.lines.end rest

                        children =
                            getElements [] childPieces
                    in
                    Just
                        ( Syn.Item
                            { markLoc = markLoc
                            , lines = { start = piece.lines.start, end = end }
                            , indent = piece.indent
                            , children = Syn.Paragraph { contents = markup } :: children
                            }
                        , remainder
                        )

                Piece.Command mark command Piece.EndsWithoutDivert ->
                    Just
                        ( Syn.Command
                            { mark = mark
                            , command = command
                            , child = Nothing
                            , lines = piece.lines
                            , indent = piece.indent
                            }
                        , rest
                        )

                Piece.Command mark command (Piece.EndsImmediate _ markup) ->
                    let
                        {- TODO: this is _slightly_ wonky because this will be parsed as two separate paragraphs:

                              ! foo vv
                                bar
                              baz

                           when it should really be a single paragraph containing "bar baz." (Honestly, it should really
                           just be a Problem; this definitely remains as an underspecified corner case.)
                        -}
                        firstChild =
                            case markup of
                                [] ->
                                    []

                                _ ->
                                    [ Syn.Paragraph { contents = markup } ]

                        children =
                            firstChild ++ getElements [] rest
                    in
                    Just
                        ( Syn.Command
                            { mark = mark
                            , command = command
                            , child = Just (Syn.Immediate children)
                            , lines = piece.lines
                            , indent = piece.indent
                            }
                        , []
                        )

                Piece.Command mark command (Piece.EndsNamed label) ->
                    Just
                        ( Syn.Command
                            { mark = mark
                            , command = command
                            , child = Just (Syn.Reference label)
                            , lines = piece.lines
                            , indent = piece.indent
                            }
                        , rest
                        )

                Piece.Command mark command (Piece.EndsNested markup) ->
                    let
                        ( childPieces, remainder, end ) =
                            getMoreIndentedPieces [] piece.indent piece.lines.end rest

                        firstChild =
                            case markup of
                                [] ->
                                    []

                                _ ->
                                    [ Syn.Paragraph { contents = markup } ]

                        children =
                            firstChild ++ getElements [] childPieces
                    in
                    Just
                        ( Syn.Command
                            { mark = mark
                            , command = command
                            , child = Just (Syn.Nested children)
                            , lines = { start = piece.lines.start, end = end }
                            , indent = piece.indent
                            }
                        , remainder
                        )

                Piece.Preformatted str ->
                    Just
                        ( Syn.Preformatted { lines = piece.lines, indent = piece.indent, contents = str }
                        , rest
                        )

                Piece.Problem { errorLocation, errorMessage } ->
                    let
                        ( _, remainder, end ) =
                            getMoreIndentedPieces [] piece.indent piece.lines.end rest
                    in
                    Just
                        ( Syn.Problem
                            { lines = { start = piece.lines.start, end = end }
                            , indent = piece.indent
                            , loc = errorLocation
                            , problem = errorMessage
                            }
                        , remainder
                        )


getMoreIndentedPieces : List Piece -> Int -> Int -> List Piece -> ( List Piece, List Piece, Int )
getMoreIndentedPieces accum baseIndent end pieces =
    case pieces of
        [] ->
            ( List.reverse accum, [], end )

        piece :: rest ->
            if piece.indent > baseIndent then
                getMoreIndentedPieces (piece :: accum) baseIndent piece.lines.end rest

            else
                ( List.reverse accum, pieces, end )


type Mode
    = TopLevel
    | UnknownLevel
    | KnownLevel { expectedIndent : Int, lineWhereIndentSeen : Int, wasACommand : Bool }


checkElementsNesting : Mode -> List Syn.Element -> List Syn.Element -> List Syn.Element
checkElementsNesting mode elems accum =
    case elems of
        [] ->
            List.reverse accum

        elem :: rest ->
            let
                ( newElem, newMode ) =
                    checkElementNesting mode elem
            in
            checkElementsNesting newMode rest (newElem :: accum)


checkElementNesting : Mode -> Syn.Element -> ( Syn.Element, Mode )
checkElementNesting mode elem =
    let
        loc { lines, indent } =
            { start = { line = lines.start, column = indent + 1 }
            , end = { line = lines.start, column = indent + 2 }
            }

        msg { lineWhereIndentSeen, wasACommand } thisIsMoreIndented =
            if wasACommand && thisIsMoreIndented then
                "Did you forget to add the chevron `>>` at the end of the command on line " ++ String.fromInt lineWhereIndentSeen ++ "?"

            else
                "Either the indentation of line " ++ String.fromInt lineWhereIndentSeen ++ ", or the indentation of this section, needs to change. (This sometimes happens if there is a tab character on one of these lines.)"
    in
    case elem of
        Syn.Paragraph _ ->
            ( elem, mode )

        Syn.Problem _ ->
            ( elem, mode )

        Syn.Preformatted pre ->
            let
                knowledge =
                    { expectedIndent = pre.indent, lineWhereIndentSeen = pre.lines.start, wasACommand = False }

                problem =
                    { lines = pre.lines, indent = pre.indent, loc = loc pre, problem = "" }
            in
            case mode of
                TopLevel ->
                    if pre.indent == 0 then
                        ( elem, KnownLevel knowledge )

                    else
                        ( Syn.Problem { problem | problem = "I did not expect this preformatted section to be indented, and that is confusing." }, mode )

                UnknownLevel ->
                    ( elem, KnownLevel knowledge )

                KnownLevel known ->
                    if pre.indent == known.expectedIndent then
                        ( elem, KnownLevel knowledge )

                    else
                        ( Syn.Problem { problem | problem = "I am confused, because I expect this preformatted section to be indented the same amount as line " ++ String.fromInt known.lineWhereIndentSeen ++ ". " ++ msg known (pre.indent > known.expectedIndent) }, mode )

        Syn.Item item ->
            let
                knowledge =
                    { expectedIndent = item.indent, lineWhereIndentSeen = item.lines.start, wasACommand = False }

                problem =
                    { lines = item.lines, indent = item.indent, loc = item.markLoc, problem = "" }

                newItem =
                    Syn.Item { item | children = checkElementsNesting UnknownLevel item.children [] }
            in
            case mode of
                TopLevel ->
                    if item.indent == 0 then
                        ( newItem, KnownLevel knowledge )

                    else
                        ( Syn.Problem { problem | problem = "I did not expect this item to be indented, and that is confusing." }, mode )

                UnknownLevel ->
                    ( newItem, KnownLevel knowledge )

                KnownLevel known ->
                    if item.indent == known.expectedIndent then
                        ( newItem, KnownLevel knowledge )

                    else
                        ( Syn.Problem { problem | problem = "I am confused, because I expect this item to be indented the same amount as line " ++ String.fromInt known.lineWhereIndentSeen ++ ". " ++ msg known (item.indent > known.expectedIndent) }, mode )

        Syn.Command command ->
            let
                knowledge =
                    { expectedIndent = command.indent, lineWhereIndentSeen = command.lines.start, wasACommand = True }

                problem =
                    { lines = command.lines, indent = command.indent, loc = Loc.location command.mark, problem = "" }

                newItem =
                    Syn.Command
                        { command
                            | child =
                                Maybe.map
                                    (\child ->
                                        case child of
                                            Syn.Reference ref ->
                                                Syn.Reference ref

                                            Syn.Nested children ->
                                                Syn.Nested <|
                                                    checkElementsNesting UnknownLevel children []

                                            Syn.Immediate children ->
                                                Syn.Immediate <|
                                                    checkElementsNesting (KnownLevel { knowledge | wasACommand = False }) children []
                                    )
                                    command.child
                        }
            in
            case mode of
                TopLevel ->
                    if command.indent == 0 then
                        ( newItem, KnownLevel knowledge )

                    else
                        ( Syn.Problem { problem | problem = "I did not expect this command to be indented, and that is confusing" }, mode )

                UnknownLevel ->
                    ( newItem, KnownLevel knowledge )

                KnownLevel known ->
                    if command.indent == known.expectedIndent then
                        ( newItem, KnownLevel knowledge )

                    else
                        ( Syn.Problem { problem | problem = "I am confused, because I expect this command to be indented the same amount as line " ++ String.fromInt known.lineWhereIndentSeen ++ ". " ++ msg known (command.indent > known.expectedIndent) }, mode )
