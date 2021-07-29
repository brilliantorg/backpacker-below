module Camperdown.Parse.Parse exposing (CommandParseResult(..), Continuation(..), lineCommand, lineItem, paragraph)

import Camperdown.Config.Config as Config
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Occurs exposing (Occurs(..))
import Camperdown.Parse.CommandElement as CommandElement exposing (parseCommandElement)
import Camperdown.Parse.Offset exposing (Offset, locate)
import Camperdown.Parse.Syntax as Syn
import Camperdown.Parse.TextCursor as TextCursor exposing (TextCursor, addParsed, addText, commitCursor, newline, pushAnnotation)
import Camperdown.Problem as Problem
import Camperdown.Util exposing (Parser, loop, return)
import List.Extra as List
import Parser.Advanced as Parser exposing ((|.), (|=))
import Set exposing (Set)


type alias Config =
    Config.ParserConfig



-- PART ONE: INCREMENTAL, FAIL-FREE COMMAND PARSING


{-| A command succeeds or fails to parse as a whole, but can ultimately
parse in a number of different forms
-}
type CommandParseResult
    = -- ! command
      CommandSimple Syn.Command
    | -- ! command -> some label
      CommandWithDivert Syn.Command (Loc String)
    | -- ! command vv
      CommandWithImmediateCont Syn.Command Loc.Location Syn.Markup
    | -- ! command >> some markup text
      CommandWithNestedCont Syn.Command Syn.Markup
    | -- Something went wrong
      Er
        { capturedStartIndex : Int
        , capturedEndIndex : Int
        , errorLocation : Loc.Location
        , errorMessage : String
        }


type Continuation
    = -- ! command
      None
    | -- ! command -> go somewhere else
      Divert (Loc String)
    | -- ! command vv
      Immediate
    | -- ! command >> text in the child block
      Nested


{-| Parses a line command: a command beginning with a `!` or a `?`.

Called externally, by the passage parser.

-}
lineCommand : Config -> Offset -> Parser CommandParseResult
lineCommand config offset =
    (Parser.succeed identity
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompIf (\ch -> ch == '?' || ch == '!') ()
        |. Parser.spaces
    )
        |> Parser.andThen
            (\beginStrIndex ->
                Parser.succeed (\result -> ( beginStrIndex, result ))
                    |= command config True offset beginStrIndex
                    |. Parser.spaces
            )
        |> Parser.andThen
            (\( beginStrIndex, result ) ->
                Parser.oneOf
                    [ Parser.succeed result
                        |. Parser.end ()
                    , (Parser.succeed (\start x end endOffset -> ( locate offset start x end, endOffset ))
                        |= Parser.getPosition
                        |= Parser.getChompedString (Parser.chompIf (\_ -> True) ())
                        |= Parser.getPosition
                        |= Parser.getOffset
                      )
                        |> Parser.map
                            (\( ( loc, str ), endOffset ) ->
                                Er
                                    { capturedStartIndex = beginStrIndex
                                    , capturedEndIndex = endOffset
                                    , errorLocation = loc
                                    , errorMessage = "Didn't expect to encounter the character `" ++ str ++ "` here."
                                    }
                            )
                    ]
            )


lineItem : Config -> Offset -> Parser Syn.Markup
lineItem config offset =
    Parser.succeed identity
        |. Parser.spaces
        |. Parser.chompIf (\ch -> ch == ':') ()
        |= paragraph config offset


{-| Parses an inline command: a command that takes place as part of an annotation inside parentheses

Called internally, from the markup parser.

-}
inlineCommand :
    Config
    -> Offset
    -> Parser (Result { loc : Loc.Location, reason : String, captured : String } Syn.Command)
inlineCommand config offset =
    Parser.getOffset
        |> Parser.andThen (command config False offset)
        |> Parser.andThen
            (\result ->
                case result of
                    CommandSimple cmd ->
                        Parser.succeed <| Ok cmd

                    Er { capturedStartIndex, capturedEndIndex, errorLocation, errorMessage } ->
                        Parser.succeed
                            (\str ->
                                Err <|
                                    { loc = errorLocation
                                    , reason = errorMessage
                                    , captured = String.slice capturedStartIndex capturedEndIndex str
                                    }
                            )
                            |= Parser.getSource

                    _ ->
                        Parser.problem ()
            )


{-| Core command parser, called by `lineCommand` and `inlineCommand`
-}
command : Config -> Bool -> Offset -> Int -> Parser CommandParseResult
command config divertable offset capturedStartIndex =
    Parser.loop Start (commandLoop config divertable offset capturedStartIndex)


{-| The command loop operates via a command cursor, which captures the pieces of the command
that have thus far been captured.
-}
type CommandCursor
    = Start
    | PositionalArgs
        { commandName : Maybe (Loc String)
        , accum : List (Loc Syn.Value)
        }
    | Parameters
        { commandName : Maybe (Loc String)
        , positionalArgs : List (Loc Syn.Value)
        , parameter : Loc (Loc String)
        , accumVals : List (Loc Syn.Value)
        , accumParams : List (Loc Syn.Parameter)
        }


commandLoop : Config -> Bool -> Offset -> Int -> CommandCursor -> Parser (Parser.Step CommandCursor CommandParseResult)
commandLoop config divertable offset capturedStartIndex cursor =
    parseCommandElement offset
        |> Parser.andThen
            (\commandElement ->
                case commandElement of
                    Nothing ->
                        return <| CommandSimple (cursorToCommand cursor)

                    Just ( loc, CommandElement.Identifier str ) ->
                        loop <| appendIdentifier ( loc, str ) cursor

                    Just ( loc, CommandElement.Number n ) ->
                        loop <| appendNumber ( loc, n ) cursor

                    Just ( loc, CommandElement.String str ) ->
                        loop <| appendString ( loc, str ) cursor

                    Just ( loc, CommandElement.Parameter param ) ->
                        loop <| appendParameter ( loc, param ) cursor

                    Just ( locStart, CommandElement.OpenSquareBracket ) ->
                        -- Markup time!
                        styledText config offset
                            |> Parser.andThen
                                (\markup ->
                                    Parser.oneOf
                                        [ -- The "correct" case is simpler: after the markup,
                                          -- there is a closing bracket, as expected.
                                          (Parser.succeed (locate offset)
                                            |= Parser.getPosition
                                            |= Parser.chompIf (\ch -> ch == ']') ()
                                            |= Parser.getPosition
                                            |. Parser.spaces
                                          )
                                            |> Parser.andThen
                                                (\( locEnd, () ) ->
                                                    loop <| appendMarkup ( { start = locStart.start, end = locEnd.end }, markup ) cursor
                                                )
                                        , -- Oh no, something has gone wrong. Either the markup went
                                          -- all the way to the end of input, or else... something
                                          -- even weirder happened.
                                          Parser.succeed
                                            (\startPos chompyChar endPos capturedEndIndex ->
                                                let
                                                    ( locEnd, () ) =
                                                        locate offset startPos () endPos
                                                in
                                                case chompyChar of
                                                    Nothing ->
                                                        Parser.Done <|
                                                            Er
                                                                { capturedStartIndex = capturedStartIndex
                                                                , capturedEndIndex = capturedEndIndex
                                                                , errorLocation = { start = locStart.start, end = locEnd.end }
                                                                , errorMessage = "While parsing marked-up text in an annotation, I never found the ending square bracket `]` that I expected."
                                                                }

                                                    Just ch ->
                                                        -- This is probably defensive dead code.
                                                        -- It would take a misconfiguration in the
                                                        -- Config for the styledText parser to
                                                        -- stop on anything besides an unmatched
                                                        -- closing square bracket.
                                                        --
                                                        -- (See comments to `paragraph` function)
                                                        Parser.Done <|
                                                            Er
                                                                { capturedStartIndex = capturedStartIndex
                                                                , capturedEndIndex = capturedEndIndex
                                                                , errorLocation = locEnd
                                                                , errorMessage = "While parsing marked-up text in an annotation, I found the character `" ++ ch ++ "`, which I didn't expect."
                                                                }
                                            )
                                            |= Parser.getPosition
                                            |= Parser.oneOf [ Parser.getChompedString (Parser.chompIf (\_ -> True) ()) |> Parser.map Just, Parser.succeed Nothing ]
                                            |= Parser.getPosition
                                            |. abandoned config offset False
                                            |= Parser.getOffset
                                        ]
                                )

                    Just ( loc, CommandElement.Divert foundDivert ) ->
                        if not divertable then
                            Parser.succeed
                                (\capturedEndIndex ->
                                    Parser.Done <|
                                        Er
                                            { capturedStartIndex = capturedStartIndex
                                            , capturedEndIndex = capturedEndIndex
                                            , errorLocation = loc
                                            , errorMessage = "Annotation commands cannot contain diverts (`vv`, `>>`, or `->`)."
                                            }
                                )
                                |. abandoned config offset False
                                |= Parser.getOffset

                        else
                            case foundDivert of
                                CommandElement.Immediate ->
                                    (Parser.succeed (locate offset)
                                        |= Parser.getPosition
                                        |= Parser.getChompedString (Parser.chompWhile (\ch -> ch /= '\n'))
                                        |= Parser.getPosition
                                    )
                                        |> Parser.andThen
                                            (\( textloc, text ) ->
                                                if String.trim text == "" then
                                                    Parser.succeed
                                                        (\markup ->
                                                            Parser.Done <| CommandWithImmediateCont (cursorToCommand cursor) loc markup
                                                        )
                                                        |= styledText config offset

                                                else
                                                    Parser.succeed
                                                        (\capturedEndIndex ->
                                                            Parser.Done <|
                                                                Er
                                                                    { capturedStartIndex = capturedStartIndex
                                                                    , capturedEndIndex = capturedEndIndex
                                                                    , errorLocation = textloc
                                                                    , errorMessage = "There cannot be text on the same line after an immediate divert `vv`."
                                                                    }
                                                        )
                                                        |= Parser.getOffset
                                            )

                                CommandElement.Indented ->
                                    (Parser.succeed identity
                                        |. Parser.spaces
                                        |= styledText config offset
                                    )
                                        |> Parser.map (CommandWithNestedCont (cursorToCommand cursor) >> Parser.Done)

                                CommandElement.Named ->
                                    (Parser.succeed (locate offset)
                                        |= Parser.getPosition
                                        |= Parser.getChompedString (Parser.chompWhile (\ch -> ch /= '\n'))
                                        |= Parser.getPosition
                                    )
                                        |> Parser.map
                                            (\( textloc, text ) ->
                                                Parser.Done <| CommandWithDivert (cursorToCommand cursor) ( textloc, String.trim text )
                                            )

                    Just ( loc, CommandElement.Problem problem ) ->
                        Parser.succeed
                            (\capturedEndIndex ->
                                Parser.Done <|
                                    Er
                                        { capturedStartIndex = capturedStartIndex
                                        , capturedEndIndex = capturedEndIndex
                                        , errorLocation = loc
                                        , errorMessage = problem.errorMessage
                                        }
                            )
                            |. abandoned config offset problem.inStringParse
                            |= Parser.getOffset
            )



{- The cursor can be in multiple modes: capturing positional arguments, and capturing
   parameters. The append* helper functions avoid incessant case analysis inside of the
   `commandLoop`
-}


appendIdentifier : Loc String -> CommandCursor -> CommandCursor
appendIdentifier ( loc, s ) cursor =
    case cursor of
        Start ->
            PositionalArgs
                { commandName = Just ( loc, s )
                , accum = []
                }

        PositionalArgs curse ->
            PositionalArgs { curse | accum = ( loc, Syn.Variable s ) :: curse.accum }

        Parameters curse ->
            Parameters { curse | accumVals = ( loc, Syn.Variable s ) :: curse.accumVals }


appendNumber : Loc Int -> CommandCursor -> CommandCursor
appendNumber ( loc, n ) cursor =
    case cursor of
        Start ->
            PositionalArgs
                { commandName = Nothing
                , accum = [ ( loc, Syn.Int n ) ]
                }

        PositionalArgs curse ->
            PositionalArgs { curse | accum = ( loc, Syn.Int n ) :: curse.accum }

        Parameters curse ->
            Parameters { curse | accumVals = ( loc, Syn.Int n ) :: curse.accumVals }


appendString : Loc String -> CommandCursor -> CommandCursor
appendString ( loc, str ) cursor =
    case cursor of
        Start ->
            PositionalArgs
                { commandName = Nothing
                , accum = [ ( loc, Syn.String str ) ]
                }

        PositionalArgs curse ->
            PositionalArgs { curse | accum = ( loc, Syn.String str ) :: curse.accum }

        Parameters curse ->
            Parameters { curse | accumVals = ( loc, Syn.String str ) :: curse.accumVals }


appendMarkup : Loc Syn.Markup -> CommandCursor -> CommandCursor
appendMarkup ( loc, markup ) cursor =
    case cursor of
        Start ->
            PositionalArgs
                { commandName = Nothing
                , accum = [ ( loc, Syn.Markup markup ) ]
                }

        PositionalArgs curse ->
            PositionalArgs { curse | accum = ( loc, Syn.Markup markup ) :: curse.accum }

        Parameters curse ->
            Parameters { curse | accumVals = ( loc, Syn.Markup markup ) :: curse.accumVals }


appendParameter : Loc (Loc String) -> CommandCursor -> CommandCursor
appendParameter parameter cursor =
    case cursor of
        Start ->
            Parameters
                { commandName = Nothing
                , positionalArgs = []
                , parameter = parameter
                , accumVals = []
                , accumParams = []
                }

        PositionalArgs { commandName, accum } ->
            Parameters
                { commandName = commandName
                , positionalArgs = List.reverse accum
                , parameter = parameter
                , accumVals = []
                , accumParams = []
                }

        Parameters curse ->
            let
                ( paramBaseLoc, paramName ) =
                    curse.parameter

                paramLoc =
                    case curse.accumVals of
                        [] ->
                            paramBaseLoc

                        ( loc, _ ) :: _ ->
                            { start = paramBaseLoc.start, end = loc.end }
            in
            Parameters
                { curse
                    | parameter = parameter
                    , accumVals = []
                    , accumParams = ( paramLoc, ( paramName, List.reverse curse.accumVals ) ) :: curse.accumParams
                }


{-| Ultimately "commit" a cursor to the command it represents, when finished adding new things
-}
cursorToCommand : CommandCursor -> Syn.Command
cursorToCommand cursor1 =
    let
        -- Kind of hacky, but I think it's the best solution:
        -- appendParameter does some nitpicky work with setting locations of things
        -- so we just push a fake parameter to reuse that code
        cursor2 =
            appendParameter ( Loc.todoDummyLocation, ( Loc.todoDummyLocation, "_dummy_" ) ) cursor1
    in
    case cursor2 of
        Start ->
            -- Impossible because we just pushed a parameter
            -- but it makes the types happy
            ( Nothing, ( [], [] ) )

        PositionalArgs curse ->
            -- See above
            ( curse.commandName
            , ( List.reverse curse.accum, [] )
            )

        Parameters curse ->
            -- We can ignore curse.accumVals and curse.parameter
            -- Because those were set by the fake appendParameter call
            ( curse.commandName
            , ( curse.positionalArgs, List.reverse curse.accumParams )
            )


{-| This sad, forlorn parser is a big part of making the command parser work inside texts.
When the command cursor has given up, the `abandoned` parser consumes (in a string-quote-aware
fashion) all the content up until the end or the matching `)` that hopefully ends the
current command, or else until the end of the text.
-}
abandoned : Config -> Offset -> Bool -> Parser ()
abandoned config offset inStringParse =
    Parser.loop inStringParse (abandonedLoop config offset)


abandonedLoop : Config -> Offset -> Bool -> Parser (Parser.Step Bool ())
abandonedLoop config offset inStringParse =
    if inStringParse then
        Parser.oneOf
            [ Parser.succeed (Parser.Loop False)
                |. Parser.chompIf (\ch -> ch == '"') ()
            , Parser.succeed (Parser.Loop False)
                |. Parser.chompIf (\ch -> ch == '\n') ()
            , Parser.succeed (Parser.Loop True)
                |. Parser.chompIf (\ch -> ch == '\\') ()
                |. Parser.oneOf [ Parser.chompIf (\_ -> True) (), Parser.succeed () ]
            , Parser.succeed (Parser.Loop True)
                |. Parser.chompIf (\_ -> True) ()
                |. Parser.chompWhile (\ch -> ch /= '"' && ch /= '\\' && ch /= '\n')
            , Parser.succeed (Parser.Done ())
            ]

    else
        Parser.oneOf
            [ Parser.succeed (Parser.Loop True)
                |. Parser.chompIf (\ch -> ch == '"') ()
            , Parser.succeed (Parser.Loop False)
                |. Parser.chompIf (\ch -> ch == '[') ()
                |. styledText config offset
            , Parser.succeed (Parser.Loop False)
                |. Parser.chompIf (\ch -> ch == '\\') ()
                |. Parser.oneOf
                    [ Parser.chompIf (\_ -> True) ()
                    , Parser.succeed ()
                    ]
            , Parser.succeed (Parser.Loop True)
                |. Parser.chompIf (\ch -> ch /= '-' && ch /= ')') ()
                |. Parser.chompWhile (\ch -> ch /= '-' && ch /= ')' && ch /= '"' && ch /= '[')
            , Parser.succeed (Parser.Done ())
            ]



-- PART 2: INCREMENTAL, FAIL-FREE MARKUP TEXT PARSING


{-| Read an segement of text as a paragraph of styled text.
-}
paragraph : Config -> Offset -> Parser Syn.Markup
paragraph config offset =
    Parser.loop []
        (\oldContents ->
            styledText config offset
                |> Parser.andThen
                    (\newContents ->
                        {- This is *almost* defensive coding! `styledText` itself should manage to capture all the content in a
                           string. If that process goes as intended, then `Parser.oneOf` will always take the first branch, and this
                           will be a loop that always runs exactly once, `oldContents` will be empty, and `newContents` will
                           be the entire parsed file.

                           If `styledText` fails to parse the entire document, it's because a character immediately ahead
                           of the cursor was `meaningful`, and the document didn't have any other way of dealing with it.

                               *** This SHOULD only happen with the `]` character ***

                           the reason it needs to happen with the `]` is to allow the `styledText` loop
                           to be used to parse paragraphs of markup and to parse commands containing markup, like

                               ! button [foo bar *baz*]

                           A single character (again, it should only be `]`) will be consumed and marked with
                           `Syn.UnexpectedMarkupCharacter`, and then the `styledText` loop will take over again and attempt
                           to parse the rest of the string.
                        -}
                        Parser.oneOf
                            [ Parser.succeed (Parser.Done <| List.append oldContents newContents)
                                |. Parser.end ()
                            , (Parser.succeed (locate offset)
                                |= Parser.getPosition
                                |= chompIf (\_ -> True)
                                |= Parser.getPosition
                              )
                                |> Parser.map
                                    (\ch ->
                                        Parser.Loop <|
                                            List.concat [ oldContents, newContents, [ Syn.InlineProblem <| Problem.UnexpectedMarkupCharacter ch ] ]
                                    )
                            ]
                    )
        )


{-| `Parser.symbol` for when you promise you won't raise `()`.
-}
symbol : String -> Parser ()
symbol tok =
    Parser.symbol (Parser.Token tok ())


{-| `charmember ch set` is just `Set.member ch set`, but when the types don't match
because `ch` is definitely a one-character string because it was captured by
calling `Parser.getChompedString` on a
`Parser.chompIf`, but while you know that the type system does not.
-}
charmember : String -> Set Char -> Bool
charmember str =
    String.uncons str
        |> Maybe.withDefault ( '\u{0000}', "" )
        |> (\( ch, _ ) -> ch)
        |> Set.member


{-| `Parser.chompIf` returns a `Parser ()`. This is a convienence function to make it
return a `Parser Char` instead.
-}
chompIf : (Char -> Bool) -> Parser Char
chompIf f =
    Parser.getChompedString (Parser.chompIf f ())
        |> Parser.map (String.uncons >> Maybe.withDefault ( '\u{0000}', "" ) >> (\( ch, _ ) -> ch))


{-| Uses the `TextCursor` to parse an entire document in one swell foop.

See the definition of `TextCursor` for an explanation of how the cursor is set up.

-}
styledText : Config -> Offset -> Parser (List Syn.Text)
styledText config offset =
    Parser.loop { text = "", parsed = [], annotationStack = [] }
        (styledTextLoop config offset)


{-| Perhaps single most complex move in the parser is this one: You've just parsed the
first part of an annotation. it might be a freestanding symbol, `---`, a marked-up region
like `**some text**`, and it might be followed by a mandatory command _or_ by an optional
command. This function (which could benefit from further refactoring) parses the command,
if one is present and allowed/required, generates errors as necessary, and then inserts
the annotation (with optional included command) into the TextCursor.
-}
closeAnnotationWithPossibleCommand : Config -> Offset -> TextCursor -> TextCursor.Annotation -> List TextCursor.Annotation -> Maybe (Loc String) -> Parser TextCursor
closeAnnotationWithPossibleCommand config offset cursor annotation annotationStack endMark =
    let
        capturedText =
            if cursor.text == "" then
                List.reverse cursor.parsed

            else
                List.reverse (Syn.Raw cursor.text :: cursor.parsed)

        takeTheCommandPartAndUpdateTheCursor =
            \cmd ->
                { cursor
                    | text = ""
                    , parsed = Syn.Annotation annotation.startMark capturedText endMark cmd :: annotation.precedingText
                    , annotationStack = annotationStack
                }

        parseACommand =
            (Parser.succeed
                (\start offsetBeforeCommand result offsetAfterCommand hopefullyACloseParen offsetAtTheEnd end ->
                    ( locate offset start result end, hopefullyACloseParen, ( offsetBeforeCommand, offsetAfterCommand, offsetAtTheEnd ) )
                )
                |= Parser.getPosition
                |. symbol "("
                |= Parser.getOffset
                |= inlineCommand config offset
                |. Parser.spaces
                |= Parser.getOffset
                |= Parser.oneOf
                    [ -- Encode the status of "did I find a closing paren?" as a Maybe Bool
                      (Parser.succeed (locate offset)
                        |= Parser.getPosition
                        |= (Parser.chompIf (\ch -> ch == ')') () |> Parser.map (\_ -> ")"))
                        |= Parser.getPosition
                      )
                        |> Parser.map Just
                    , (Parser.succeed (locate offset)
                        |= Parser.getPosition
                        |= Parser.getChompedString (Parser.chompIf (\_ -> True) ())
                        |= Parser.getPosition
                        |. abandoned config offset False
                        |. Parser.oneOf [ Parser.chompIf (\ch -> ch == ')') (), Parser.succeed () ]
                      )
                        |> Parser.map Just
                    , Parser.succeed Nothing
                    ]
                |= Parser.getOffset
                |= Parser.getPosition
            )
                |> Parser.andThen
                    (\( result, hopefullyACloseParen, ( sliceStart, sliceMiddle, sliceEnd ) ) ->
                        case ( result, hopefullyACloseParen ) of
                            ( ( loc, Ok cmd ), Just ( _, ")" ) ) ->
                                -- All's well, the expected closing parenthesis was found
                                Parser.succeed <|
                                    takeTheCommandPartAndUpdateTheCursor (Just ( loc, cmd ))

                            ( ( loc, Ok cmd ), Nothing ) ->
                                -- The end of input was found instead of a closing parenthesis
                                Parser.succeed <|
                                    { cursor
                                        | text = ""
                                        , parsed =
                                            Syn.InlineProblem (Problem.CommandLacksClosingParen loc)
                                                :: Syn.Annotation annotation.startMark capturedText endMark (Just ( loc, cmd ))
                                                :: annotation.precedingText
                                        , annotationStack = annotationStack
                                    }

                            ( ( _, Ok _ ), Just ch ) ->
                                -- Some other character was found that's not the closing parenthesis
                                Parser.getSource
                                    |> Parser.map
                                        (\source ->
                                            { cursor
                                                | text = String.slice sliceMiddle sliceEnd source
                                                , parsed =
                                                    Syn.InlineProblem (Problem.CommandHasUnexpectedCharacter ch)
                                                        :: Syn.Raw (String.slice sliceStart sliceMiddle source)
                                                        :: Syn.Raw "("
                                                        -- TODO :: Syn.Raw (Maybe.map Loc.value endMark |> Maybe.withDefault )
                                                        :: List.reverse capturedText
                                                        ++ Syn.Raw (Loc.value annotation.startMark)
                                                        :: annotation.precedingText
                                                , annotationStack = annotationStack
                                            }
                                        )

                            ( ( _, Err explanation ), _ ) ->
                                -- The command parsing itself errored out
                                Parser.getSource
                                    |> Parser.map
                                        (\source ->
                                            { cursor
                                                | text = String.slice sliceMiddle sliceEnd source
                                                , parsed =
                                                    Syn.InlineProblem (Problem.InlineCommandParseError explanation)
                                                        :: Syn.Raw explanation.captured
                                                        :: Syn.Raw "("
                                                        -- TODO :: Syn.Raw (Loc.value endMark)
                                                        :: List.reverse capturedText
                                                        ++ Syn.Raw (Loc.value annotation.startMark)
                                                        :: annotation.precedingText
                                                , annotationStack = annotationStack
                                            }
                                        )
                    )
    in
    case annotation.commandOccursAfterwards of
        Never ->
            -- Success! Don't even look for a parentheses.
            Parser.succeed (takeTheCommandPartAndUpdateTheCursor Nothing)

        Sometimes ->
            -- There **might** be a command in parentheses.
            Parser.oneOf
                [ -- It seems like there is a command in parentheses. Let's add it!
                  parseACommand
                , -- It seems like there isn't a command in parentheses. Let's carry on.
                  Parser.succeed (takeTheCommandPartAndUpdateTheCursor Nothing)
                ]

        Always ->
            -- There had better be a command in parentheses
            Parser.oneOf
                [ parseACommand
                , -- It seems like there IS NOT a command in parenthesis. That needs to get flagged
                  -- as an error. Because there's no valid annotation, the annotation-captured text
                  -- will just be dumped back into the text stream.
                  Parser.succeed
                    { cursor
                        | text = ""
                        , parsed =
                            Syn.InlineProblem (Problem.CommandNotPresentWhereRequired (Loc.location (Maybe.withDefault annotation.startMark endMark)))
                                -- TODO : :: Syn.Raw (Loc.value endMark)
                                :: List.reverse capturedText
                                ++ Syn.Raw (Loc.value annotation.startMark)
                                :: annotation.precedingText
                        , annotationStack = annotationStack
                    }
                ]


{-| Attempts to add whatever content is directly in front of the cursor into the cursor.

It is critical that the parser succeed and either:

  - Finish the loop by returning `Parser.Done`, or
  - Chomp at least one character (and integrate it into the cursor)
    before returning `Parser.Loop`.

-}
styledTextLoop : Config -> Offset -> TextCursor -> Parser (Parser.Step TextCursor (List Syn.Text))
styledTextLoop config offset cursor =
    Parser.oneOf
        [ --
          -- ESCAPE CHARACTERS HAVE TO GET CAUGHT WITH HIGHEST PRIORITY
          --
          (Parser.succeed (locate offset)
            |= Parser.getPosition
            |. symbol "\\"
            |= Parser.getChompedString (Parser.chompIf (\_ -> True) ())
            |= Parser.getPosition
          )
            |> Parser.map
                (\(( _, ch ) as escape) ->
                    if charmember ch config.escapable then
                        addText ch cursor

                    else
                        addParsed (Syn.InlineProblem <| Problem.BadEscapeChar escape) cursor
                )
            |> Parser.map Parser.Loop
        , --
          -- CHECK FOR VERBATIM CAPTURE
          --
          (Parser.succeed (\start mark -> ( start, mark ))
            |= Parser.getPosition
            |= chompIf (\ch -> Set.member ch config.verbatimOpts)
          )
            |> Parser.andThen
                -- You can capture verbatim sections like `this` or like ````this````.
                -- This step captures the "endmark" - the set of repeating symbols that start - and
                -- therefore must end - the verbatim section
                (\( start, mark ) ->
                    Parser.succeed
                        (\str -> ( start, mark, String.repeat (1 + String.length str) (String.fromChar mark) ))
                        |= Parser.getChompedString (Parser.chompWhile (\ch -> ch == mark))
                        -- A single whitespace character from the beginning or end of a verbatim segment will always get trimmed
                        |. Parser.oneOf
                            [ Parser.chompIf (\ch2 -> ch2 == ' ') ()
                            , Parser.succeed ()
                            ]
                )
            |> Parser.andThen
                (\( start, mark, endmark ) ->
                    Parser.succeed
                        (\verbatimParseResult ->
                            case verbatimParseResult of
                                Ok verbatimText ->
                                    addParsed (Syn.Verbatim mark verbatimText) cursor

                                Err ( loc, str ) ->
                                    cursor
                                        |> addParsed (Syn.Raw (endmark ++ " " ++ str))
                                        |> addParsed
                                            (Syn.InlineProblem
                                                (Problem.VerbatimDoesNotFinish
                                                    { expectedEndmark = endmark
                                                    , loc = loc
                                                    }
                                                )
                                            )
                        )
                        |= Parser.loop [] (verbatimTextLoop offset start mark endmark)
                )
            |> Parser.map Parser.Loop
        , --
          -- CHECK FOR CLOSING ANNOTATION (AND POSSIBLE FOLLOW-ON COMMAND)
          --
          {- Wherever the cursor is, there is either 0 or 1 annotation _closing_ marks that are being looked for.
             0 if the cursor is inside of no annotations, 1 if the cursor is inside one or more annotations
             (annotations must be well nested.)
          -}
          case cursor.annotationStack of
            [] ->
                Parser.problem ()

            annotation :: annotationStack ->
                (Parser.succeed (locate offset)
                    |= Parser.getPosition
                    |= (symbol annotation.expectedEndMark |> Parser.map (\() -> annotation.expectedEndMark))
                    |= Parser.getPosition
                )
                    |> Parser.andThen (\endMark -> closeAnnotationWithPossibleCommand config offset cursor annotation annotationStack (Just endMark))
                    |> Parser.map Parser.Loop
        , --
          -- CHECK FOR OPENING ANNOTATION
          --
          {- All annotation START markers that are available to push into a new
             annotation. It's important that this check comes after the previous check for an annotation close.
             Otherwise in text like `foo *bar* baz` the second asterisk would be treated as a second
             open annotation, not as a close annotation.
          -}
          (Parser.oneOf <|
            List.map
                (\{ startSymbol, endSymbol, commandOccursAfterwards } ->
                    (Parser.succeed (locate offset)
                        |= Parser.getPosition
                        |= (Parser.succeed startSymbol |. symbol startSymbol)
                        |= Parser.getPosition
                    )
                        |> Parser.andThen
                            (\startMark ->
                                let
                                    newCursor =
                                        pushAnnotation
                                            { startMark = startMark
                                            , expectedEndMark = endSymbol |> Maybe.withDefault ""
                                            , commandOccursAfterwards = commandOccursAfterwards
                                            }
                                            cursor
                                in
                                case endSymbol of
                                    Nothing ->
                                        case newCursor.annotationStack of
                                            [] ->
                                                -- Can't happen, we just pushed a thing!
                                                Parser.problem ()

                                            annotation :: annotationStack ->
                                                closeAnnotationWithPossibleCommand config offset newCursor annotation annotationStack Nothing

                                    Just _ ->
                                        Parser.succeed newCursor
                            )
                )
                config.annotationOpts
          )
            |> Parser.map Parser.Loop
        , --
          -- LOOK IT'S A NEWLINE
          --
          symbol "\n"
            |> Parser.map (\_ -> Parser.Loop <| newline cursor)
        , --
          -- GET MOST ANYTHING ELSE
          --
          {- The interplay between the `meaningful` and `replacementFirstChars` configurations
             are a bit subtle. The `meaningful` chars are the ones that we _shouldn't_ just gobble up
             mindlessly, because they might mean something. Obviously, anything that should be caught
             above needs to be `meaningful`, so the cursor can stop advancing and make an intelligent decision.

             Most meaningful characters have the property that, if we chomp them, we're off to the races with
             doing something that is _definitely not_ just adding that text to the cursor. The exception is
             `replacementFirstChars`. If we see a period and we have a text replacement `...`, we need to
             make sure this interrupts the loop so we can check if `...` is chompable at this point. If
             we don't manage to chomp a replacement, that means that the period character should get added
             to the cursor.
          -}
          Parser.succeed (\c str -> Parser.Loop (addText (String.cons c str) cursor))
            |= chompIf (\c -> not (Set.member c config.meaningful) || Set.member c config.annotationFirstChars)
            |= Parser.getChompedString (Parser.chompWhile (\c -> not <| Set.member c config.meaningful))
        , --
          -- NOTHING MORE CAN BE PARSED, LEAVE LOOP
          --
          Parser.succeed (\end -> Parser.Done <| commitCursor offset end cursor)
            |= Parser.getPosition
        ]


verbatimTextLoop : Offset -> ( Int, Int ) -> Char -> String -> List Char -> Parser (Parser.Step (List Char) (Result (Loc String) (Loc String)))
verbatimTextLoop offset start _ endmark accum =
    Parser.oneOf
        [ Parser.succeed (\end -> Parser.Done <| Err <| locate offset start (String.fromList <| List.reverse accum) end)
            |= Parser.getPosition
            |. Parser.oneOf
                [ -- endmark must be encountered before end of file
                  Parser.end ()
                , -- endmark must be encountered before end of line
                  Parser.chompIf (\ch -> ch == '\n') ()
                ]
        , Parser.succeed (\end -> Parser.Done <| Ok <| locate offset start (String.fromList <| List.reverse accum) end)
            -- A single whitespace character from the end of a verbatim segment will always get trimmed
            |. Parser.oneOf [ symbol (" " ++ endmark), symbol endmark ]
            |= Parser.getPosition
        , Parser.succeed (\ch -> Parser.Loop (ch :: accum))
            |= chompIf (\_ -> True)
        ]
