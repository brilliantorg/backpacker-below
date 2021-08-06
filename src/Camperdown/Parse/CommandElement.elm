module Camperdown.Parse.CommandElement exposing (CommandElement(..), FoundDivert(..), parseCommandElement)

import Camperdown.Loc exposing (Loc)
import Camperdown.Parse.Offset exposing (Offset, locate)
import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser value =
    Parser.Parser Never () value


type CommandElement
    = Identifier String
    | Number Int
    | Problem { errorMessage : String, inStringParse : Bool }
    | Parameter (Loc String)
    | Divert FoundDivert
    | String String
    | OpenSquareBracket


type FoundDivert
    = Immediate
    | Indented
    | Named


commandElementToString : CommandElement -> String
commandElementToString elem =
    case elem of
        Identifier str ->
            "identifier `" ++ str ++ "`"

        Number n ->
            "number `" ++ String.fromInt n ++ "`"

        Problem _ ->
            "problem"

        Parameter ( _, str ) ->
            "parameter `|> " ++ str ++ "`"

        Divert Immediate ->
            "immediate divert `vv`"

        Divert Indented ->
            "divert `>>`"

        Divert Named ->
            "divert `->`"

        String _ ->
            "quoted string"

        OpenSquareBracket ->
            "opening bracket `[`"


{-| This performs the tokenization step, handling that, for instance,
both `->` and `-14` are single tokens but `->4` is two, as are `5|>` and `x>>`.

It dispatches a string to `pickCommandElement`.

-}
parseCommandElement : Offset -> Parser (Maybe (Loc CommandElement))
parseCommandElement offset =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ -- Stringtime!
              (Parser.succeed identity
                |= Parser.getPosition
                |. Parser.symbol (Parser.Token "\"" ())
              )
                |> Parser.andThen (parseElmString offset)
                |> Parser.map
                    (\result ->
                        case result of
                            Ok ( loc, str ) ->
                                Just ( loc, String str )

                            Err ( loc, str ) ->
                                Just ( loc, Problem { errorMessage = str, inStringParse = True } )
                    )
            , -- Parametertime!
              (Parser.succeed (locate offset)
                |= Parser.getPosition
                |= Parser.symbol (Parser.Token "|>" ())
                |= Parser.getPosition
                |. Parser.spaces
              )
                |> Parser.andThen
                    (\( loc, () ) ->
                        Parser.succeed
                            (\next ->
                                case next of
                                    Nothing ->
                                        Just
                                            ( loc
                                            , Problem
                                                { errorMessage = "I found the parameter symbol `|>` without any following parameter name."
                                                , inStringParse = False
                                                }
                                            )

                                    Just ( _, Problem _ ) ->
                                        next

                                    Just ( loc2, Identifier str ) ->
                                        Just ( { start = loc.start, end = loc2.end }, Parameter ( loc2, str ) )

                                    Just ( loc2, elem ) ->
                                        Just
                                            ( { start = loc2.start, end = loc2.end }
                                            , Problem
                                                { errorMessage = "I expect the parameter symbol `|>` to be followed by an identifier, instead it was followed by a " ++ commandElementToString elem ++ "."
                                                , inStringParse = False
                                                }
                                            )
                            )
                            |= parseCommandElement offset
                    )
            , Parser.succeed (pickCommandElement offset)
                |= Parser.getPosition
                |= (Parser.symbol (Parser.Token "->" ()) |> Parser.map (\() -> "->"))
                |= Parser.succeed ""
                |= Parser.getPosition
            , Parser.succeed (pickCommandElement offset)
                |= Parser.getPosition
                |= (Parser.symbol (Parser.Token ">>" ()) |> Parser.map (\() -> ">>"))
                |= Parser.succeed ""
                |= Parser.getPosition
            , Parser.succeed (pickCommandElement offset)
                |= Parser.getPosition
                |= (Parser.symbol (Parser.Token "[" ()) |> Parser.map (\() -> "["))
                |= Parser.succeed ""
                |= Parser.getPosition
            , Parser.succeed (pickCommandElement offset)
                |= Parser.getPosition
                |= Parser.getChompedString (Parser.chompIf (\ch -> Char.isAlphaNum ch || ch == '-') ())
                |= Parser.getChompedString (Parser.chompWhile (\ch -> Char.isAlphaNum ch || ch == '_'))
                |= Parser.getPosition
            , Parser.succeed Nothing
            ]


{-| -}
pickCommandElement : Offset -> ( Int, Int ) -> String -> String -> ( Int, Int ) -> Maybe (Loc CommandElement)
pickCommandElement offset start a b end =
    let
        str =
            a ++ b
    in
    (case String.toList str of
        [] ->
            Problem
                { errorMessage = "An identifier was unexpectedly empty. (This probably indicates an unintended bug in the parser.)"
                , inStringParse = False
                }

        [ '[' ] ->
            OpenSquareBracket

        [ '0' ] ->
            Number 0

        [ '-', '0' ] ->
            Problem
                { errorMessage = "Negative zero? Don't be a troll."
                , inStringParse = False
                }

        [ '-', '>' ] ->
            Divert Named

        [ 'v', 'v' ] ->
            Divert Immediate

        [ '>', '>' ] ->
            Divert Indented

        [ '-' ] ->
            Problem { errorMessage = "You can't use mathematical notation like `-` here.", inStringParse = False }

        '0' :: _ ->
            Problem
                { errorMessage = "I want to treat `" ++ str ++ "` as an integer, but that integers can't start with a 0 (unless they're _only_ 0)."
                , inStringParse = False
                }

        '-' :: ch :: _ ->
            if ch == '0' then
                Problem
                    { errorMessage = "I wanted to treat `" ++ str ++ "` as an integer, but negative integers can't start with 0."
                    , inStringParse = False
                    }

            else if Char.isDigit ch then
                case String.toInt str of
                    Just n ->
                        Number n

                    Nothing ->
                        Problem
                            { errorMessage = "The `-` sign made me expect that `" ++ str ++ "` would be an integer, but that didn't work."
                            , inStringParse = False
                            }

            else
                Problem
                    { errorMessage = "The `-` sign made me expect that `" ++ str ++ "` would be an integer, but it's not made up of all digits."
                    , inStringParse = False
                    }

        ch :: chs ->
            if Char.isDigit ch then
                case String.toInt str of
                    Just n ->
                        Number n

                    Nothing ->
                        Problem
                            { errorMessage = "The leading `" ++ String.fromChar ch ++ "` made me expect that `" ++ str ++ "` would be an integer, but that didn't work."
                            , inStringParse = False
                            }

            else if Char.isUpper ch || Char.isLower ch then
                Problem
                    { errorMessage = "I don't know how to understand `" ++ str ++ "`. It doesn't look like a number, an identifier, or a special symbol that I know about."
                    , inStringParse = False
                    }

            else
                case List.filter (\c -> not (Char.isAlphaNum c || c == '_')) chs of
                    [] ->
                        Identifier str

                    bad :: _ ->
                        Problem
                            { errorMessage = "Because it starts with a letter, I want to treat `" ++ str ++ "` as an identifier, but the character `" ++ String.fromChar bad ++ "` is not allowed inside of an identifier."
                            , inStringParse = False
                            }
    )
        |> (\tok -> Just <| locate offset start tok end)


type alias ElmStringCursor =
    { accum : List Char }


type alias ElmStringParseResult =
    Result (Loc String) (Loc String)


parseElmString : Offset -> ( Int, Int ) -> Parser ElmStringParseResult
parseElmString offset begin =
    Parser.loop { accum = [] } (parseElmStringLoop offset begin)


lookupSpecialChar : String -> Maybe Char
lookupSpecialChar ch =
    case ch of
        "n" ->
            Just '\n'

        "r" ->
            Just '\u{000D}'

        "t" ->
            Just '\t'

        "\"" ->
            Just '"'

        "\\" ->
            Just '\\'

        _ ->
            Nothing


parseElmStringLoop : Offset -> ( Int, Int ) -> ElmStringCursor -> Parser (Parser.Step ElmStringCursor ElmStringParseResult)
parseElmStringLoop offset begin { accum } =
    Parser.oneOf
        [ -- The second '"' character ends the string
          Parser.succeed
            (\end ->
                let
                    capturedString =
                        String.fromList (List.reverse accum)
                in
                Parser.Done <| Ok (locate offset begin capturedString end)
            )
            |. Parser.chompIf (\ch -> ch == '"') ()
            |= Parser.getPosition
        , -- String escapes
          (Parser.succeed identity
            |= Parser.getPosition
            |. Parser.chompIf (\ch -> ch == '\\') ()
          )
            |> Parser.andThen
                (\before ->
                    Parser.oneOf
                        [ (Parser.succeed (locate offset before)
                            |= Parser.chompIf (\ch -> ch == 'u') ()
                            |= Parser.getPosition
                          )
                            |> Parser.map
                                (\( loc, () ) ->
                                    -- TODO unicode string support
                                    Parser.Done <| Err ( loc, "No unicode string support yet." )
                                )
                        , (Parser.succeed (locate offset before)
                            |= Parser.getChompedString (Parser.chompIf (\_ -> True) ())
                            |= Parser.getPosition
                          )
                            |> Parser.andThen
                                (\( loc, ch ) ->
                                    case lookupSpecialChar ch of
                                        Just c ->
                                            Parser.succeed (Parser.Loop { accum = c :: accum })

                                        Nothing ->
                                            Parser.succeed (Parser.Done <| Err ( loc, "The sequence `\\" ++ ch ++ "` is not a valid escape sequence." ))
                                )
                        , (Parser.succeed (locate offset before)
                            |= Parser.end ()
                            |= Parser.getPosition
                          )
                            |> Parser.map
                                (\( loc, () ) ->
                                    Parser.Done <| Err ( loc, "String not finished by the end of a line." )
                                )
                        ]
                )
        , -- Bad!! Newline or end of input
          (Parser.succeed (locate offset begin)
            |= Parser.oneOf [ Parser.chompIf (\ch -> ch == '\n') (), Parser.end () ]
            |= Parser.getPosition
          )
            |> Parser.map
                (\( loc, () ) ->
                    Parser.Done <| Err ( loc, "String not finished by the end of a line." )
                )
        , -- All other characters just get chomped. Nom.
          Parser.succeed (\ch -> Parser.Loop <| { accum = String.toList ch ++ accum })
            |= Parser.getChompedString (Parser.chompIf (\_ -> True) ())
        ]
