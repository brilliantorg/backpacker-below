module Camperdown.Problem exposing (Inline(..), inlineToString)

{-|

@docs Inline, inlineToString

-}

import Camperdown.Loc as Loc exposing (Loc, Location)


{-| -}
type Inline
    = BadEscapeChar (Loc String)
    | VerbatimDoesNotFinish { expectedEndmark : String, loc : Location }
    | CommandLacksClosingParen Location
    | CommandHasUnexpectedCharacter (Loc String)
    | AnnotationDoesNotFinish { startMark : Loc String, expectedEndMark : String, end : Loc.Position }
    | InlineCommandParseError { loc : Loc.Location, reason : String, captured : String }
    | UnexpectedMarkupCharacter (Loc Char)
    | CommandNotPresentWhereRequired Location
    | InvalidVerbatimChar (Loc Char)
    | InvalidAnnotation (Loc ( String, Maybe String )) (Maybe (Loc (Maybe (Loc String))))
    | InvalidParameter (Loc String)


{-| -}
inlineToString : Inline -> ( Loc.Location, String )
inlineToString problem =
    case problem of
        BadEscapeChar ( loc, escaped ) ->
            ( loc, "Bad escape character: `" ++ escaped ++ "`" )

        VerbatimDoesNotFinish prob ->
            ( prob.loc, "Never found expected marker `" ++ prob.expectedEndmark ++ "` to end verbatim environment." )

        CommandLacksClosingParen loc ->
            ( loc, "I didn't find the closing parenthesis for this command." )

        CommandHasUnexpectedCharacter ( loc, ch ) ->
            ( loc, "I wasn't able to parse this command because I didn't know what to do with the `" ++ ch ++ "` character here." )

        AnnotationDoesNotFinish { startMark, expectedEndMark } ->
            ( Loc.location startMark, "I never found the closing mark `" ++ expectedEndMark ++ "` that I expected to match this `" ++ Loc.value startMark ++ "`" )

        InlineCommandParseError prob ->
            ( prob.loc, prob.reason )

        UnexpectedMarkupCharacter ( loc, ch ) ->
            ( loc, "I don't know what to do with this `" ++ String.fromChar ch ++ "` character." )

        CommandNotPresentWhereRequired loc ->
            ( loc, "A command in parentheses must begin immediately after this annotation, and there was no command found." )

        InvalidVerbatimChar ( loc, ch ) ->
            ( loc, "The verbatim delimiter `" ++ String.fromChar ch ++ "` is not supported." )

        InvalidAnnotation ( loc, ( start, Nothing ) ) Nothing ->
            ( loc, "The annotation `" ++ start ++ "` is not supported." )

        InvalidAnnotation ( loc, ( start, Just end ) ) Nothing ->
            ( loc, "The annotation `" ++ start ++ "..." ++ end ++ "` is not supported." )

        InvalidAnnotation ( _, ( start, Nothing ) ) (Just ( loc, Nothing )) ->
            ( loc, "The annotation `" ++ start ++ "(...)` must be given a command as the first argument with parentheses." )

        InvalidAnnotation ( _, ( start, Just end ) ) (Just ( loc, Nothing )) ->
            ( loc, "The annotation `" ++ start ++ "..." ++ end ++ "(...)` must be given a command as the first argument with parentheses." )

        InvalidAnnotation ( _, ( start, Nothing ) ) (Just ( _, Just ( loc, str ) )) ->
            ( loc, "The annotation `" ++ start ++ "(" ++ str ++ "...)` is not supported." )

        InvalidAnnotation ( _, ( start, Just end ) ) (Just ( _, Just ( loc, str ) )) ->
            ( loc, "The annotation `" ++ start ++ "... " ++ end ++ "(" ++ str ++ "...)` is not supported." )

        InvalidParameter ( loc, string ) ->
            ( loc, "There is no parameter `|> " ++ string ++ "`." )
