module Camperdown.Parse.TextCursor exposing
    ( Annotation
    , TextCursor
    , addParsed
    , addText
    , commitCursor
    , newline
    , pushAnnotation
    )

import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Occurs exposing (Occurs(..))
import Camperdown.Parse.Offset exposing (Offset, position)
import Camperdown.Parse.Syntax as Syn
import Camperdown.Problem as Problem


{-| The TextCursor design is taken from the elm-markup data structure with the same name.

The idea behind using a text cursor is that, in order to successfully have a fail-free parser in
the context of Elm's current parsing infrastructure, you have to be able to parse every _prefix_
in a meaningful sense.

Let's take a look at what that looks like.

    My [dog [has] [fleas]] today
    ^

So far, you've parsed the string `M`

    My [dog [has] [fleas]] today
    ^^^^^^^

So far, you've parsed the string `My [dog`, and are in a state where a single closing `]`
is required at some point.

    My [dog [has] [fleas]] today
    ^^^^^^^^^^^^^^^^^

This is getting complicated.

  - The _current_ passage being parsed is `fl`, which is eventually going to
    become an annotation, one supposes.
  - Once that annotation is completely discovered, then we're working on parsing
    an annotation containing `dog [has]`, followed by whatever the annotation we
    eventually create is.
  - Once we know what _that_ annotation is, then we're working on parsing an annotation
    containing `My`, followed by whatever the annotation we eventually create is.

This ordered sequence of goals forms a (call) stack structure:

  - Goal: Parse `fl`
  - Goal: Parse `dog [has]`, plus whatever is returned from above.
  - Goal: Parse `My`, plus whetever is returned from above.

The current _raw text component_ being parsed is stored in the `text` field. This would
contain `M` in the first example, `dog` in the second example, and `fl` in the third
example.

The current _trailing list of text segments_ is stored in the `parsed` field. This would
be the empty list in the first and third examples, and `dog [has]` in the third example.
That's really three components: the raw text `dog`, the annotation `[has]`, and the
raw text segment that's the single space between annotations.

Because functional programming lists, these are stored in the "wrong order":

    [ Types.Raw " "
    , Annotation { startMark = "[", contents = [ Types.Raw "has" ], endMark = "]", command = Nothing }
    , Types.Raw "dog "
    ]

So "cursor" is perhaps a misnomer: it's really a "selection," and the process of parsing feels like
dragging that selection, line by line, across the whole passage. The reason "cursor" is a good name
is that the goal of the parser's `styledTextLoop` is to always chomp at least
SOME of the file that's immediately ahead of the "cursor", incorporating that new text into the
cursor/selection/whatever.

-}
type alias TextCursor =
    { text : String
    , parsed : List Syn.Text
    , annotationStack : List Annotation
    }


type alias Annotation =
    { startMark : Loc String
    , expectedEndMark : String
    , commandOccursAfterwards : Occurs
    , precedingText : List Syn.Text
    }


{-| Append raw text to the current cursor.
-}
addText : String -> TextCursor -> TextCursor
addText newText cursor =
    { cursor | text = cursor.text ++ newText }


{-| Append a text component to the current cursor.
-}
addParsed : Syn.Text -> TextCursor -> TextCursor
addParsed newParsed cursor =
    if cursor.text == "" then
        { cursor | parsed = newParsed :: cursor.parsed }

    else
        { cursor
            | parsed = newParsed :: Syn.Raw cursor.text :: cursor.parsed
            , text = ""
        }


{-| When an annotation BEGINS (for example, when a `[` is found, or a new bold
section is opened with `*`), this function transforms the text cursor so that
it is prepared to take in text that is captured by the annotation.

This involves pushing the current context onto the cursor's stack of tasks.

-}
pushAnnotation : { startMark : Loc String, expectedEndMark : String, commandOccursAfterwards : Occurs } -> TextCursor -> TextCursor
pushAnnotation { startMark, expectedEndMark, commandOccursAfterwards } cursor =
    if cursor.text == "" then
        { cursor
            | parsed = []
            , annotationStack =
                { startMark = startMark
                , expectedEndMark = expectedEndMark
                , commandOccursAfterwards = commandOccursAfterwards
                , precedingText = cursor.parsed
                }
                    :: cursor.annotationStack
        }

    else
        { cursor
            | text = ""
            , parsed = []
            , annotationStack =
                { startMark = startMark
                , expectedEndMark = expectedEndMark
                , commandOccursAfterwards = commandOccursAfterwards
                , precedingText = Syn.Raw cursor.text :: cursor.parsed
                }
                    :: cursor.annotationStack
        }


{-| Newlines are basically ignored by the cursor, aside from counting as mandatory
whitespace and always splitting text segments.
-}
newline : TextCursor -> TextCursor
newline cursor =
    { cursor | text = "", parsed = Syn.Raw (cursor.text ++ " ") :: cursor.parsed }


{-| commitCursor is called when the cursor cannot advance any further
(because we screwed up or because we reached the end of input).

If the cursor has entered into annotation zones that were never finished, the stack of
unfinished annotation zones needs to be recursively unwound. That error handling is the
main purpose of this function.

-}
commitCursor : Offset -> ( Int, Int ) -> TextCursor -> List Syn.Text
commitCursor offset end cursor =
    let
        parsed =
            if cursor.text == "" then
                cursor.parsed

            else
                Syn.Raw cursor.text :: cursor.parsed
    in
    case cursor.annotationStack of
        [] ->
            List.reverse parsed

        { startMark, expectedEndMark, precedingText } :: annotationStack ->
            let
                problem =
                    Problem.AnnotationDoesNotFinish
                        { startMark = startMark
                        , expectedEndMark = expectedEndMark
                        , end = position offset end
                        }

                newParsed =
                    Syn.InlineProblem problem :: parsed ++ Syn.Raw (Loc.value startMark) :: precedingText
            in
            commitCursor offset end { cursor | text = "", annotationStack = annotationStack, parsed = newParsed }
