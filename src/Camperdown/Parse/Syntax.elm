module Camperdown.Parse.Syntax exposing
    ( Document, Label(..), Section
    , Element(..), ParagraphConfig, PreformattedConfig, ItemConfig, CommandConfig, Mark(..), Divert(..), ProblemConfig
    , Command, Config, Parameter, Value(..)
    , Markup, Text(..)
    )

{-|


# Documents

@docs Document, Label, Section


# Block elements

@docs Element, ParagraphConfig, PreformattedConfig, ItemConfig, CommandConfig, Mark, Divert, ProblemConfig


# Configuration

@docs Command, Config, Parameter, Value


# Text

@docs Markup, Text

-}

import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Problem as Problem


{-| Sections are marked by non-indented lines that start with a `#` character.

If the section description has content (like `# the best section`), then the section's
label will be `Named (loc, "the best section")`, where `loc` is the file position of the
string `the best section` - `{ start = { line = 4, column = 3 }, end = { line = 4, column = 18 } }`
if the section divider is on line 4 of the file, for example.

If the section description doesn't have any content (if it's just a series of `#` characters
followed by white space) then the section's label will be `Anonymous n`, where `n` is the
line number of the section.

-}
type Label
    = Named (Loc String)
    | Anonymous Int


{-| A document is a prelude followed by a list of sections.
-}
type alias Document =
    { prelude : List Element
    , sections : List Section
    }


{-| A section's level is described by the number of `#` section markers at the beginning of the line:

    # level 1 section
    ## level 2 section
    ##### level 5 section

and a section contains a list of block-level elements.

-}
type alias Section =
    { level : Int
    , label : Label
    , contents : List Element
    }


{-| Commands have two forms: `! command` and `? command`, the latter is a _subcommand_. The
characters `!` and `?` can be pronounced "bang" and "huh", respectively.
-}
type Mark
    = Bang
    | Huh


{-| Camperdown Elements are roughly analgous to HTML block-level elements.

Here's a possible Camperdown document that contains all the possible Element
types:

    This is a Paragraph. A single paragraph
    can continue across multiple lines
    as long as
    there are no empty lines.

    An empty line creates a new Paragraph. Paragraphs can contain
    **formatted** _text_ and [commands](link "https://google.com/").

    $$$ If `$$$` is specified as a Preformatted section sentinel,
        then this is a preformatted section, and
        the section will continue until we reach a line that's
        only has the same indentation level as the three dollar-signs
        that started the preformatted section.

    A Command is written like this `! command`, but only if the exclamation
    point (pronounced "bang") or the question mark (pronounced "huh") is
    the very first character on a line.

    ! quote >> This might indicate a quoted section.

    : This element is an Item, which is a useful way to do things like
      offsetting lists.

    The next element will be parsed as a Problem, because it's indented
    and the parser doesn't expect an indented element here.

        ! command "this will be parsed as a Problem"

-}
type Element
    = Paragraph ParagraphConfig
    | Preformatted PreformattedConfig
    | Item ItemConfig
    | Command CommandConfig
    | Problem ProblemConfig


{-| The configuration of a `Paragraph` `Element`.
-}
type alias ParagraphConfig =
    { contents : Markup
    }


{-| The configuration of a `Preformatted` `Element`.
-}
type alias PreformattedConfig =
    { lines : { start : Int, end : Int }
    , indent : Int
    , contents : String
    }


{-| The configuration of a `Item` `Element`.
-}
type alias ItemConfig =
    { lines : { start : Int, end : Int }
    , indent : Int
    , markLoc : Loc.Location
    , children : List Element
    }


{-| The configuration of a `Command` `Element`.
-}
type alias CommandConfig =
    { lines : { start : Int, end : Int }
    , mark : Loc Mark
    , command : Command
    , child : Maybe Divert
    , indent : Int
    }


{-| The configuration of a `Problem` `Element`.
-}
type alias ProblemConfig =
    { lines : { start : Int, end : Int }
    , indent : Int
    , loc : Loc.Location
    , problem : String
    }


{-| A command can (optionally) refer to a child section in one of three ways:

A command's child is the whole indented section past the command if using the `Nested`
chevron `>>`

    ! command >> Nested paragraph can start on the
        same line or a following line.

        Another paragraph nested inside the command.

        ! command nested in the command

A command's child is the entire equally-indented section past the command (up to the
next `#` marker, the end of the document, or the next less-indented section) if using
the `Immediate` chevron `vv`.

    ! command vv
    Next content needs to start on a subsequent line.

    This paragaph is also nested inside the command

    # another section
    This paragraph is not nested inside the command because
    it's in a new section.

A command's child can also be indirectly referenced by using a pointy-arrow `->` to
`Reference` some other section.

    ! choose vv
    ? [Option A] >> You picked option A.
    ? [Option B] -> option b

    ## option b
    You picked option B.

-}
type Divert
    = Nested (List Element)
    | Immediate (List Element)
    | Reference (Loc String)


{-| A `Command` captures the elm-like configuration components associated with block
level and inline commands.

Simple block-level command:

    > ! test
    >   ----
    >   loc1

    ( Just ( loc1, "test" ), ( [], [] ) )

Block in a block-level command:

    > ! image "colorful.png" |> bold
    >   ----- -------------- -------
    >   loc1  loc2           loc3
    >                           ----
    >                           loc4

    ( Just ( loc1, "image" )
    , ( [ ( loc2, String "colorful.png" ) ], [ ( loc3, ( ( loc4, "bold" ), [] ) ) ] )
    )

Subcommand:

    > ? choice |> response [Correct!]
    >   ------ ----------------------
    >   loc1   loc2
    >             -------- ----------
    >             loc3     loc4

    ( Just ( loc1, "choice" )
    , ( [], [ ( loc2, ( ( loc3, "response" ), [ ( loc4, Markup [ Raw "Correct!" ] ) ] ) ) ] )
    )

Subcommand with no command name:

    > ? [Door Number 2] >> That's correct!
    >   ---------------
    >   loc1

    ( Nothing
    , ( [ ( loc1, Markup [ Raw "Door Number 2" ] ) ], [] )
    )

Inline command

    > This is a [ website ] (link "https://google.com")
    >                       ---- --------------------
    >                       loc1 loc2

    ( Just ( loc1, "choice" )
    , ( [ ( loc2, String "https://google.com" ) ], [] )
    )

-}
type alias Command =
    ( Maybe (Loc String), Config )


{-| The `Config` is everything in a command except for the command name.
-}
type alias Config =
    ( List (Loc Value), List (Loc Parameter) )


{-| A `Parameter` in Camperdown is written as `|> parameterName arg1 arg2 ...`.
Writing `! command |> param1 |> param2 "argument"` is intended to roughly correspond
to the XML form `<command param1 param2="argument">`, but the use of the Elmish
pipe syntax makes it reasonable for parameters to have multiple arguments, instead
of the 0 or 1 allowed by XML's syntax.
-}
type alias Parameter =
    ( Loc String, List (Loc Value) )


{-| The arguments to a command or a parameter can have one of four forms:

  - `Variable`: `true`, `False`, `forgetMeNot`
  - `String`: `"this or that"`, `"a \"lost\" doll"`
  - `Int`: `0`, `-12`, `33`
  - `Markup`: `[Text intended for display]`, `[It can be **formatted**]`,
    `[It can even contain [subcommands](link "https://www.google.com/search?q=subcommand") and stuff like that]`

-}
type Value
    = Variable String
    | String String
    | Int Int
    | Markup Markup


{-| Markup is just a list of text elements.
-}
type alias Markup =
    List Text


{-| A text element can be a verbatim element, raw text, or an annotation. Issues with
parsing are represented with `InlineProblem`.
-}
type Text
    = Raw String
    | Verbatim Char (Loc String)
    | Annotation (Loc String) (List Text) (Maybe (Loc String)) (Maybe (Loc Command))
    | InlineProblem Problem.Inline
