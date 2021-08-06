module Camperdown.Config.Config exposing (ParserConfig, config, config2)

{-|

@docs ParserConfig, config, config2

-}

import Camperdown.Occurs exposing (Occurs(..))
import Set exposing (Set)



-- CONFIGURATION


{-| Markup parsing configuration

  - `lineOffset`: difference between what the parser will report as a line position
    and the actual line position, since this is used to parse file fragments
  - `escapable`: Escapable characters in markup text are:
      - Always: `\`, \`\`\``,`!`,`#`,`[`,`]`,`(`,`)\`
      - Verbatim or annotation capture characters
      - The first character in a replacement

-}
type alias ParserConfig =
    { verbatimOpts : Set Char
    , annotationOpts :
        List
            { startSymbol : String
            , endSymbol : Maybe String
            , commandOccursAfterwards : Occurs
            }
    , annotationFirstChars : Set Char
    , escapable : Set Char
    , meaningful : Set Char
    , verbatimMarkers : List String
    }

{-| The standard configuration -}
config : ParserConfig
config =
    { verbatimOpts = Set.fromList [ '`', '$' ]
    , annotationOpts =
        [ { startSymbol = "**", endSymbol = Just "**", commandOccursAfterwards = Never }
        , { startSymbol = "*", endSymbol = Just "*", commandOccursAfterwards = Never }
        , { startSymbol = "~", endSymbol = Just "~", commandOccursAfterwards = Never } -- Added by JC
        , { startSymbol = "__", endSymbol = Just "__", commandOccursAfterwards = Never }
        , { startSymbol = "_", endSymbol = Just "_", commandOccursAfterwards = Never }
        , { startSymbol = "/", endSymbol = Just "/", commandOccursAfterwards = Never }
        , { startSymbol = "\"", endSymbol = Just "\"", commandOccursAfterwards = Never }
        , { startSymbol = "[", endSymbol = Just "]", commandOccursAfterwards = Always }
        , { startSymbol = "...", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "---", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "--", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "&", endSymbol = Nothing, commandOccursAfterwards = Sometimes }
        ]
    , -- This is the set of all of the first chars of annotations (start or end)
      -- XXX IS THIS TRUE???!?!
      -- It is critical that ']' NOT be in this set: that's how values like `! foo [some text]`
      -- stop parsing the embedded markup `some text` before grabbing onto the final `]`.
      annotationFirstChars = Set.fromList [ '*', '_', '/', '"', '[', '.', '-', '&', '~' ]
    , -- Above, `~' Added by JC
      -- defined by negation:
      -- *non*-meaningful chars can't possibly be the start of a command or interesting feature
      -- meaningful chars have to be explicitly gobbled somewhere or they'll be treated as unknown and
      -- unallowed markup errors
      --  - automatically meaningful: '\\', ']', '\n'
      --  - also meaningful: annotationFirstChars
      meaningful = Set.fromList [ '\\', '[', ']', '\n', '.', '-', '`', '*', '_', '/', '"', '&', '~' ]
    , -- Above, `~' Added by JC
      -- Escapable should be the union of meaningful chars, and first chars of starters
      --  - automatically escapable: '\\', '[', ']', '!', '?', ':', '(', ')'
      --  - also escapable: annotationFirstChars
      --  - also escapable: first chars of verbatimMarkers
      --  - also escapable: verbatimOpts
      escapable = Set.fromList [ '\\', '\'', '!', '?', '#', '[', ']', '(', ')', '.', '-', '`', '$', '*', '_', '/', '"', '&' ]
    , verbatimMarkers = [ "%%%", "$$$", "```" ]
    }

{-| An alternate configuration -}
config2 : ParserConfig
config2 =
    { verbatimOpts = Set.fromList [ '`', '$' ]
    , annotationOpts =
        [ { startSymbol = "*", endSymbol = Just "*", commandOccursAfterwards = Never }
        , { startSymbol = "~", endSymbol = Just "~", commandOccursAfterwards = Never } -- Added by JC
        , { startSymbol = "_", endSymbol = Just "_", commandOccursAfterwards = Never }
        , { startSymbol = "/", endSymbol = Just "/", commandOccursAfterwards = Never }
        , { startSymbol = "\"", endSymbol = Just "\"", commandOccursAfterwards = Never }
        , { startSymbol = "[", endSymbol = Just "]", commandOccursAfterwards = Always }
        ]
    , -- This is the set of all of the first chars of annotations (start or end)
      -- XXX IS THIS TRUE???!?!
      -- It is critical that ']' NOT be in this set: that's how values like `! foo [some text]`
      -- stop parsing the embedded markup `some text` before grabbing onto the final `]`.
      annotationFirstChars = Set.fromList [ '*', '_', '/', '"', '[', '.', '-', '&', '~' ]
    , -- Above, `~' Added by JC
      -- defined by negation:
      -- *non*-meaningful chars can't possibly be the start of a command or interesting feature
      -- meaningful chars have to be explicitly gobbled somewhere or they'll be treated as unknown and
      -- unallowed markup errors
      --  - automatically meaningful: '\\', ']', '\n'
      --  - also meaningful: annotationFirstChars
      meaningful = Set.fromList [ '\\', '[', ']', '\n', '.', '-', '`', '*', '_', '/', '"', '&', '~' ]
    , -- Above, `~' Added by JC
      -- Escapable should be the union of meaningful chars, and first chars of starters
      --  - automatically escapable: '\\', '[', ']', '!', '?', ':', '(', ')'
      --  - also escapable: annotationFirstChars
      --  - also escapable: first chars of verbatimMarkers
      --  - also escapable: verbatimOpts
      escapable = Set.fromList [ '\\', '\'', '!', '?', '#', '[', ']', '(', ')', '.', '-', '`', '$', '*', '_', '/', '"', '&' ]
    , verbatimMarkers = [ "%%%", "$$$", "```" ]
    }
