module Camperdown.Config.Config exposing (config, config2)

{-|

@docs config, config2

-}

import Camperdown.Config exposing (Config)
import Camperdown.Occurs exposing (Occurs(..))
import Set exposing (Set)



-- CONFIGURATION


{-| The standard configuration
-}
config : Config
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


{-| An alternate configuration
-}
config2 : Config
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
