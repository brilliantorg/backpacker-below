module Camperdown.Config.Configurations exposing (l0, markup, minimal, standard)

{-| All configs except 'built' yield 'True' on application of 'Check.isValid'
-}

import Camperdown.Config.Build as Build exposing (build, parserConfigFromResult)
import Camperdown.Config.Config exposing (ParserConfig)
import Camperdown.Occurs exposing (Occurs(..))
import Set


standard : ParserConfig
standard =
    Camperdown.Config.Config.config


markup : ParserConfig
markup =
    parserConfigFromResult <|
        build
            [ Build.annotation "$" "$" Never
            , Build.annotation "_" "_" Never
            , Build.annotation "*" "*" Never
            , Build.annotation "~" "~" Never
            , Build.annotation "[" "]" Always
            , Build.annotation "`" "`" Never
            ]


minimal : ParserConfig
minimal =
    Build.minimal


l0 : ParserConfig
l0 =
    { verbatimOpts = Set.fromList [ '`', '$' ]
    , annotationOpts =
        [ { startSymbol = "**", endSymbol = Just "**", commandOccursAfterwards = Never }

        -- , { startSymbol = "*", endSymbol = Just "*", commandOccursAfterwards = Never }
        , { startSymbol = "~", endSymbol = Just "~", commandOccursAfterwards = Never } -- Added by JC
        , { startSymbol = "__", endSymbol = Just "__", commandOccursAfterwards = Never }

        -- , { startSymbol = "_", endSymbol = Just "_", commandOccursAfterwards = Never }
        --, { startSymbol = "\"", endSymbol = Just "\"", commandOccursAfterwards = Never }
        , { startSymbol = "[", endSymbol = Just "]", commandOccursAfterwards = Never }
        , { startSymbol = "|", endSymbol = Just "|", commandOccursAfterwards = Never }
        , { startSymbol = "...", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "---", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "--", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "&", endSymbol = Nothing, commandOccursAfterwards = Sometimes }
        ]
    , -- This is the set of all of the first chars of annotations (start or end)
      -- XXX IS THIS TRUE???!?!
      -- It is critical that ']' NOT be in this set: that's how values like `! foo [some text]`
      -- stop parsing the embedded markup `some text` before grabbing onto the final `]`.
      annotationFirstChars = Set.fromList [ '_', '"', '[', '.', '&', '~', '|' ]
    , -- Above, `~' Added by JC
      -- defined by negation:
      -- *non*-meaningful chars can't possibly be the start of a command or interesting feature
      -- meaningful chars have to be explicitly gobbled somewhere or they'll be treated as unknown and
      -- unallowed markup errors
      --  - automatically meaningful: '\\', ']', '\n'
      --  - also meaningful: annotationFirstChars
      meaningful = Set.fromList [ '\\', '[', ']', '\n', '.', '`', '_', '"', '&', '~', '|' ]
    , -- Above, `~' Added by JC
      -- Escapable should be the union of meaningful chars, and first chars of starters
      --  - automatically escapable: '\\', '[', ']', '!', '?', ':', '(', ')'
      --  - also escapable: annotationFirstChars
      --  - also escapable: first chars of verbatimMarkers
      --  - also escapable: verbatimOpts
      escapable = Set.fromList [ '\\', '\'', '!', '?', '#', '[', ']', '(', ')', '.', '`', '$', '_', '"', '&', '|' ]
    , verbatimMarkers = [ "%%%", "$$$", "```" ]
    }
