module Camperdown.Config exposing (Config)

{-|

@docs Config

-}

import Camperdown.Occurs exposing (Occurs)
import Set exposing (Set)


{-| Markup parsing configuration

  - `lineOffset`: difference between what the parser will report as a line position
    and the actual line position, since this is used to parse file fragments
  - `escapable`: Escapable characters in markup text are:
      - Always: `\`, \`\`\``,`!`,`#`,`[`,`]`,`(`,`)\`
      - Verbatim or annotation capture characters
      - The first character in a replacement

-}
type alias Config =
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
