module Camperdown.Parse.Offset exposing (Offset, locate, position)

import Camperdown.Loc as Loc exposing (Loc)


{-| The parser need to know the difference between the string they _think_ they're
parsing and the file they're _actually_ parsing. This is contained in the `Offset`
-}
type alias Offset =
    { line : Int }


{-| Given a file offset and a parser-library reported position, return a Loc.
-}
locate : Offset -> ( Int, Int ) -> a -> ( Int, Int ) -> Loc a
locate { line } ( startLine, startColumn ) x ( endLine, endColumn ) =
    {- The two instances of `- 1` here need explanation. The elm parser appears
       to correct for the fact that line "zero" should be reported as actually being
       on line 1, which gets fed into endLine. So therefore, the very first character
       in the string will be on line 1, position 1.

       But the Offset's `line` also reports the one-indexed line. Therefore, the actual
       first character in the file would have an `offset.line` of `1` and a
       `startLine` of `1`, and would therefore be reported as being on line 2.

       There are a couple of places that this could be addressed, but addressing
       it here and in the subsequent `position` function seemed the least damaging. - RJS
    -}
    Loc.locate
        { line = startLine + line - 1, column = startColumn }
        { line = endLine + line - 1, column = endColumn }
        x


{-| Given a file offset and a parser-library reported position, return a Position. USE SPARINGLY.
-}
position : Offset -> ( Int, Int ) -> Loc.Position
position { line } ( posLine, posColumn ) =
    { line = posLine + line - 1, column = posColumn }
