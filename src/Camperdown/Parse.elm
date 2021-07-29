module Camperdown.Parse exposing (parse)

{-| Placeholder 

@docs parse
-}

import Camperdown.Config.Config as Config
import Camperdown.Parse.Hierarchy as Hierarchy
import Camperdown.Parse.Pieces as Pieces
import Camperdown.Parse.Sections as Sections
import Camperdown.Parse.Syntax as Syn


{-| Invokes the internal parsing infrastructure to parse a complete file.

A document is split into sections by lines that start with `#`. This function returns
the elements in the prelude (part of the file before the first `#` marker) followed by
the parsed sections.

-}
parse : Config.ParserConfig -> String -> Syn.Document
parse config file =
    let
        {- Parsing pipeline:

           - Use lines that start with a `#` to split the document into _sections_ (`Parse.Section`)
           - Use indentation to split the document into _pieces_ (`Parse.Pieces`)
           - Reorganize the document according to its hierarchical structure (`Parse.Hierarchy`)

           See PARSE.md for details

        -}
        rawparsed =
            Sections.splitIntoSections file

        prelude =
            Pieces.pieces config rawparsed.prelude
                |> Hierarchy.elements

        sections =
            List.map
                (\section ->
                    let
                        problems =
                            List.map
                                (\loc ->
                                    Syn.Problem
                                        { lines = { start = loc.start.line, end = loc.end.line }
                                        , indent = loc.start.column
                                        , loc = loc
                                        , problem = "A space is required between the `#` markers and the section label."
                                        }
                                )
                                section.problems

                        contents =
                            Pieces.pieces config section.contents
                                |> Hierarchy.elements
                    in
                    { level = section.level
                    , label = section.label
                    , contents = problems ++ contents
                    }
                )
                rawparsed.sections
    in
    { prelude = prelude, sections = sections }
