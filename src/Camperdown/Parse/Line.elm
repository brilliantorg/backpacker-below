module Camperdown.Parse.Line exposing (FirstLine, Line, Lines, consJust, consNothing, findNext, reverse)

{-| An empty line has contents = Nothing
-}


type alias Line =
    { lineNum : Int
    , contents : Maybe { indent : Int, str : String }
    }


{-| A Line that doesn't have the option of being empty
-}
type alias FirstLine =
    { lineNum : Int
    , contents : { indent : Int, str : String }
    }


{-| Like "List Line" but with a type guarantee that the first line, if any, is nonempty.
-}
type alias Lines =
    Maybe ( FirstLine, List Line )


findNext : List Line -> Lines
findNext lines =
    case lines of
        [] ->
            Nothing

        { lineNum, contents } :: rest ->
            case contents of
                Nothing ->
                    findNext rest

                Just nonEmptyContents ->
                    Just ( { lineNum = lineNum, contents = nonEmptyContents }, rest )



{- These are utility functions for using Lines as accumulator in tail recursive
   functions (which means they need to be reversed after collection)
-}


consNothing : Lines -> Int -> Lines
consNothing accum lineNum =
    Maybe.map
        (\( first, rest ) ->
            ( first, { lineNum = lineNum, contents = Nothing } :: rest )
        )
        accum


consJust : Lines -> Int -> Int -> String -> Lines
consJust accum lineNum indent str =
    case accum of
        Nothing ->
            Just ( { lineNum = lineNum, contents = { indent = indent, str = str } }, [] )

        Just ( first, rest ) ->
            Just ( first, { lineNum = lineNum, contents = Just { indent = indent, str = str } } :: rest )


reverse : Lines -> Lines
reverse =
    Maybe.map (\( first, rest ) -> ( first, List.reverse rest ))
