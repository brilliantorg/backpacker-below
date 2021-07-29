module ParserTests exposing (..)

import ASTHelper exposing (..)
import Camperdown.Config.Config exposing (config)
import Camperdown.Config.Configurations exposing (markup, minimal)
import Camperdown.Parse exposing (parse)
import Camperdown.Parse.Syntax exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra
import Maybe.Extra
import Test exposing (..)


suite : Test
suite =
    describe "The Camperdown parser"
        [ describe "at top level"
            [ test "recognizes the prelude" <|
                \_ ->
                    parse markup "Test"
                        |> .prelude
                        |> Expect.equal
                            [ Paragraph { contents = [ Raw "Test" ] } ]
            , test "recognizes sections" <|
                \_ ->
                    parse markup "Test\n\n# A\n\n# B"
                        |> .sections
                        |> List.length
                        |> Expect.equal
                            2
            , test "fails if there is space before '#'" <|
                \_ ->
                    parse markup "Test\n\n# A\n\n # B"
                        |> .sections
                        |> List.map .contents
                        |> List.map hasProblem
                        |> someAreTrue
                        |> Expect.equal True
            ]
        , describe "recognizes commmands"
            [ test "such as !" <|
                \_ ->
                    parse minimal "! foo"
                        |> preludeElement 0
                        |> Maybe.map isCommand
                        |> Expect.equal (Just True)
            , test "such as ?" <|
                \_ ->
                    parse minimal "? foo"
                        |> preludeElement 0
                        |> Maybe.map isCommand
                        |> Expect.equal (Just True)
            , test "but not  >" <|
                \_ ->
                    parse minimal "> foo"
                        |> preludeElement 0
                        |> Maybe.map isCommand
                        |> Expect.notEqual (Just True)
            , test "because > is nothing special and here just begins a paragraph" <|
                \_ ->
                    parse minimal "> foo"
                        |> preludeElement 0
                        |> Maybe.map isParagraph
                        |> Expect.equal (Just True)
            , test "commands have children.  But note that the child text is indented" <|
                \_ ->
                    parse markup "! foo >>\n  bar"
                        |> preludeElement 0
                        |> Maybe.andThen commandChild
                        |> Expect.equal (Just (Nested [ Paragraph { contents = [ Raw "bar" ] } ]))
            , test "text following a command after '..' must be indented for the nested content to be non-empty" <|
                \_ ->
                    parse markup "! foo >>\nbar"
                        |> preludeElement 0
                        |> Maybe.andThen commandChild
                        |> Expect.equal
                            (Just (Nested []))
            , test "the text following the >> may consist of several paragraphs (in this case two paragraphs)" <|
                \_ ->
                    parse markup "! foo >>\n  bar\n\n  baz"
                        |> preludeElement 0
                        |> Maybe.andThen commandChild
                        |> Maybe.map (nestedElements >> List.length)
                        |> Expect.equal (Just 2)
            , test "commands may be immediate, signaled by vv instead of >>. We check for the correct children" <|
                \_ ->
                    parse markup "! choice vv\n  yada"
                        |> preludeElement 0
                        |> Maybe.andThen commandChild
                        |> Expect.equal (Just (Immediate [ Paragraph { contents = [ Raw " ", Raw "  yada" ] } ]))
            ]
        , describe "recognizes annotations"
            [ test "such as (_test_) with config markup" <|
                \_ ->
                    parse markup "_test_"
                        |> .prelude
                        |> List.map paragraphContents
                        |> List.head
                        |> Maybe.Extra.join
                        |> Maybe.map (List.map (matchAnnotation "_" "test" "_"))
                        |> Expect.equal
                            (Just [ True ])
            ]
        , describe "recognizes verbatim constructs"
            [ test "with prefix ```" <|
                \_ ->
                    parse minimal "```\nfoo\nbar"
                        |> Expect.equal
                            { prelude =
                                [ Preformatted { contents = "```", indent = 0, lines = { end = 1, start = 1 } }
                                , Paragraph { contents = [ Raw "foo ", Raw "bar" ] }
                                ]
                            , sections = []
                            }
            , test "with prefix ``` (use isPreformatted to test)" <|
                \_ ->
                    parse minimal "```\nfoo\nbar"
                        |> preludeElement 0
                        |> Maybe.map isPreformatted
                        |> Expect.equal
                            (Just True)
            , test "with prefix %%%" <|
                \_ ->
                    parse minimal "%%%\nfoo\nbar"
                        |> preludeElement 0
                        |> Maybe.map isPreformatted
                        |> Expect.equal
                            (Just True)
            , test "with non-verbatim prefix %&%" <|
                \_ ->
                    parse minimal "%&%\nfoo\nbar"
                        |> preludeElement 0
                        |> Maybe.map isPreformatted
                        |> Expect.notEqual
                            (Just True)
            ]
        , describe "handles text with markup"
            [ test "plain text is captured as as a paragraph" <|
                \_ ->
                    parse markup "test"
                        |> preludeElement 0
                        |> Maybe.map isParagraph
                        |> Expect.equal (Just True)
            , test "capturing the actual test in Raw" <|
                \_ ->
                    parse markup "test"
                        |> preludeElement 0
                        |> Maybe.andThen paragraphContents
                        |> Expect.equal
                            (Just [ Raw "test" ])
            , test ",capturing Annotations (*stuff*)" <|
                \_ ->
                    parse markup "test *stuff*"
                        |> preludeElement 0
                        |> Maybe.andThen paragraphContents
                        |> Maybe.andThen (List.Extra.getAt 1)
                        |> Maybe.map isAnnotation
                        |> Expect.equal (Just True)
            , test ", recognizing inline verbatim text" <|
                \_ ->
                    parse markup "*stuff"
                        |> preludeElement 0
                        |> Maybe.andThen paragraphContents
                        |> Maybe.map (List.map isVerbatim >> someAreTrue)
                        |> Expect.equal (Just True)
            , test "and of course flagging errors (*stuff)" <|
                \_ ->
                    parse markup "*stuff"
                        |> preludeElement 0
                        |> Maybe.andThen paragraphContents
                        |> Maybe.map (List.map isInlineProblem >> someAreTrue)
                        |> Expect.equal (Just True)
            ]
        ]
