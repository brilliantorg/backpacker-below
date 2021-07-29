module ConfigTest exposing (..)

import Camperdown.Config.Build as Build exposing (..)
import Camperdown.Config.Check exposing (check, isValid)
import Camperdown.Config.Configurations as Config
import Camperdown.Occurs exposing (Occurs(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Configuration builds and checks"
        [ describe "How builds can fail"
            -- Nest as many descriptions as you like.
            [ test "Build.minimal is a valid configuration" <|
                \_ ->
                    isValid Build.minimal
                        |> Expect.equal True
            , test "badBuildAnnotation produces an invalid configuration" <|
                \_ ->
                    (check <| buildSomething [ badAnnotation "_" "_" ])
                        |> Expect.equal
                            [ Ok "annotationFirstChars does not contain ']'"
                            , Err "These characters appear in 'annotationOpts' but not in 'annotationFirstChars': _"
                            , Err "These characters appear in 'annotationOpts' but not in 'meaningful': _"
                            ]
            , test "badBuildAnnotation fails 'isConsistent'" <|
                \_ ->
                    (isValid <| buildSomething [ badAnnotation "_" "_" ])
                        |> Expect.equal False
            , test "buildAnnotation passes 'isConsistent'" <|
                \_ ->
                    (isValid <| buildSomething [ annotation "_" "_" Never ])
                        |> Expect.equal True
            ]
        ]
