module CommonTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)
import Parser.Common exposing (..)


suite : Test
suite =
    describe "Parser.Common"
        [ describe "int"
            [ test "it matches an integer" <|
                \_ ->
                    int
                        |> parse "42yolo"
                        |> Expect.equal (Ok 42)
            , test "it fails on non integers" <|
                \_ ->
                    int
                        |> parse "yolo42"
                        |> Expect.equal (Err "expected int")
            ]
        , describe "space"
            [ test "matches a space" <|
                \_ ->
                    space
                        |> parse " "
                        |> Expect.equal (Ok ' ')
            ]
        , describe "tab"
            [ test "matches a tab" <|
                \_ ->
                    tab
                        |> parse "\t"
                        |> Expect.equal (Ok '\t')
            ]
        , describe "blank"
            [ test "matches a space" <|
                \_ ->
                    blank
                        |> parse " "
                        |> Expect.equal (Ok ' ')
            , test "matches a tab" <|
                \_ ->
                    blank
                        |> parse "\t"
                        |> Expect.equal (Ok '\t')
            ]
        , describe "blanks"
            [ test "matches tabs and spaces" <|
                \_ ->
                    blanks
                        |> parse " \t  \t"
                        |> Expect.equal (Ok " \t  \t")
            , test "always succeeds" <|
                \_ ->
                    blanks
                        |> parse "x"
                        |> Expect.equal (Ok "")
            ]
        , describe "newline"
            [ test "matches newline" <|
                \_ ->
                    newline
                        |> parse "\n"
                        |> Expect.equal (Ok '\n')
            ]
        ]
