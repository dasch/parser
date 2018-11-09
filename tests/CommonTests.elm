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
        ]
