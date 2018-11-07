module ParserTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser


suite : Test
suite =
    describe "Parser"
        [ describe "peek"
            [ test "returns the next n characters" <|
                \_ ->
                    Parser.init "hello world"
                        |> Parser.peek 5
                        |> Expect.equal (String.toList "hello")
            ]
        , describe "char"
            [ test "matches the exact characters" <|
                \_ ->
                    Parser.char 'h'
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok ())
            , test "advances the position" <|
                \_ ->
                    Parser.char 'h'
                        |> Parser.andThen (\_ -> Parser.anyChar)
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok 'e')
            ]
        , describe "string"
            [ test "matches the exact characters" <|
                \_ ->
                    Parser.string "hello"
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok ())
            , test "advances the position" <|
                \_ ->
                    Parser.string "hell"
                        |> Parser.andThen (\_ -> Parser.anyChar)
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok 'o')
            ]
        ]
