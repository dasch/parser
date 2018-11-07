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
        , describe "oneOf"
            [ test "it can pick the first choice" <|
                \_ ->
                    Parser.oneOf [ Parser.string "hello", Parser.string "goodbye" ]
                        |> Parser.andThen (\_ -> Parser.anyChar)
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok ' ')
            , test "it can pick the second choice" <|
                \_ ->
                    Parser.oneOf [ Parser.string "goodbye", Parser.string "hello" ]
                        |> Parser.andThen (\_ -> Parser.anyChar)
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok ' ')
            ]
        , describe "zeroOrMore"
            [ test "it succeeds if there are no matches" <|
                \_ ->
                    Parser.zeroOrMore (Parser.char 'x')
                        |> Parser.parse "yyy"
                        |> Expect.equal (Ok [])
            , test "it matches one time" <|
                \_ ->
                    Parser.zeroOrMore (Parser.char 'x')
                        |> Parser.parse "xyy"
                        |> Expect.equal (Ok [ () ])
            ]
        , describe "oneOrMore"
            [ test "it fails if there are no matches" <|
                \_ ->
                    Parser.oneOrMore (Parser.char 'x')
                        |> Parser.parse "yyy"
                        |> Expect.equal (Err "expected char")
            , test "it matches one time" <|
                \_ ->
                    Parser.oneOrMore (Parser.char 'x')
                        |> Parser.parse "xyy"
                        |> Expect.equal (Ok [ () ])
            , test "it matches many times" <|
                \_ ->
                    Parser.oneOrMore (Parser.char 'x')
                        |> Parser.parse "xxy"
                        |> Expect.equal (Ok [ (), () ])
            ]
        ]
