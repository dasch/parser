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
                        |> Expect.equal (Ok 'h')
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
                        |> Expect.equal (Ok "hello")
            , test "advances the position" <|
                \_ ->
                    Parser.string "hell"
                        |> Parser.andThen (\_ -> Parser.anyChar)
                        |> Parser.parse "hello world"
                        |> Expect.equal (Ok 'o')
            ]
        , describe "end"
            [ test "matches the end of the input" <|
                \_ ->
                    Parser.string "hello"
                        |> Parser.andThen (\_ -> Parser.end)
                        |> Parser.parse "hello"
                        |> Expect.equal (Ok ())
            , test "does not match anything but the end of the input" <|
                \_ ->
                    Parser.end
                        |> Parser.parse "hello"
                        |> Expect.equal (Err "expected end")
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
            , test "it fails if none of the choices succeed" <|
                \_ ->
                    Parser.oneOf [ Parser.string "goodbye", Parser.string "hello" ]
                        |> Parser.andThen (\_ -> Parser.anyChar)
                        |> Parser.parse "yolo"
                        |> Expect.equal (Err "expected one of the parsers to match")
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
                        |> Expect.equal (Ok [ 'x' ])
            , test "it matches many times" <|
                \_ ->
                    Parser.zeroOrMore (Parser.char 'x')
                        |> Parser.parse "xxy"
                        |> Expect.equal (Ok [ 'x', 'x' ])
            ]
        , describe "oneOrMore"
            [ test "it fails if there are no matches" <|
                \_ ->
                    Parser.oneOrMore (Parser.char 'x')
                        |> Parser.parse "yyy"
                        |> Expect.equal (Err "expected char x")
            , test "it matches one time" <|
                \_ ->
                    Parser.oneOrMore (Parser.char 'x')
                        |> Parser.parse "xyy"
                        |> Expect.equal (Ok [ 'x' ])
            , test "it matches many times" <|
                \_ ->
                    Parser.oneOrMore (Parser.char 'x')
                        |> Parser.parse "xxy"
                        |> Expect.equal (Ok [ 'x', 'x' ])
            ]
        , describe "int"
            [ test "it matches an integer" <|
                \_ ->
                    Parser.int
                        |> Parser.parse "42yolo"
                        |> Expect.equal (Ok 42)
            , test "it fails on non integers" <|
                \_ ->
                    Parser.int
                        |> Parser.parse "yolo42"
                        |> Expect.equal (Err "expected int")
            ]
        ]
