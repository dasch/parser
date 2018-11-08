module ParserTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (..)


suite : Test
suite =
    describe "Parser"
        [ describe "peek"
            [ test "returns the next n characters" <|
                \_ ->
                    init "hello world"
                        |> peek 5
                        |> Expect.equal (String.toList "hello")
            ]
        , describe "char"
            [ test "matches the exact characters" <|
                \_ ->
                    char 'h'
                        |> parse "hello world"
                        |> Expect.equal (Ok 'h')
            , test "advances the position" <|
                \_ ->
                    char 'h'
                        |> andThen (\_ -> anyChar)
                        |> parse "hello world"
                        |> Expect.equal (Ok 'e')
            ]
        , describe "chomp"
            [ test "matches n chars" <|
                \_ ->
                    chomp 5
                        |> parse "hello world"
                        |> Expect.equal (Ok "hello")
            ]
        , describe "maybe"
            [ test "succeeds with Nothing if the parser fails" <|
                \_ ->
                    maybe (char 'x')
                        |> parse "y"
                        |> Expect.equal (Ok Nothing)
            , test "succeeds with Just x if the parser succeeds with x" <|
                \_ ->
                    maybe (char 'x')
                        |> parse "x"
                        |> Expect.equal (Ok (Just 'x'))
            ]
        , describe "string"
            [ test "matches the exact characters" <|
                \_ ->
                    string "hello"
                        |> parse "hello world"
                        |> Expect.equal (Ok "hello")
            , test "advances the position" <|
                \_ ->
                    string "hell"
                        |> andThen (\_ -> anyChar)
                        |> parse "hello world"
                        |> Expect.equal (Ok 'o')
            ]
        , describe "end"
            [ test "matches the end of the input" <|
                \_ ->
                    string "hello"
                        |> andThen (\_ -> end)
                        |> parse "hello"
                        |> Expect.equal (Ok ())
            , test "does not match anything but the end of the input" <|
                \_ ->
                    end
                        |> parse "hello"
                        |> Expect.equal (Err "expected end")
            ]
        , describe "oneOf"
            [ test "it can pick the first choice" <|
                \_ ->
                    oneOf [ string "hello", string "goodbye" ]
                        |> andThen (\_ -> anyChar)
                        |> parse "hello world"
                        |> Expect.equal (Ok ' ')
            , test "it can pick the second choice" <|
                \_ ->
                    oneOf [ string "goodbye", string "hello" ]
                        |> andThen (\_ -> anyChar)
                        |> parse "hello world"
                        |> Expect.equal (Ok ' ')
            , test "it fails if none of the choices succeed" <|
                \_ ->
                    oneOf [ string "goodbye", string "hello" ]
                        |> andThen (\_ -> anyChar)
                        |> parse "yolo"
                        |> Expect.equal (Err "expected one of the parsers to match")
            ]
        , describe "zeroOrMore"
            [ test "it succeeds if there are no matches" <|
                \_ ->
                    zeroOrMore (char 'x')
                        |> parse "yyy"
                        |> Expect.equal (Ok [])
            , test "it matches one time" <|
                \_ ->
                    zeroOrMore (char 'x')
                        |> parse "xyy"
                        |> Expect.equal (Ok [ 'x' ])
            , test "it matches many times" <|
                \_ ->
                    zeroOrMore (char 'x')
                        |> parse "xxy"
                        |> Expect.equal (Ok [ 'x', 'x' ])
            ]
        , describe "oneOrMore"
            [ test "it fails if there are no matches" <|
                \_ ->
                    oneOrMore (char 'x')
                        |> parse "yyy"
                        |> Expect.equal (Err "expected char x")
            , test "it matches one time" <|
                \_ ->
                    oneOrMore (char 'x')
                        |> parse "xyy"
                        |> Expect.equal (Ok [ 'x' ])
            , test "it matches many times" <|
                \_ ->
                    oneOrMore (char 'x')
                        |> parse "xxy"
                        |> Expect.equal (Ok [ 'x', 'x' ])
            ]
        , describe "followedBy"
            [ test "allows chaining more parsers" <|
                \_ ->
                    succeed (\x y -> ( x, y ))
                        |> followedBy (char 'x')
                        |> followedBy (char 'y')
                        |> parse "xyz"
                        |> Expect.equal (Ok ( 'x', 'y' ))
            ]
        , describe "ignoring"
            [ test "allows ignoring parsers in chains" <|
                \_ ->
                    succeed (\x y -> ( x, y ))
                        |> followedBy (char 'x')
                        |> ignoring (char 'y')
                        |> followedBy (char 'z')
                        |> parse "xyz"
                        |> Expect.equal (Ok ( 'x', 'z' ))
            ]
        , describe "int"
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
