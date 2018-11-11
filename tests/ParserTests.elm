module ParserTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
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
        , describe "when"
            [ test "succeeds when the predicate holds" <|
                \_ ->
                    when Char.isUpper
                        |> parse "Hello world"
                        |> Expect.equal (Ok 'H')
            , test "fails when the predicate fails" <|
                \_ ->
                    when Char.isUpper
                        |> parse "hello world"
                        |> Expect.equal (Err "char h failed predicate")
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
        , describe "grabbing"
            [ test "allows chaining more parsers" <|
                \_ ->
                    succeed (\x y -> ( x, y ))
                        |> grabbing (char 'x')
                        |> grabbing (char 'y')
                        |> parse "xyz"
                        |> Expect.equal (Ok ( 'x', 'y' ))
            ]
        , describe "ignore"
            [ test "allows ignore parsers in chains" <|
                \_ ->
                    succeed (\x y -> ( x, y ))
                        |> grabbing (char 'x')
                        |> ignore (char 'y')
                        |> grabbing (char 'z')
                        |> parse "xyz"
                        |> Expect.equal (Ok ( 'x', 'z' ))
            ]
        , describe "separatedBy"
            [ test "matches elements separated by the separator" <|
                \_ ->
                    separatedBy (char ',') (char 'x')
                        |> parse "x,x,x"
                        |> Expect.equal (Ok [ 'x', 'x', 'x' ])
            , test "matches a single element" <|
                \_ ->
                    separatedBy (char ',') (char 'x')
                        |> parse "xyz"
                        |> Expect.equal (Ok [ 'x' ])
            , test "succeeds even if no elements match" <|
                \_ ->
                    separatedBy (char ',') (char 'x')
                        |> parse "yz"
                        |> Expect.equal (Ok [])
            , test "stops when the element after the separator doesn't match" <|
                \_ ->
                    separatedBy (char ',') (char 'x')
                        |> parse "x,x,y"
                        |> Expect.equal (Ok [ 'x', 'x' ])
            ]
        , describe "except"
            [ test "does not match if that argument succeeds" <|
                \_ ->
                    except (char 'x')
                        |> parse "xyz"
                        |> Expect.equal (Err "expected to not match")
            , test "matches any char if the argument fails" <|
                \_ ->
                    except (char 'x')
                        |> parse "yz"
                        |> Expect.equal (Ok 'y')
            ]
        ]
