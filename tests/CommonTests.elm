module CommonTests exposing (expectMatch, suite)

import Expect exposing (Expectation)
import Parser exposing (..)
import Parser.Common exposing (..)
import Test exposing (..)
import Url


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
                        |> Result.mapError .message
                        |> Expect.equal (Err "expected int")
            ]
        , describe "float"
            [ test "it matches valid floats" <|
                \_ ->
                    let
                        examples =
                            [ 42.13
                            , -13.5
                            , 13.5
                            ]
                    in
                    expectFloats examples float
            ]
        , describe "word"
            [ test "matches words" <|
                \_ ->
                    let
                        examples =
                            [ "xy123"
                            , "_h_"
                            , "_"
                            , "hello_world"
                            ]
                    in
                    expectMatch examples word
            , test "fails on non-word char" <|
                \_ ->
                    word
                        |> parse "%hello"
                        |> Result.mapError .message
                        |> Expect.equal (Err "expected word")
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
        , describe "iso8601" <|
            let
                examples =
                    [ "1970-01-01"
                    , "1970-01-01T00:00:00.000Z"
                    , "1970-01-01T00:00:00Z"
                    , "2012-04-01T00:00:00-05:00"
                    , "2012-11-12T00:00:00+01:00"
                    , "2018-08-31T23:25:16.019345+02:00"
                    , "2018-08-31T23:25:16.019345123+02:00"
                    ]

                testFor example =
                    test ("matches " ++ example) <|
                        \_ ->
                            Expect.equal (Ok example) (parse example parser)

                parser =
                    into identity
                        |> grab iso8601
                        |> ignore end
            in
            List.append (List.map testFor examples)
                [ test "+1:00 is an invalid UTC offset (should be +01:00)" <|
                    \_ ->
                        parse "2012-04-01T00:00:00+1:00" (iso8601 |> followedBy end)
                            |> Expect.err
                , test "-5:00 is an invalid UTC offset (should be -05:00)" <|
                    \_ ->
                        parse "2012-04-01T00:00:00-5:00" (iso8601 |> followedBy end)
                            |> Expect.err
                ]
        , describe "url"
            [ test "parses URLs" <|
                \_ ->
                    let
                        example =
                            "http://hello.com/world?x=42&y=13#nice"
                    in
                    parse example url
                        |> Result.map Url.toString
                        |> Expect.equal (Ok example)
            , test "fails on invalid URLs" <|
                \_ ->
                    parse "omg://yolo.com" url
                        |> Expect.err
            ]
        ]


expectMatch examples parser =
    Expect.all (List.map (\example p -> Expect.ok (parse example p)) examples) parser


expectFloats examples parser =
    Expect.all (List.map (\example p -> Expect.equal (Ok example) (parse (String.fromFloat example) p)) examples) parser
