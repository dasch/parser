module InternetTests exposing (suite)

import Expect exposing (Expectation)
import Parser exposing (..)
import Parser.Internet exposing (..)
import Test exposing (..)
import Url


suite : Test
suite =
    describe "Parser.Internet"
        [ describe "url"
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
