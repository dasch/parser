module UrlTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Examples.Url exposing (parse)


suite : Test
suite =
    describe "Url"
        [ describe "parse"
            [ test "parses correct URLs" <|
                \_ ->
                    let
                        expected =
                            { protocol = "https"
                            , host = "hello.com"
                            , port_ = Just 123
                            , path = "/greetings"
                            , query = Just "recipient=world"
                            , fragment = Just "message"
                            }
                    in
                        parse "https://hello.com:123/greetings?recipient=world#message"
                            |> Expect.equal (Ok expected)
            ]
        ]
