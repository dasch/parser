module TomlTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Examples.Toml exposing (..)


suite : Test
suite =
    describe "Toml"
        [ describe "integers"
            [ test "valid integers" <|
                \_ ->
                    parse "42"
                        |> expectValue (TomlInt 42)
            ]
        , describe "strings"
            [ test "empty string" <|
                \_ ->
                    parse "\"\""
                        |> expectValue (TomlString "")
            , test "valid strings" <|
                \_ ->
                    parse "\"hello\""
                        |> expectValue (TomlString "hello")
            ]
        , describe "key/value"
            [ test "simple assignment" <|
                \_ ->
                    parse "hello = 42"
                        |> expectValue (TomlKv "hello" (TomlInt 42))
            , test "leading spaces" <|
                \_ ->
                    parse " \thello = 42"
                        |> expectValue (TomlKv "hello" (TomlInt 42))
            , test "trailing spaces" <|
                \_ ->
                    parse "hello = 42  \n"
                        |> expectValue (TomlKv "hello" (TomlInt 42))
            ]
        , describe "tables"
            [ test "with no key/value pairs" <|
                \_ ->
                    parse "[greetings]"
                        |> expectValue (TomlKv "greetings" (TomlTable []))
            , test "with key/value pairs" <|
                \_ ->
                    parse """
                    [greetings]
                    hello = 42
                    world = 13
                    """
                        |> expectValue
                            (TomlKv "greetings"
                                (TomlTable
                                    [ TomlKv "hello" (TomlInt 42)
                                    , TomlKv "world" (TomlInt 13)
                                    ]
                                )
                            )
            ]
        ]


expectValue : Value -> Result String Value -> Expectation
expectValue expected =
    Expect.equal (Ok expected)
