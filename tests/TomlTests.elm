module TomlTests exposing (..)

import Expect exposing (Expectation)
import Dict
import Test exposing (..)
import Examples.Toml exposing (..)


suite : Test
suite =
    describe "Toml"
        [ describe "integers"
            [ test "valid integers" <|
                \_ ->
                    parseValue "42"
                        |> expectValue (TomlInt 42)
            ]
        , describe "strings"
            [ test "empty string" <|
                \_ ->
                    parseValue "\"\""
                        |> expectValue (TomlString "")
            , test "valid strings" <|
                \_ ->
                    parseValue "\"hello\""
                        |> expectValue (TomlString "hello")
            , test "multiline strings" <|
                \_ ->
                    parseValue multilineString
                        |> expectValue (TomlString "hello\n")
            ]
        , describe "key/value"
            [ test "simple assignment" <|
                \_ ->
                    parse "hello = 42"
                        |> expectValue (TomlTable (Dict.fromList [ ( "hello", TomlInt 42 ) ]))
            , test "leading spaces" <|
                \_ ->
                    parse " \thello = 42"
                        |> expectValue (TomlTable (Dict.fromList [ ( "hello", TomlInt 42 ) ]))
            , test "trailing spaces" <|
                \_ ->
                    parse "hello = 42  \n"
                        |> expectValue (TomlTable (Dict.fromList [ ( "hello", TomlInt 42 ) ]))
            ]
        , describe "booleans"
            [ test "true" <|
                \_ ->
                    parseValue "true"
                        |> expectValue (TomlBool True)
            , test "false" <|
                \_ ->
                    parseValue "false"
                        |> expectValue (TomlBool False)
            ]
        , tableTests
        , describe "arrays"
            [ test "empty array" <|
                \_ ->
                    parseValue "[]"
                        |> expectValue (TomlArray [])
            , test "one element array" <|
                \_ ->
                    parseValue "[42]"
                        |> expectValue (TomlArray [ TomlInt 42 ])
            , test "multi element array" <|
                \_ ->
                    parseValue "[ 42 , 13 ]"
                        |> expectValue (TomlArray [ TomlInt 42, TomlInt 13 ])
            ]
        ]


tableTests =
    describe "tables"
        [ test "with no key/value pairs" <|
            \_ ->
                parse "[greetings]"
                    |> expectValue
                        (TomlTable
                            (Dict.fromList
                                [ ( "greetings", TomlTable Dict.empty ) ]
                            )
                        )
        , test "with key/value pairs" <|
            \_ ->
                parse """
                    [greetings]
                    hello = 42
                    world = 13
                    """
                    |> expectValue
                        (TomlTable
                            (Dict.fromList
                                [ ( "greetings"
                                  , TomlTable
                                        (Dict.fromList
                                            [ ( "hello", TomlInt 42 )
                                            , ( "world", TomlInt 13 )
                                            ]
                                        )
                                  )
                                ]
                            )
                        )
        , test "multiple tables" <|
            \_ ->
                parse """
                            [greetings]
                            hello = 42

                    [places]
                    world = 13
                    """
                    |> expectValue
                        (TomlTable
                            (Dict.fromList
                                [ ( "greetings"
                                  , TomlTable
                                        (Dict.fromList [ ( "hello", TomlInt 42 ) ])
                                  )
                                , ( "places"
                                  , TomlTable
                                        (Dict.fromList [ ( "world", TomlInt 13 ) ])
                                  )
                                ]
                            )
                        )
        , test "with leading key/value pairs" <|
            \_ ->
                parse """
                            hello = 42

                    [places]
                    world = 13
                    """
                    |> expectValue
                        (TomlTable
                            (Dict.fromList
                                [ ( "hello", TomlInt 42 )
                                , ( "places"
                                  , TomlTable
                                        (Dict.fromList [ ( "world", TomlInt 13 ) ])
                                  )
                                ]
                            )
                        )
        ]


expectValue : Value -> Result String Value -> Expectation
expectValue expected =
    Expect.equal (Ok expected)


multilineString =
    """\"\"\"
hello
\"\"\"
"""
