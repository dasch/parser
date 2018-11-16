module TomlTests exposing (expectValue, multilineString, suite, tableTests)

import Dict
import Examples.Toml exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Toml"
        [ test "example document" <|
            \_ ->
                parse example
                    |> Expect.ok
        , describe "integers"
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
        , describe "datetimes"
            [ test "valid iso8601 datetime" <|
                \_ ->
                    parseValue "1979-05-27T07:32:00Z"
                        |> expectValue (TomlDatetime (Time.millisToPosix 296638320000))
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
expectValue expected result =
    case result of
        Ok actual ->
            Expect.equal expected actual

        Err err ->
            Expect.fail err


multilineString =
    """\"\"\"
hello
\"\"\"
"""


example =
    """
# This is a TOML document.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]
"""
