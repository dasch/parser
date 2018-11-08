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
        ]


expectValue : Value -> Result String Value -> Expectation
expectValue expected =
    Expect.equal (Ok expected)
