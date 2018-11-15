module BencodeTests exposing (expectValue, suite)

import Dict
import Examples.Bencode exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Bencode"
        [ describe "integers"
            [ test "valid integers" <|
                \_ ->
                    parse "i42e"
                        |> expectValue (BInt 42)
            , test "invalid char at beginning" <|
                \_ ->
                    parse "ix42e"
                        |> Expect.err
            , test "invalid char at end" <|
                \_ ->
                    parse "i42xe"
                        |> Expect.err
            , test "invalid char in middle" <|
                \_ ->
                    parse "i4x2e"
                        |> Expect.err
            ]
        , describe "strings"
            [ test "valid string" <|
                \_ ->
                    parse "5:hello"
                        |> expectValue (BString "hello")
            , test "empty string" <|
                \_ ->
                    parse "0:"
                        |> expectValue (BString "")
            ]
        , describe "lists"
            [ test "empty list" <|
                \_ ->
                    parse "le"
                        |> expectValue (BList [])
            , test "valid lists" <|
                \_ ->
                    parse "li42ei13ee"
                        |> expectValue (BList [ BInt 42, BInt 13 ])
            ]
        , describe "dicts"
            [ test "empty dict" <|
                \_ ->
                    parse "de"
                        |> expectValue (BDict Dict.empty)
            , test "valid dicts" <|
                \_ ->
                    parse "d5:helloi42e5:worldi13ee"
                        |> expectValue (BDict (Dict.fromList [ ( "hello", BInt 42 ), ( "world", BInt 13 ) ]))
            ]
        ]


expectValue : Value -> Result String Value -> Expectation
expectValue expected =
    Expect.equal (Ok expected)
