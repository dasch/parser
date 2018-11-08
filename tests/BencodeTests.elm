module BencodeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Examples.Bencode exposing (..)


suite : Test
suite =
    describe "Bencode"
        [ describe "integers"
            [ test "valid integers" <|
                \_ ->
                    parse "i42e"
                        |> Expect.equal (Ok (BInt 42))
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
        , describe "lists"
            [ test "empty list" <|
                \_ ->
                    parse "le"
                        |> Expect.equal (Ok (BList []))
            , test "valid lists" <|
                \_ ->
                    parse "li42ei13ee"
                        |> Expect.equal (Ok (BList [ BInt 42, BInt 13 ]))
            ]
        ]
