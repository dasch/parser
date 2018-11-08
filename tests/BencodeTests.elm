module BencodeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Examples.Bencode exposing (..)


suite : Test
suite =
    describe "Bencode"
        [ describe "integers"
            [ test "parsing integers" <|
                \_ ->
                    parse "i42e"
                        |> Expect.equal (Ok (BInt 42))
            ]
        ]
