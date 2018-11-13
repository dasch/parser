module TreeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing (parse)
import Examples.Tree exposing (..)


suite : Test
suite =
    describe "Tree"
        [ test "tree definition" <|
            \_ ->
                tree
                    |> parse "@x@xx"
                    |> Expect.equal (Ok (Node Leaf (Node Leaf Leaf)))
        ]
