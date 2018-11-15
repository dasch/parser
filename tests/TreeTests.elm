module TreeTests exposing (suite)

import Examples.Tree exposing (..)
import Expect exposing (Expectation)
import Parser exposing (parse)
import Test exposing (..)


suite : Test
suite =
    describe "Tree"
        [ test "tree definition" <|
            \_ ->
                tree
                    |> parse "@x@xx"
                    |> Expect.equal (Ok (Node Leaf (Node Leaf Leaf)))
        ]
