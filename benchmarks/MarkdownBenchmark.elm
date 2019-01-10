module Main exposing (suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Examples.Markdown as Markdown


input : String
input =
    """
# This is an example of Markdown

1. One
2. Two
3. Three

You can have *different* kinds of __emphasis__!

* And
* Unordered
* Lists!
"""


suite : Benchmark
suite =
    describe "Markdown"
        [ benchmark "parse" <|
            \_ -> Markdown.parse input
        ]


main : BenchmarkProgram
main =
    program suite
