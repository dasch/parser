module Main exposing (suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Parser exposing (..)


suite : Benchmark
suite =
    describe "Parser" <|
        [ benchmark "char" <|
            let
                input =
                    "xxx"

                parser =
                    repeat 3 (char 'x')
            in
            \_ -> parse input parser
        ]


main : BenchmarkProgram
main =
    program suite
