module Examples.Toml exposing (Value(..), parse)

import Parser exposing (..)


type Value
    = TomlString String
    | TomlInt Int


parse : String -> Result String Value
parse input =
    Parser.parse input value


value : Parser Value
value =
    oneOf [ int ]


int : Parser Value
int =
    map TomlInt Parser.int
