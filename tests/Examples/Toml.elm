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
    oneOf [ int, string ]


int : Parser Value
int =
    map TomlInt Parser.int


string : Parser Value
string =
    succeed (TomlString << String.fromList)
        |> ignoring doubleQuote
        |> followedBy (until doubleQuote anyChar)
        |> ignoring doubleQuote


doubleQuote : Parser Char
doubleQuote =
    char '"'
