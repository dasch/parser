module Examples.Toml exposing (Value(..), parse)

import Parser exposing (..)


type Value
    = TomlString String
    | TomlInt Int
    | TomlKv String Value


parse : String -> Result String Value
parse input =
    Parser.parse input value


value : Parser Value
value =
    oneOf [ int, string, keyValue ]


int : Parser Value
int =
    map TomlInt Parser.int


string : Parser Value
string =
    between doubleQuote doubleQuote anyChar
        |> map (TomlString << String.fromList)


doubleQuote : Parser Char
doubleQuote =
    char '"'


keyValue : Parser Value
keyValue =
    succeed TomlKv
        |> ignoring spaces
        |> followedBy key
        |> ignoring spaces
        |> ignoring (char '=')
        |> ignoring spaces
        |> followedBy (lazy (\_ -> value))


key : Parser String
key =
    succeed String.fromList
        |> followedBy (oneOrMore (when Char.isAlphaNum))


spaces : Parser String
spaces =
    zeroOrMore space
        |> map String.fromList


space : Parser Char
space =
    [ ' ', '\t' ]
        |> List.map char
        |> oneOf
