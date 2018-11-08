module Examples.Toml exposing (Value(..), parse)

import Parser exposing (..)


type Value
    = TomlString String
    | TomlInt Int
    | TomlKv String Value
    | TomlTable (List Value)


parse : String -> Result String Value
parse input =
    Parser.parse input document


document : Parser Value
document =
    succeed identity
        |> ignoring (zeroOrMore blankLine)
        |> followedBy value


value : Parser Value
value =
    oneOf [ int, string, table, keyValue ]


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


table : Parser Value
table =
    succeed (\name values -> TomlKv name (TomlTable values))
        |> ignoring spaces
        |> ignoring (char '[')
        |> followedBy key
        |> ignoring (char ']')
        |> ignoring (maybe newline)
        |> followedBy (zeroOrMore keyValue)


keyValue : Parser Value
keyValue =
    succeed TomlKv
        |> ignoring spaces
        |> followedBy key
        |> ignoring spaces
        |> ignoring (char '=')
        |> ignoring spaces
        |> followedBy (lazy (\_ -> value))
        |> ignoring spaces
        |> ignoring (maybe newline)


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


newline : Parser Char
newline =
    char '\n'


blankLine : Parser ()
blankLine =
    succeed ()
        |> ignoring spaces
        |> ignoring newline
