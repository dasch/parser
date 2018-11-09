module Examples.Toml exposing (Value(..), parse, parseValue)

import Parser exposing (..)
import Parser.Common exposing (..)
import Dict exposing (Dict)


type Value
    = TomlString String
    | TomlInt Int
    | TomlBool Bool
    | TomlArray (List Value)
    | TomlTable (Dict String Value)


parse : String -> Result String Value
parse input =
    Parser.parse input document


parseValue : String -> Result String Value
parseValue input =
    Parser.parse input value


document : Parser Value
document =
    succeed (\x y -> (TomlTable (Dict.fromList (List.append x y))))
        |> ignoring blankLines
        |> grabbing (zeroOrMore keyValue)
        |> ignoring blankLines
        |> grabbing (zeroOrMore table)


value : Parser Value
value =
    oneOf [ bool, int, string, array ]


paddedValue : Parser Value
paddedValue =
    succeed identity
        |> ignoring spaces
        |> grabbing (lazy (\_ -> value))
        |> ignoring spaces


bool : Parser Value
bool =
    let
        true =
            Parser.string "true"
                |> map (\_ -> True)

        false =
            Parser.string "false"
                |> map (\_ -> False)
    in
        oneOf [ true, false ]
            |> map TomlBool


int : Parser Value
int =
    map TomlInt Parser.Common.int


string : Parser Value
string =
    between doubleQuote doubleQuote anyChar
        |> map (TomlString << String.fromList)


doubleQuote : Parser Char
doubleQuote =
    char '"'


array : Parser Value
array =
    let
        elements =
            zeroOrMore int
    in
        succeed TomlArray
            |> ignoring (char '[')
            |> grabbing (separatedBy (char ',') paddedValue)
            |> ignoring (char ']')


table : Parser ( String, Value )
table =
    let
        heading : Parser String
        heading =
            succeed identity
                |> ignoring spaces
                |> ignoring (char '[')
                |> grabbing key
                |> ignoring (char ']')
                |> ignoring (maybe newline)
    in
        succeed tuple
            |> ignoring blankLines
            |> grabbing heading
            |> ignoring blankLines
            |> grabbing keyValues


keyValues : Parser Value
keyValues =
    succeed (TomlTable << Dict.fromList)
        |> grabbing (zeroOrMore keyValue)


keyValue : Parser ( String, Value )
keyValue =
    succeed tuple
        |> ignoring spaces
        |> grabbing key
        |> ignoring spaces
        |> ignoring (char '=')
        |> ignoring spaces
        |> grabbing (lazy (\_ -> value))
        |> ignoring spaces
        |> ignoring (maybe newline)


key : Parser String
key =
    oneOrMore (when Char.isAlphaNum)
        |> map String.fromList


spaces : Parser String
spaces =
    zeroOrMore space
        |> map String.fromList


space : Parser Char
space =
    oneOf (List.map char [ ' ', '\t' ])


newline : Parser Char
newline =
    char '\n'


blankLines : Parser ()
blankLines =
    zeroOrMore blankLine
        |> map (\_ -> ())


blankLine : Parser ()
blankLine =
    succeed ()
        |> ignoring spaces
        |> ignoring newline


tuple : a -> b -> ( a, b )
tuple a b =
    ( a, b )
