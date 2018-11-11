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
        |> ignore blankLines
        |> grabbing (zeroOrMore keyValue)
        |> ignore blankLines
        |> grabbing (zeroOrMore table)


value : Parser Value
value =
    oneOf [ bool, int, string, array ]


paddedValue : Parser Value
paddedValue =
    succeed identity
        |> ignore blanks
        |> grabbing (lazy (\_ -> value))
        |> ignore blanks


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
            |> ignore (char '[')
            |> grabbing (separatedBy (char ',') paddedValue)
            |> ignore (char ']')


table : Parser ( String, Value )
table =
    let
        heading : Parser String
        heading =
            succeed identity
                |> ignore blanks
                |> ignore (char '[')
                |> grabbing key
                |> ignore (char ']')
                |> ignore (maybe newline)
    in
        succeed tuple
            |> ignore blankLines
            |> grabbing heading
            |> ignore blankLines
            |> grabbing keyValues


keyValues : Parser Value
keyValues =
    succeed (TomlTable << Dict.fromList)
        |> grabbing (zeroOrMore keyValue)


keyValue : Parser ( String, Value )
keyValue =
    succeed tuple
        |> ignore blanks
        |> grabbing key
        |> ignore blanks
        |> ignore (char '=')
        |> ignore blanks
        |> grabbing (lazy (\_ -> value))
        |> ignore blanks
        |> ignore (maybe newline)


key : Parser String
key =
    oneOrMore (when Char.isAlphaNum)
        |> map String.fromList


blankLines : Parser ()
blankLines =
    zeroOrMore blankLine
        |> map (\_ -> ())


blankLine : Parser ()
blankLine =
    succeed ()
        |> ignore blanks
        |> ignore newline


tuple : a -> b -> ( a, b )
tuple a b =
    ( a, b )
