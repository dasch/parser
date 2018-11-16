module Examples.Toml exposing (Value(..), parse, parseValue)

import Dict exposing (Dict)
import Iso8601
import Parser exposing (..)
import Parser.Common exposing (..)
import Parser.MaybeCommon exposing (..)
import Time


type Value
    = TomlString String
    | TomlInt Int
    | TomlBool Bool
    | TomlDatetime Time.Posix
    | TomlArray (List Value)
    | TomlTable (Dict String Value)


parse : String -> Result String Value
parse input =
    Parser.parse input document
        |> Result.mapError (formatError input)


parseValue : String -> Result String Value
parseValue input =
    Parser.parse input value
        |> Result.mapError .message


document : Parser Value
document =
    into (\x y -> TomlTable (Dict.fromList (List.append x y)))
        |> ignore blankLines
        |> grab (zeroOrMore keyValue)
        |> ignore blankLines
        |> grab (zeroOrMore table)
        |> ignore blankLines
        |> ignore blanks
        |> ignore end


value : Parser Value
value =
    oneOf [ bool, datetime, int, multilineString, string, array ]


paddedValue : Parser Value
paddedValue =
    into identity
        |> ignore blanks
        |> grab (lazy (\_ -> value))
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


multilineString : Parser Value
multilineString =
    let
        open =
            repeat 3 doubleQuote
                |> followedBy (maybe newline)

        close =
            repeat 3 doubleQuote
    in
    between open close anyChar
        |> map (TomlString << String.fromList)


doubleQuote : Parser Char
doubleQuote =
    char '"'


datetime : Parser Value
datetime =
    let
        parseTime input =
            Iso8601.toTime input
                |> Result.mapError (always "invalid datetime")
    in
    iso8601
        |> map parseTime
        |> fromResult
        |> map TomlDatetime


array : Parser Value
array =
    let
        elements =
            zeroOrMore int
    in
    into TomlArray
        |> ignore (char '[')
        |> grab (separatedBy (char ',') paddedValue)
        |> ignore (char ']')


table : Parser ( String, Value )
table =
    let
        heading : Parser String
        heading =
            into identity
                |> ignore blanks
                |> ignore (char '[')
                |> grab key
                |> ignore (char ']')
                |> ignore (maybe newline)
    in
    into Tuple.pair
        |> ignore blankLines
        |> grab heading
        |> ignore blankLines
        |> grab keyValues


keyValues : Parser Value
keyValues =
    into (TomlTable << Dict.fromList)
        |> grab (zeroOrMore keyValue)


keyValue : Parser ( String, Value )
keyValue =
    into Tuple.pair
        |> ignore blanks
        |> grab key
        |> ignore blanks
        |> ignore (char '=')
        |> commit
        |> ignore blanks
        |> grab (lazy (\_ -> value))
        |> ignore blanks
        |> ignore (maybe newline)


key : Parser String
key =
    oneOrMore (when Char.isAlphaNum)
        |> map String.fromList
