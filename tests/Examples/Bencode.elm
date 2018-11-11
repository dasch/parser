module Examples.Bencode exposing (parse, Value(..))

import Parser exposing (..)
import Parser.Common exposing (..)
import Dict exposing (Dict)


type Value
    = BString String
    | BInt Int
    | BList (List Value)
    | BDict (Dict String Value)


parse : String -> Result String Value
parse input =
    Parser.parse input value


value : Parser Value
value =
    oneOf [ int, list, dict, string ]


int : Parser Value
int =
    into BInt
        |> ignore (char 'i')
        |> grab Parser.Common.int
        |> ignore e


string : Parser Value
string =
    rawString
        |> map BString


rawString : Parser String
rawString =
    Parser.Common.int
        |> ignore (char ':')
        |> andThen (\length -> chomp length)


list : Parser Value
list =
    into BList
        |> ignore (char 'l')
        |> grab (zeroOrMore (lazy (\_ -> value)))
        |> ignore e


dict : Parser Value
dict =
    let
        kvPair =
            into (\k v -> ( k, v ))
                |> grab rawString
                |> grab (lazy (\_ -> value))
    in
        into (BDict << Dict.fromList)
            |> ignore (char 'd')
            |> grab (zeroOrMore kvPair)
            |> ignore e


e : Parser Char
e =
    char 'e'
