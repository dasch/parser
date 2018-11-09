module Examples.Bencode exposing (parse, Value(..))

import Parser exposing (..)
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
    succeed BInt
        |> ignoring (char 'i')
        |> grabbing Parser.int
        |> ignoring e


string : Parser Value
string =
    rawString
        |> map BString


rawString : Parser String
rawString =
    Parser.int
        |> ignoring (char ':')
        |> andThen (\length -> chomp length)


list : Parser Value
list =
    succeed BList
        |> ignoring (char 'l')
        |> grabbing (zeroOrMore (lazy (\_ -> value)))
        |> ignoring e


dict : Parser Value
dict =
    let
        kvPair =
            succeed (\k v -> ( k, v ))
                |> grabbing rawString
                |> grabbing (lazy (\_ -> value))
    in
        succeed (BDict << Dict.fromList)
            |> ignoring (char 'd')
            |> grabbing (zeroOrMore kvPair)
            |> ignoring e


e : Parser Char
e =
    char 'e'
