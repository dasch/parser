module Examples.Bencode exposing (parse, Value(..))

import Parser exposing (Parser, succeed, char, followedBy, ignoring, oneOf)
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
    oneOf [ int ]


int : Parser Value
int =
    succeed BInt
        |> ignoring (char 'i')
        |> followedBy Parser.int
        |> ignoring e


e : Parser Char
e =
    char 'e'
